#include "ncphead.h"
#include "servkel.h"
#include <algorithm>
#include <vector>

using namespace std;

class op_serv_addr
{
	CSessionAddr m_addr;
public:
	op_serv_addr( const CSessionAddr& addr ) : m_addr(addr){}
	bool operator()( CServerSession* pServSession ){
		return pServSession->GetAddr()==m_addr;
	}
};

bool CServerProxy::SendMessage( CMessageHead* pMsg )
{
	CSockLocker<CServerProxy> locker(this);
	if( m_sock==NULL )return false;
	try{
		pMsg->Send( m_sock );
	}catch( CSockException* e ){
		e->Delete();
		closesocket( m_sock );
		m_sock = 0;
		return false;
	}
	return true;
}

CServerSession* CServerProxy::GetServerSession( const CSessionAddr& addr )
{
	CObjLocker< CServerProxy > locker(this);
	map<CSessionAddr, CServerSession*>::iterator iter = m_ServSessions.find( addr );
	if( iter==m_ServSessions.end() )return NULL;

	return iter->second;
}

void CServerProxy::InsertServerSession( CServerSession* pServSession )
{
	CObjLocker< CServerProxy > locker(this);
	m_ServSessions[ pServSession->GetAddr() ] = pServSession;
}

void CServerProxy::RemoveServerSession( const CSessionAddr& addr )
{
	CObjLocker< CServerProxy > locker(this);
	map<CSessionAddr, CServerSession*>::iterator iter = m_ServSessions.find( addr );
	if( iter!=m_ServSessions.end() )m_ServSessions.erase(iter);
}

bool CServerProxy::MessageFromClient()
{
	CMessageHead* pMsg=NULL;
	pMsg = RecvMessage( m_sock );
	if( pMsg==NULL ){	//the sock is shutdown
		{
			CSockLocker<CServerProxy> locker(this);
			//close the socket
			if( m_sock!=0 ){
				closesocket( m_sock );
				m_sock = 0;
			}
		}
		//get a snapshot of the current activate sessions and release the locker.
		vector<CServerSession*> tmpSessions;
		{
			CObjLocker<CServerProxy> locker(this);
			//notify the sessions to quit
			tmpSessions.reserve( m_ServSessions.size() );

			map<CSessionAddr, CServerSession*>::iterator iter;
			for( iter = m_ServSessions.begin(); iter!=m_ServSessions.end(); iter++ ){
				tmpSessions.push_back( iter->second );
			}
		}

		//wait for the sessions to quit
		CMessageHead msg(NCM_PROXYSHUTDOWN);
		for( int i=0; i<tmpSessions.size(); i++ ){
			if( !tmpSessions[i]->IsClosed() ){
				tmpSessions[i]->PostMessage( msg.Clone() );
//				tmpSessions[i]->WaitForObject();
			}
		}

		//close and quite the routing loop
		m_pServApp->DestroyServerProxy( this );
		return false;
	}

	DispatchMessage( pMsg );
	return true;
}

bool CServerProxy::MessageFromServer( SOCKET sock )
{
	CMessageHead* pMsg = RecvMessage( sock );
	if( pMsg==NULL ){	//the sock is shutdown

		//get a snapshot of the associated activate sessions and release the locker.
		vector<CServerSession*> tmpSessions;
		{
			CObjLocker<CServerProxy> locker(this);
			//notify the sessions to quit
			tmpSessions.reserve( m_ServSessions.size() );

			map<CSessionAddr, CServerSession*>::iterator iter;
			for( iter = m_ServSessions.begin(); iter!=m_ServSessions.end(); iter++ ){
				CServerSessionStub* pServStub = (CServerSessionStub*)iter->second;
				if( *(pServStub->GetProxySocket())==sock )tmpSessions.push_back( iter->second );
			}
		}
		for( int i=0; i<tmpSessions.size(); i++ ){
			if( !tmpSessions[i]->IsClosed() )tmpSessions[i]->Close();
		}
		return true;
	}else{
		CServerSessionStub* pServStub = (CServerSessionStub*)GetServerSession( pMsg->GetDstAddr() );
		if( pServStub!=NULL ){
			pServStub->PostReturnMessage( pMsg );
			//pServSession->PostMessage( pMsg );
			return true;
		}
	}
	return false;
}

void CServerProxy::PumpWaitingMessages( SOCKET sock, bool bOk )
{
	//loop for each waited messages
	list<CMessageHead*>::iterator iter;
	for( iter=m_WaitingMessages.begin(); iter!=m_WaitingMessages.end(); ){
		CMessageHead* pMsg = *iter;
		//find the stub for the message
		CServerSession* pServSession = GetServerSession( pMsg->GetDstAddr() );
		if( pServSession==NULL ){
			m_WaitingMessages.erase( iter++ );
			pMsg->Destroy();
			continue;
		}

		//find the stub, check if it is waiting for the socket.
		CServerSessionStub* pServStub = (CServerSessionStub*)pServSession;
		if( *(pServStub->GetProxySocket())==sock ){				//this stub is waiting for the sock.
			m_WaitingMessages.erase( iter++ );
			if( bOk ){
				if( pMsg->GetMessage()==NCM_OPENSESSION )pServStub->Open2();
				ASSERT( pServStub->IsEstablished() );
				pServStub->PostMessage( pMsg );
			}else{
				if( pMsg->GetMessage()==NCM_OPENSESSION )pServStub->Close();
				CMessageHead msg;
				msg.Init( NCF_ACK|NCM_OPENSESSION, GetAddr(), pMsg->GetSrcAddr(), E_NOHOST );
				SendMessage( &msg );
				pMsg->Destroy();
			}
			continue;
		}
		++iter;
	}
}

void CServerProxy::RouteMessage()
{
	//prepare to accept the socket connection
	int maxfds;
	fd_set rfds;
	fd_set wfds;
	fd_set efds;

	//prepare to accept the socket connection
	while( true ){
		maxfds = 0;
		FD_ZERO( &rfds );
		FD_ZERO( &wfds );
		FD_ZERO( &efds );
		FD_SET( m_sock, &rfds );

		map<IN_ADDR, CProxySocket*>::iterator pos;
		for( pos = m_PeerSocks.begin(); pos!=m_PeerSocks.end(); ++pos ){
			SOCKET s = *(pos->second);
			if( pos->second->IsOpened() ){
				FD_SET( s, &rfds );
			}else{
				FD_SET( s, &wfds );
				FD_SET( s, &efds );
			}
			maxfds = max( s, maxfds );
		}
		maxfds = max( m_sock, maxfds )+1;

		Select( maxfds, &rfds, &wfds, &efds, NULL );
		if( FD_ISSET(m_sock, &rfds) ){			//get message from client socket.
			if( !MessageFromClient() )break;
		}

		//peer socket is ready for connecton or exception happened.
		for( pos = m_PeerSocks.begin(); pos!=m_PeerSocks.end(); ){ //note the MessageFromServer may remove a node from m_PeerSocks
			SOCKET s = *(pos->second);
			++pos;
			if( FD_ISSET(s, &rfds) ){			//get message from peer socket.
				MessageFromServer( s );
			}

			if( FD_ISSET(s, &wfds) ){			//connection is ready now.
				//set socket status to opened.
				SOCKADDR_IN addr;
				socklen_t len = sizeof(SOCKADDR_IN);
				GetPeerName( s, (SOCKADDR*)&addr, &len );
				CProxySocket* pProxySock = GetPeerSocket( addr.sin_addr );
				pProxySock->SetState( PS_OPENED );
				ASSERT( s==(*pProxySock) );

				//set the connection back to block mode.
				SetSockNonBlock( s, false );
//				u_long nonblock = 0;
//				ioctlsocket( s, FIONBIO, &nonblock );
				PumpWaitingMessages( s, true );
			}

			if( FD_ISSET(s, &efds) ){			//connection is failed.
				PumpWaitingMessages( s, false );
			}
		}
	}//end loop while(ture)
}

void CServerProxy::DispatchMessage( CMessageHead* pMsg )
{
	if( pMsg->GetDstAddr()==GetAddr() ){
		//this is the message for the proxy
		ProcessMessage( pMsg );
		pMsg->Destroy();
		return;
	}else{
		CServerSession* pServSession = GetServerSession( pMsg->GetDstAddr() );
		if( pServSession==NULL ){
			pMsg->Destroy();
			return;
		}

		pServSession->PostMessage( pMsg );
		return;
	}
}

void CServerProxy::ProcessMessage( CMessageHead* pMsg )
{
	switch( pMsg->GetCommand() )
	{
	case NCM_OPENSESSION:
		CMessage1Param<CSessionAddr>* pOpenMsg = (CMessage1Param<CSessionAddr>*)pMsg;
		pOpenMsg->ntoh();
		OpenServerSession( pMsg->GetSrcAddr(), pOpenMsg->GetParam() );
		break;
	};

	return;
}

void CServerProxy::OpenServerSession( const CSessionAddr& addrClnt, const CSessionAddr& addrServ )
{
	//retrieve the session tocken from the CServerApp.
	CServerSession* pServSession = m_pServApp->RetrieveServerSession( addrServ );
	if( pServSession!=NULL ){
		InsertServerSession( pServSession );
		pServSession->Open( this, addrClnt );
	}

	//Ack the command
	if( pServSession==NULL ){
		CMessageHead msg;
		msg.Init( NCF_ACK|NCM_OPENSESSION, GetAddr(), addrClnt, E_NOHOST );
		SendMessage( &msg );
	}else{
		//post the OPENSESSION message to the session, it will acknowledge it
		CMessage1Param<CSessionAddr> msg;
		msg.Init( NCM_OPENSESSION, pServSession->GetClntAddr(), pServSession->GetAddr(), addrServ );
//		CMessageHead msg;
//		msg.Init( NCM_OPENSESSION, pServSession->GetClntAddr(), pServSession->GetAddr() );
		if( pServSession->IsEstablished() ){	//post the message for established session
			pServSession->PostMessage( msg.GetMessageHead()->Clone() );
		}else{									//put the message into the waiting list, if session is not ready
			m_WaitingMessages.push_back( msg.GetMessageHead()->Clone() );
		}
	}
}

void CServerProxy::CloseServerSession( CServerSession* pServSession )
{
	RemoveServerSession( pServSession->GetAddr() );
	m_pServApp->ReturnServerSession( pServSession );
}


int CServerProxy::ServerProxyThread( void* arg )
{
	CServerProxy* pServProxy = (CServerProxy*)arg;
	pServProxy->RouteMessage();
	return 0;
}

bool CServerProxy::StartServerProxy()
{
	ASSERT( m_hProxyThread==NULL );
	m_hProxyThread = CreateThread( (THREAD_ROUTINE)ServerProxyThread, this, 0 );
	if( m_hProxyThread==NULL )return false;
	return true;
}

void CServerProxy::WaitForObject()
{
	if( m_hProxyThread==NULL )return;

	WaitForThread( m_hProxyThread, NULL );
	CloseHandle( m_hProxyThread );
	m_hProxyThread = NULL;
}

/**************************************************************************************************************
			CServerSessionStub
***************************************************************************************************************/
/*void CServerSessionStub::ProcessMessage( CMessageHead* pMsg )
{
	//save the m_pServProxy, note Close() may set m_pServProxy to NULL;
	CServerProxy* pServProxy = m_pServProxy;

	switch( pMsg->GetMessage() ){
	case NCM_PROXYSHUTDOWN:
		Close();
		return;
	}

	if( pMsg->GetSrcAddr()==GetClntAddr() ){	//this is a message from client to server. route it to proxy socket.
		pMsg->SetSrcAddr( GetAddr() );
		pMsg->SetDstAddr( GetPeerAddr() );
		SendPeerMessage( pMsg );
		return;
	}

	//this is from server to client. route it to ServerProxy
	ASSERT( pMsg->GetSrcAddr()==GetPeerAddr() );
	pMsg->SetSrcAddr( GetAddr() );
	pMsg->SetDstAddr( GetClntAddr() );
	if( pMsg->IsAcking(NCM_OPENSESSION) ){
		if( pMsg->IsSucceeded() ){		//open session successeeded
			CMessage1Param<CSessionAddr>* pAddrMsg = (CMessage1Param<CSessionAddr>*)pMsg;
			pAddrMsg->ntoh();
			SetPeerAddr( pAddrMsg->GetParam() );
			pAddrMsg->SetParam( GetAddr() );
			pAddrMsg->hton();
		}else{							//open session failed
			Close();
		}
	}else if( pMsg->IsAcking(NCM_CLOSESESSION) ){
		Close();
	}
	pServProxy->SendMessage( pMsg );
}*/

//reflect the messages returned from server to the client.
void CServerSessionStub::PostReturnMessage( CMessageHead* pMsg )
{
	CMessageTrash trash( pMsg );	//will destroy the message

	//save the m_pServProxy, note Close() may set m_pServProxy to NULL;
	CServerProxy* pServProxy = m_pServProxy;

	//this is from server to client. route it to ServerProxy
	ASSERT( pMsg->GetSrcAddr()==GetPeerAddr() );
	pMsg->SetSrcAddr( GetAddr() );
	pMsg->SetDstAddr( GetClntAddr() );
	if( pMsg->IsAcking(NCM_OPENSESSION) ){
		if( pMsg->IsSucceeded() ){		//open session successeeded
			CMessage1Param<CSessionAddr>* pAddrMsg = (CMessage1Param<CSessionAddr>*)pMsg;
			pAddrMsg->ntoh();
			SetPeerAddr( pAddrMsg->GetParam() );
			pAddrMsg->SetParam( GetAddr() );
			pAddrMsg->hton();
		}else{							//open session failed
			Close();
		}
	}else if( pMsg->IsAcking(NCM_CLOSESESSION) ){
		Close();
	}
	pServProxy->SendMessage( pMsg );
}

//virtual functions
//note PostMessage declare the ownership of pMsg. So pMsg must be dynamically allocated.
void CServerSessionStub::PostMessage( CMessageHead* pMsg )
{
	CMessageTrash trash( pMsg );	//will destroy the message

	switch( pMsg->GetMessage() ){
	case NCM_PROXYSHUTDOWN:
		Close();
		return;
	case NCM_OPENSESSION:
		CMessage1Param<CSessionAddr>* pOpenMsg = (CMessage1Param<CSessionAddr>*)pMsg;
		pOpenMsg->SetParam( m_pProxyToken->GetPeerAddr() );
		pOpenMsg->hton();
		break;
	}

	//this is a message from client to server. route it to proxy socket.
	ASSERT( pMsg->GetSrcAddr()==GetClntAddr() );
	pMsg->SetSrcAddr( GetAddr() );
	pMsg->SetDstAddr( GetPeerAddr() );
	SendPeerMessage( pMsg );
	return;
}

/*//virtual functions
//note PostMessage declare the ownership of pMsg. So pMsg must be dynamically allocated.
void CServerSessionStub::PostMessage( CMessageHead* pMsg )
{
	//save the m_pServProxy, note Close() may set m_pServProxy to NULL;
	CServerProxy* pServProxy = m_pServProxy;

	if( pMsg->GetMessage()==NCM_PROXYSHUTDOWN ){
		Close();
		pMsg->Destroy();
		return;
	}

	if( pMsg->GetSrcAddr()==GetClntAddr() ){	//this is a message from client to server. route it to proxy socket.
		pMsg->SetSrcAddr( GetAddr() );
		pMsg->SetDstAddr( GetPeerAddr() );
		SendPeerMessage( pMsg );
	}else{										//this is from server to client. route it to ServerProxy
		ASSERT( pMsg->GetSrcAddr()==GetPeerAddr() );
		pMsg->SetSrcAddr( GetAddr() );
		pMsg->SetDstAddr( GetClntAddr() );

		if( pMsg->IsAcking(NCM_OPENSESSION) ){		//this is the ack message for open session from peer
			if( pMsg->IsSucceeded() ){		//open session successeeded
				CMessage1Param<CSessionAddr>* pAddrMsg = (CMessage1Param<CSessionAddr>*)pMsg;
				pAddrMsg->ntoh();
				SetPeerAddr( pAddrMsg->GetParam() );
				pAddrMsg->SetParam( GetAddr() );
				pAddrMsg->hton();
			}else{							//open session failed
				Close();
			}
		}
		if( pMsg->IsAcking(NCM_CLOSESESSION) ){
			Close();
		}
		pServProxy->SendMessage( pMsg );
	}
	pMsg->Destroy();
}*/


//open and close.
bool CServerSessionStub::Open( CServerProxy* pServProxy, const CSessionAddr& addrClnt )
{
	ASSERT( IsClosed() );
	m_pServProxy = pServProxy;
	m_addrClnt = addrClnt;

	CProxySocket* pPeerSock = m_pServProxy->GetPeerSocket( m_addrPeer.m_addrHost );
	if( pPeerSock==NULL ){				//the peer sock is not created, create the sock and try to connect the server
		pPeerSock = new CProxySocket();
		m_pProxySock = pPeerSock;
		m_pProxySock->AddRef();

		m_pServProxy->InsertPeerSocket( m_addrPeer.m_addrHost, pPeerSock );

		//connect the remote peer server.
		SOCKADDR_IN srvaddr;
		srvaddr.sin_family = AF_INET;
		srvaddr.sin_port = htons( NCP_SERVER_PORT );
		bcopy( &m_addrPeer.m_addrHost, &srvaddr.sin_addr, sizeof(IN_ADDR) );

		//set the connection to non-block mode.
		SetSockNonBlock( *pPeerSock, true );
//		u_long nonblock = 1;
//		ioctlsocket( (*pPeerSock), FIONBIO, &nonblock );
		int nRet = connect( (*pPeerSock), (SOCKADDR*)&srvaddr, sizeof(SOCKADDR_IN) );
		m_nState = SS_OPEN_WAIT;
		return true;
	}else{								//the peer sock is already there, check the state
		m_pProxySock = pPeerSock;
		m_pProxySock->AddRef();
		if( pPeerSock->IsOpened() ){
			m_nState = SS_ESTABLISHED;
		}else{
			m_nState = SS_OPEN_WAIT;
		}
		return true;
	}
}

void CServerSessionStub::Open2()
{
	ASSERT( IsOpenWait() );
	CProxySocket* pPeerSock = m_pServProxy->GetPeerSocket( m_addrPeer.m_addrHost );
	ASSERT( pPeerSock!=NULL );

	if( pPeerSock->IsOpened() ){	//the sock is really connected, set the state to established
		m_nState = SS_ESTABLISHED;
	}
}


bool CServerSessionStub::Close()
{
	ASSERT( m_pServProxy!=NULL );
	CProxySocket* pPeerSock = m_pServProxy->GetPeerSocket( m_addrPeer.m_addrHost );
	ASSERT( pPeerSock!=NULL );
	ASSERT( pPeerSock==m_pProxySock );

	//close the peer socket if this is the last Stub using the socket.
	if( pPeerSock->Release()==0 ){
		m_pServProxy->RemovePeerSocket( m_addrPeer.m_addrHost );
	}

	m_addrPeer.m_nServId = 0;
	m_pProxySock = NULL;
	m_nState = SS_CLOSED;
	m_pServProxy->CloseServerSession( this );
	return true;
}


/**************************************************************************************************************
			CServerSession
***************************************************************************************************************/

void CServerSessionLocal::PostQuitMessage()
{
	CMessageHead* pMsg = new CMessageHead( NCM_QUIT );
	PostMessage( pMsg );
}

void CServerSessionLocal::PostMessage( CMessageHead* pMsg )
{
	//closesession and proxyshutdown message will destroy the session object
	//save the state into local stack variables.
	HANDLE hSessionThread = m_hSessionThread;
	BOOL bSync = false;
	if( pMsg->GetMessage()==NCM_CLOSESESSION ||
		pMsg->GetMessage()==NCM_PROXYSHUTDOWN ){
			bSync = true;
		}

	m_MsgQueue.PutMessage( pMsg );

	//wait for the session object being safely destroyed.
	if( bSync ){
		WaitForThread( hSessionThread );
		CloseHandle( hSessionThread );
	}
}

void CServerSessionLocal::MessageLoop()
{
//	try{
		CMessageHead* pMsg = NULL;
		while( (pMsg=GetMessage())!=NULL ){
			ProcessMessage( pMsg );
			pMsg->Destroy();
		}
//	}catch(...){
//	}
}

CMessageHead* CServerSessionLocal::GetMessage()
{
	CMessageHead* pMsg = m_MsgQueue.GetMessage();
	if( pMsg->IsQuitting() ){
		CMessageTrash trash(pMsg);
		m_nState = SS_CLOSED;
		m_pServProxy->CloseServerSession( this );
		return NULL;
	}
	return pMsg;
}

CMessageHead* CServerSessionLocal::GetMessage( int nCmd )
{
	CMessageHead* pMsg = m_MsgQueue.GetMessage();
	if( pMsg->GetCommand()!=nCmd || pMsg->IsQuitting() ){
		//this function put the message back.
		PostMessage( pMsg );
		return NULL;
	}
	return pMsg;
}

void CServerSessionLocal::AckOpen()
{
	CMessage1Param<CSessionAddr> msg;
	msg.Init( NCF_ACK|NCM_OPENSESSION, m_pServProxy->GetAddr(), GetClntAddr(), GetAddr() );
	msg.hton();
	m_pServProxy->SendMessage( msg.GetMessageHead() );
}

void CServerSessionLocal::AckClose()
{
	CMessageHead msg;
	msg.Init( NCF_ACK|NCM_CLOSESESSION, GetAddr(), GetClntAddr() );
	m_pServProxy->SendMessage( &msg );
	m_nState = SS_CLOSED;
}

void CServerSessionLocal::ProcessMessage( CMessageHead* pMsg )
{
	switch( pMsg->GetCommand() ){
	case NCM_OPENSESSION:
		AckOpen();
		break;
	case NCM_CLOSESESSION:
		AckClose();
		Close();
		break;
	case NCM_PROXYSHUTDOWN:
		Close();
		break;
	default:
		DefaultMessage( pMsg );
		break;
	}
}

void CServerSessionLocal::DefaultMessage( CMessageHead* pMsg )
{
	CMessageHead msg;
	msg.Init( pMsg->GetCommand()|NCF_ACK, GetAddr(), GetClntAddr(), E_IMPLEMENT );
	m_pServProxy->SendMessage( &msg );
}

int CServerSessionLocal::ServerSessionThread( void* arg )
{
	CServerSessionLocal* pServSession = (CServerSessionLocal*)arg;
	pServSession->MessageLoop();
	return 0;
}

/*void CServerSessionLocal::WaitForObject()
{
	if( m_hSessionThread==NULL )return;

	WaitForThread( m_hSessionThread, NULL );
	CloseHandle( m_hSessionThread );
	m_hSessionThread = NULL;
}*/

bool CServerSessionLocal::StartServerSession()
{
	ASSERT( m_hSessionThread==NULL );
	m_hSessionThread = CreateThread( (THREAD_ROUTINE)ServerSessionThread, this, 0 );
	if( m_hSessionThread==NULL )return false;

	return true;
}

bool CServerSessionLocal::Open( CServerProxy* pServProxy, const CSessionAddr& addrClnt )
{
//	WaitForObject();
	ASSERT( IsClosed() );
	m_pServProxy = pServProxy;
	m_addrClnt = addrClnt;
	m_nState = SS_ESTABLISHED;
	return StartServerSession();
}

bool CServerSessionLocal::Close()
{
	PostQuitMessage();
	return true;
}

void CServerProxy::Shutdown()
{
	CSockLocker<CServerProxy> locker(this);
	if( m_sock!=0 ){
		closesocket( m_sock );
		m_sock = 0;
	}
}

CServerProxy* CServerApp::CreateServerProxy( SOCKET sock )
{
	CSessionAddr addrProxy( GetLocalHostAddr(), 0 );
	CServerProxy* pServProxy = new CServerProxy( this, sock, addrProxy );

	if( pServProxy ){
		CObjLocker<CServerApp> locker(this);
		m_ServerProxies.push_back( pServProxy );
	}else return NULL;
	return pServProxy;
}		

void CServerApp::DestroyServerProxy( CServerProxy* pServProxy )
{
	{
		CObjLocker<CServerApp> locker(this);

		list<CServerProxy*>::iterator iter = find(m_ServerProxies.begin(), m_ServerProxies.end(), pServProxy );
		if( iter!=m_ServerProxies.end() ){
			m_ServerProxies.erase( iter );
		}
	}
	delete pServProxy;
}

CServerApp::~CServerApp()
{
	while( !m_ServerProxies.empty() ){
		CServerProxy* pServProxy = NULL;
		{
			CObjLocker<CServerApp> locker(this);
			pServProxy = m_ServerProxies.front();
		}
		if( pServProxy!=NULL ){
			pServProxy->Shutdown();
			pServProxy->WaitForObject();
		}
	}
	{
		CObjLocker<CServerApp> locker(this);
		ASSERT( m_ServTokens.size()==m_PeerTokens.size() );
		map<CSessionAddr, CProxyToken*>::iterator iter;
		for( iter=m_PeerTokens.begin(); iter!=m_PeerTokens.end(); ++iter ){
			delete iter->second;
		}
		m_ServTokens.clear();
		m_PeerTokens.clear();
	}

/*	while( !m_SessionTockens.empty() ){
		CServerSession* pServSession = m_SessionTockens.front();
		m_SessionTockens.pop_front();
		delete pServSession;
	}*/

    CloseHandle( m_hMutex );
}

CServerSession* CServerApp::RetrieveServerSession( const CSessionAddr& addrServ )
{
	CObjLocker<CServerApp> locker(this);
	//search the server token
	map<CSessionAddr, CProxyToken*>::iterator pos = m_ServTokens.find( addrServ );
	if( pos==m_ServTokens.end() )return NULL;

	CProxyToken* pToken = pos->second;
	CServerSession* pServSession = NULL;
	if( (pToken->GetServAddr()==pToken->GetPeerAddr()) && (pToken->GetServAddr().m_addrHost==GetAddr().m_addrHost) ){
		//this is the local server.
		pServSession = new CServerSessionLocal( pToken );
	}else{
		//this is a proxy stub
		pServSession = new CServerSessionStub( pToken );
	}
	if( pServSession!=NULL ){
		pToken->SetState( CTS_RUNNING );
		m_ServTokens.erase( pos );
	}
//	CServerSession* pServSession = m_SessionTockens.front();
//	m_SessionTockens.pop_front();
	cdump<<lock<<"Session Opened, Id: "<<pServSession->GetAddr().m_nServId<<endl<<unlock;
	return pServSession;
}

void CServerApp::ReturnServerSession( CServerSession* pServSession )
{
	CObjLocker<CServerApp> locker(this);

	cdump<<lock<<"Session Closed, Id: "<<pServSession->GetAddr().m_nServId<<endl<<unlock;

	//return token to the server token map
	CProxyToken* pToken = pServSession->GetProxyToken();
	pToken->SetState( CTS_IDLE );
	m_ServTokens[ pToken->GetServAddr() ] = pToken;

	delete pServSession;
}

int ShutdownThread( void* arg )
{
	CServerApp* pServApp = (CServerApp*)arg;
	MilliSleep( 20*1000 );

	{
		CObjLocker<CServerApp> locker(pServApp );
		list<CServerProxy*>::iterator pos = pServApp->m_ServerProxies.begin();
		while( pos!=pServApp->m_ServerProxies.end() ){
			(*pos)->Shutdown();
			++pos;
		}
	}
	return 0;
}

void CServerApp::RunServer()
{		
	m_addr.m_addrHost = GetLocalHostAddr();
	m_addr.m_nServId = 0;

	if( IsServer() ){
		CPU_INFO cpuinfo;
        GetCpuInfo( &cpuinfo );
		for( int i=0; i<cpuinfo.nCpus; i++ ){
//		for( int i=0; i<2; i++ ){
			CSessionAddr addr( GetAddr().m_addrHost, GetNextSessionId() );
			CProxyToken* pToken = new CProxyToken( addr, addr, cpuinfo.nCpuType, cpuinfo.nCpuMhz );
			m_PeerTokens[ pToken->GetPeerAddr() ] = pToken;
			m_ServTokens[ pToken->GetServAddr() ] = pToken;
		}
		cdump<<cpuinfo.nCpus<<" local servers started at CPU speed "<<cpuinfo.nCpuMhz<<" Mhz"<<endl;
	}

	if( IsProxy() && !m_strHostFile.empty() ){
		ifstream ifg(m_strHostFile.c_str());
		while( !ifg.fail() && !ifg.eof() ){
			char buf[MAXLINE];
			ifg.getline( buf, ELEMENTS(buf) );
			if( ifg.eof() || ifg.fail() )break;
			if( trim(buf)=="\0" )continue;
			IN_ADDR addrHost = GetHostInAddr( buf );
			if( m_HostAddrs.find(addrHost)==m_HostAddrs.end() ){
				m_HostAddrs.insert( addrHost );
			}
//			m_SessionTockens.push_back( new CServerSessionStub( CSessionAddr(GetLocalHostAddr(), id++),
//					GetHostInAddr(buf) ) );
		}
	}
	m_hPollThread = CreateThread( (THREAD_ROUTINE)PollThread, this, 0 );

	SOCKET socksrv;
	SOCKADDR_IN srvaddr;

	//setup the tcp server socket and bind.
	socksrv = Socket( PF_INET, SOCK_STREAM, 0 );

	srvaddr.sin_family = AF_INET;
	srvaddr.sin_addr.s_addr = INADDR_ANY;

	srvaddr.sin_port = htons( NCP_SERVER_PORT );
	Bind( socksrv, (SOCKADDR*)&srvaddr, sizeof(SOCKADDR_IN) );
	Listen( socksrv, 5 );

//	HANDLE hShutdownThread = CreateThread( (LPTHREAD_START_ROUTINE)ShutdownThread, this, 0 );

	//prepare to accept the socket connection
	while( true ){
		SOCKET sockclt = Accept( socksrv, (SOCKADDR*)NULL, NULL );
		CServerProxy* pServProxy = CreateServerProxy( sockclt );
		if( pServProxy )pServProxy->StartServerProxy();
	}

	closesocket( socksrv );
}

//poll all the registered hosts
void CServerApp::DoPoll( SOCKET sock ){
	vector<IN_ADDR> arrAddrs;
	{
		CObjLocker<CServerApp> locker(this);
		arrAddrs.resize( m_HostAddrs.size() );
		copy( m_HostAddrs.begin(), m_HostAddrs.end(), arrAddrs.begin() );
	}
	SOCKADDR_IN dstaddr;
	dstaddr.sin_family = AF_INET;
	dstaddr.sin_port = htons( NCP_SERVER_PING_PORT );

	for( int i=0; i<arrAddrs.size(); i++ ){
		CMessageHead msg;
		msg.Init( NCM_POLLSERVER, CSessionAddr(), CSessionAddr(), NCP_SERVER_PING_PORT );
		dstaddr.sin_port = htons( NCP_SERVER_PING_PORT );
		dstaddr.sin_addr = arrAddrs[i];
		cdump<<"polling host:"<<inet_ntoa(arrAddrs[i])<<endl;
		msg.SendTo( sock, dstaddr );
	}
}

void CServerApp::OnAckPoll( CMessageHead* pMsg ){
	CObjLocker<CServerApp> locker(this);
	TOKEN_INFO* pinfo = (TOKEN_INFO*)(pMsg+1);

	IN_ADDR addrHost = pMsg->GetSrcAddr().m_addrHost;
	//update the host set
	if( m_HostAddrs.find( addrHost )==m_HostAddrs.end() )m_HostAddrs.insert( addrHost );

	//mark all the existing idle tokens from the host as dead
	map<CSessionAddr, CProxyToken*>::iterator pos = m_PeerTokens.lower_bound( CSessionAddr(addrHost,0) );
	while( pos!=m_PeerTokens.end() && ( pos->second->GetPeerAddr().m_addrHost==addrHost) ){
		if( pos->second->IsIdle() )pos->second->SetState(CTS_DEAD);
		++pos;
	}

	//reset the tokens to idle
	int nInfos = pMsg->GetResult();
	for( int i=0; i<nInfos; i++ ){
		pinfo->ntoh();
		CSessionAddr addrPeer( pMsg->GetSrcAddr().m_addrHost, pinfo->nServId );
		pos = m_PeerTokens.find( addrPeer );
		if( pos==m_PeerTokens.end() ){
			CSessionAddr addrServ( GetAddr().m_addrHost, GetNextSessionId() );
			CProxyToken* pToken = new CProxyToken( addrServ, addrPeer, pinfo->nCpuType, pinfo->nCpuMhz );
			m_PeerTokens[pToken->GetPeerAddr()] = pToken;
			m_ServTokens[pToken->GetServAddr()] = pToken;
		}else{
			pos->second->SetState( CTS_IDLE );
		}
		pinfo++;
	}

	cdump<<"get poll back from host:"<<inet_ntoa(addrHost)<<" with "<<nInfos<<" tokens"<<endl;

	//remove all the dead tokens
	pos = m_PeerTokens.lower_bound( CSessionAddr(addrHost,0) );
	while( pos!=m_PeerTokens.end() && ( pos->second->GetPeerAddr().m_addrHost==addrHost) ){
		CProxyToken* pToken = pos->second;
		++pos;

		if( pToken->IsDead() ){
			m_PeerTokens.erase( pToken->GetPeerAddr() );
			m_ServTokens.erase( pToken->GetServAddr() );
		}
	}
}

int CServerApp::PollThread( void* arg )
{
	CServerApp* pServApp = (CServerApp*)arg;
	ASSERT( pServApp!=NULL );

	//start the poll server and bind to specific port.
	SOCKET socksrv = Socket( PF_INET, SOCK_DGRAM, 0 );
	SOCKADDR_IN srvaddr;
	srvaddr.sin_family = AF_INET;
	srvaddr.sin_port = htons( NCP_SERVER_PING_PORT );
	srvaddr.sin_addr.s_addr = INADDR_ANY;
	Bind( socksrv, (SOCKADDR*)&srvaddr, sizeof(SOCKADDR_IN) );
	socklen_t len = sizeof(SOCKADDR_IN);
	getsockname( socksrv, (SOCKADDR*)&srvaddr, &len );

	CMessageHead msg;
	if( pServApp->IsProxy() ){
		msg.Init( NCM_POLLSERVER, CSessionAddr(), CSessionAddr(), NCP_SERVER_PING_PORT );
	}else if( pServApp->IsServer() ){
		msg.Init( NCM_SERVERSTART, CSessionAddr(), CSessionAddr() );
//		msg.Init( NCM_POLLSERVER, CSessionAddr(), CSessionAddr() );
	}
	if( pServApp->IsProxy() || pServApp->IsServer() ){
		SOCKET sockbroad = Socket( PF_INET, SOCK_DGRAM, 0 );
		msg.Broadcast( sockbroad, NCP_SERVER_PING_PORT );
		closesocket( sockbroad );
	}

	while( true ){
		fd_set rfds;
		FD_ZERO( &rfds );
		FD_SET( socksrv, &rfds );
		int maxfds = socksrv + 1;

		TIMEVAL tmout, *pto;
		if( pServApp->IsProxy() ){
			tmout.tv_sec = 30;
			tmout.tv_usec = 0;
			pto = &tmout;
		}else pto = NULL;

		int nRet = Select( maxfds, &rfds, NULL, NULL, pto );

		if( nRet==0 ){	//select timeout, should I do a poll for the remote hosts?
			if( pServApp->IsProxy() )pServApp->DoPoll( socksrv );
		}else if( FD_ISSET(socksrv, &rfds) ){
			SOCKADDR_IN srcaddr;
			CMessageHead* pMsg = RecvMsgFrom( socksrv, srcaddr );
			CMessageTrash trash(pMsg);
			cdump<<inet_ntoa(srcaddr.sin_addr)<<" with message id:"<<pMsg->GetMessage()<<endl;
			if( pMsg->GetMessage()==NCM_POLLSERVER ){
				unsigned short sPort = htons( (short)pMsg->GetResult() );
				if( !( (srcaddr.sin_addr==pServApp->GetAddr().m_addrHost)&&(sPort==srvaddr.sin_port) ) ){
					srcaddr.sin_port = htons( (short)pMsg->GetResult() );
					pServApp->AckPoll( socksrv, srcaddr );
				}
			}else if( pMsg->IsAcking(NCM_POLLSERVER) ){			//the server is acking our poll
				if( !(pServApp->GetAddr().m_addrHost==srcaddr.sin_addr) )pServApp->OnAckPoll( pMsg );
			}else if( pMsg->GetMessage()==NCM_SERVERSTART ){	//the server is starting, poll the server
				if( pServApp->IsProxy() && !(pServApp->GetAddr().m_addrHost==srcaddr.sin_addr) ){
					CMessageHead msg;
					msg.Init( NCM_POLLSERVER, CSessionAddr(), CSessionAddr(), NCP_SERVER_PING_PORT );
					srcaddr.sin_port = htons( NCP_SERVER_PING_PORT );
					msg.SendTo( socksrv, srcaddr );
				}
			}
		}
	}
	closesocket( socksrv );

	return 0;
}