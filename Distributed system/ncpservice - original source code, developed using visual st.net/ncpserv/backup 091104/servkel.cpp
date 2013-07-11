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
	pMsg->Send( m_sock );
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
				tmpSessions[i]->WaitForObject();
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
		OpenServerSession( pMsg->GetSrcAddr() );
		break;
	};

	return;
}

void CServerProxy::OpenServerSession( const CSessionAddr& addrClnt )
{
	//retrieve the session tocken from the CServerApp.
	CServerSession* pServSession = m_pServApp->RetrieveServerSession();
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
		CMessageHead msg;
		msg.Init( NCM_OPENSESSION, pServSession->GetClntAddr(), pServSession->GetAddr() );
		if( pServSession->IsEstablished() ){	//post the message for established session
			pServSession->PostMessage( msg.Clone() );
		}else{									//put the message into the waiting list, if session is not ready
			m_WaitingMessages.push_back( msg.Clone() );
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

	m_pServProxy->CloseServerSession( this );

	m_addrPeer.m_nServId = 0;
	m_pProxySock = NULL;
	m_nState = SS_CLOSED;
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
	m_MsgQueue.PutMessage( pMsg );
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
	m_nState = SS_CLOSED;
}

CMessageHead* CServerSessionLocal::GetMessage()
{
	CMessageHead* pMsg = m_MsgQueue.GetMessage();
	if( pMsg->IsQuitting() ){
		pMsg->Destroy();
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

void CServerSessionLocal::ProcessMessage( CMessageHead* pMsg )
{
	switch( pMsg->GetCommand() ){
	case NCM_OPENSESSION:
		AckOpen();
		break;
	case NCM_CLOSESESSION:
		Close();
		break;
	case NCM_PROXYSHUTDOWN:
		m_pServProxy->CloseServerSession( this );
		PostQuitMessage();
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

void CServerSessionLocal::WaitForObject()
{
	if( m_hSessionThread==NULL )return;

	WaitForThread( m_hSessionThread, NULL );
	CloseHandle( m_hSessionThread );
	m_hSessionThread = NULL;
}

bool CServerSessionLocal::StartServerSession()
{
	ASSERT( m_hSessionThread==NULL );
	m_hSessionThread = CreateThread( (THREAD_ROUTINE)ServerSessionThread, this, 0 );
	if( m_hSessionThread==NULL )return false;

	return true;
}

bool CServerSessionLocal::Open( CServerProxy* pServProxy, const CSessionAddr& addrClnt )
{
	WaitForObject();
	ASSERT( IsClosed() );
	m_pServProxy = pServProxy;
	m_addrClnt = addrClnt;
	m_nState = SS_ESTABLISHED;
	return StartServerSession();
}

bool CServerSessionLocal::Close()
{
	m_pServProxy->CloseServerSession( this );

	CMessageHead msg;
	msg.Init( NCF_ACK|NCM_CLOSESESSION, GetAddr(), GetClntAddr() );
	m_pServProxy->SendMessage( &msg );

	PostQuitMessage();
	m_nState = SS_CLOSED;
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

	while( !m_SessionTockens.empty() ){
		CServerSession* pServSession = m_SessionTockens.front();
		m_SessionTockens.pop_front();
		delete pServSession;
	}

    CloseHandle( m_hMutex );
}

CServerSession* CServerApp::RetrieveServerSession()
{
	CObjLocker<CServerApp> locker(this);
	if( m_SessionTockens.empty() )return NULL;

	CServerSession* pServSession = m_SessionTockens.front();
	m_SessionTockens.pop_front();

	cdump<<lock<<"Session Opened, Id: "<<pServSession->GetAddr().m_nServId<<endl<<unlock;
	return pServSession;
}

void CServerApp::ReturnServerSession( CServerSession* pServSession )
{
	CObjLocker<CServerApp> locker(this);
	m_SessionTockens.push_back( pServSession );

	cdump<<lock<<"Session Closed, Id: "<<pServSession->GetAddr().m_nServId<<endl<<unlock;
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
	GetCpuInfo( &m_CpuInfo );

	int id = 1;
	if( !m_strHosts.empty() ){
		ifstream ifg(m_strHosts.c_str());
		string strBuf;
		while( true ){
			char buf[MAXLINE];
			ifg.getline( buf, ELEMENTS(buf) );
			if( ifg.eof() || ifg.fail() )break;
			if( trim(buf)=="\0" )continue;
			m_SessionTockens.push_back( new CServerSessionStub( CSessionAddr(GetLocalHostAddr(), id++),
					GetHostInAddr(buf) ) );
		}
	}
	for( int i=0; i<m_CpuInfo.nCpus; i++ ){
		m_SessionTockens.push_back( new CServerSessionLocal( CSessionAddr(GetLocalHostAddr(), id++) ) );
	}
	cdump<<m_CpuInfo.nCpus<<" local servers started at CPU speed "<<m_CpuInfo.nCpuMhz<<" Mhz"<<endl;

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

void CServerApp::Broadcast( SOCKET sock, CMessageHead* pMsg )
{
	//enable broadcast
	BOOL bVal = TRUE;
	setsockopt( sock, SOL_SOCKET, SO_BROADCAST, (char*)&bVal, sizeof(BOOL) );

	//broadcast the server poll message to local net at startup.
	//create the broadcast address
	SOCKADDR_IN dstaddr;
	dstaddr.sin_family = AF_INET;
	dstaddr.sin_port = htons( NCP_SERVER_PING_PORT );
	dstaddr.sin_addr.s_addr = INADDR_BROADCAST;

	int nLength = pMsg->GetLength();
	pMsg->hton();
	sendto( sock, (char*)pMsg, nLength, 0, (SOCKADDR*)&dstaddr, sizeof(SOCKADDR_IN) );

	//disable broadcast
	bVal = FALSE;
	setsockopt( sock, SOL_SOCKET, SO_BROADCAST, (char*)&bVal, sizeof(BOOL) );
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

	int nMessage = NCM_SERVERSTART;
	if( pServApp->IsProxy() ){
		nMessage = NCM_POLLSERVER;
	}
	CMessageHead msg(nMessage);
	pServApp->Broadcast( socksrv, &msg );

	while( true ){
		fd_set rfds;
		FD_ZERO( &rfds );
		FD_SET( socksrv, &rfds );
		int maxfds = socksrv + 1;

		TIMEVAL tmout;
		tmout.tv_sec = 30;
		tmout.tv_usec = 0;
		int nRet = Select( maxfds, &rfds, NULL, NULL, &tmout );

		if( nRet==0 ){	//select timeout, should I do a poll for the remote hosts?
			if( pServApp->IsProxy() )pServApp->DoPoll( socksrv );
		}else if( FD_ISSET(socksrv, &rfds) ){
			SOCKADDR_IN srcaddr;
			socklen_t addrlen = sizeof(SOCKADDR_IN);
			CMessageHead msg;
			int nLen = recvfrom( socksrv, (char*)&msg, sizeof(CMessageHead), MSG_PEEK, (SOCKADDR*)&srcaddr, &addrlen );
			ASSERT( nLen>=sizeof(CMessageHead) );
			msg.ntoh();
			char* buf = new char[ msg.GetLength() ];
			recvfrom( socksrv, buf, msg.GetLength(), 0, (SOCKADDR*)&srcaddr, &addrlen );

			CMessageHead* pMsg = (CMessageHead*)pMsg;
			CMessageTrash trash(pMsg);
			pMsg->ntoh();
			if( pMsg->GetMessage()==NCM_POLLSERVER ){
				pServApp->AckPoll( socksrv, srcaddr );
			}else if( pMsg->IsAcking(NCM_POLLSERVER) ){			//the server is acking our poll
				pServApp->OnAckPoll( pMsg );
			}else if( pMsg->GetMessage()==NCM_SERVERSTART ){	//the server is starting, poll the server
				if( pServApp->IsProxy() ){
					CMessageHead msg(NCM_POLLSERVER);
					msg.hton();
					sendto( socksrv, (char*)&msg, sizeof(CMessageHead), 0, (SOCKADDR*)&srcaddr, sizeof(SOCKADDR_IN) );
				}
			}
		}
	}
	closesocket( socksrv );

	return 0;
}