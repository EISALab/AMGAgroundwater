#include "ncphead.h"
#include "servkel.h"
#include "netstd.h"
#include "uwkelobj.h"
#include <algorithm>
#include <strstream>
#include <vector>

//#define MAX_PATH	512
#define MAXPATH		MAX_PATH

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

CServerSession* CServerProxy::GetSession( const CSessionAddr& addr )
{
	CObjLocker< CServerProxy > locker(this);
	map<CSessionAddr, CServerSession*>::iterator iter = m_ServSessions.find( addr );
	if( iter==m_ServSessions.end() )return NULL;

	return iter->second;
}

void CServerProxy::InsertSession( CServerSession* pServSession )
{
	CObjLocker< CServerProxy > locker(this);
	m_ServSessions[ pServSession->GetAddr() ] = pServSession;
}

void CServerProxy::RemoveSession( const CSessionAddr& addr )
{
	CObjLocker< CServerProxy > locker(this);
	map<CSessionAddr, CServerSession*>::iterator iter = m_ServSessions.find( addr );
	if( iter!=m_ServSessions.end() )m_ServSessions.erase(iter);
}

void CServerProxy::ShutdownSessions()
{
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
	CMessageBase msg(NCM_PROXYSHUTDOWN);
	for( int i=0; i<tmpSessions.size(); i++ ){
		if( !tmpSessions[i]->IsClosed() ){
			tmpSessions[i]->PostMessage( msg.Clone() );
		}
	}
}

void CServerProxy::ShutdownStubs( SOCKET sock )
{
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
	//close the stubs
	for( int i=0; i<tmpSessions.size(); i++ ){
		if( !tmpSessions[i]->IsClosed() )tmpSessions[i]->Close();
	}
}

bool CServerProxy::MessageFromStream( SOCKET sock )
{
	CMessageBase* pMsg=NULL;
	try{
		pMsg = RecvMessage<CMessageBase>( sock );
	}catch( CSockException* e ){
		cdump<<"Stream Sock Error:"<<e->what()<<endl;
		pMsg = NULL;
		e->Delete();
	}
	if( pMsg!=NULL ){
		CMessageTrash trash(pMsg);
		ASSERT( pMsg->GetMessage()==NCM_OPENSTREAM );
		CServerSession* pSession = GetSession( pMsg->GetDstAddr() );
		if( pSession==NULL )return false;

		pSession->OpenStream( sock );
		return true;
	}
	return false;
}

bool CServerProxy::MessageFromClient()
{
	CMessageBase* pMsg=NULL;
	pMsg = RecvMessage<CMessageBase>( m_sock );

	if( pMsg==NULL ){	//the sock is shutdown
		//close and quite the routing loop
		return false;
	}

	DispatchMessage( pMsg );
	return true;
}

bool CServerProxy::MessageFromServer( SOCKET sock )
{
	CMessageBase* pMsg=NULL;
	try{
		pMsg = RecvMessage<CMessageBase>( sock );
	}catch( CSockException* e ){
		cdump<<"Stub Sock Error:"<<e->what()<<endl;
		pMsg = NULL;
		e->Delete();
	}

	if( pMsg==NULL ){	//the sock is shutdown or sock error, shutdown all stubs
		ShutdownStubs( sock );
		return false;
	}else{
		CServerSessionStub* pServStub = (CServerSessionStub*)GetSession( pMsg->GetDstAddr() );
		if( pServStub!=NULL ){	//let the stub deals with the message.
			pServStub->PostReturnMessage( pMsg );
		}else{
			//this is an acknowledge from server to client. ignore it.
		}
	}
	return true;
}

void CServerProxy::PumpWaitingMessages( SOCKET sock, bool bOk )
{
	//loop for each waited messages
	list<CMessageBase*>::iterator iter;
	for( iter=m_WaitingMessages.begin(); iter!=m_WaitingMessages.end(); ){
		CMessageBase* pMsg = *iter;
		//find the stub for the message
		CServerSession* pServSession = GetSession( pMsg->GetDstAddr() );
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
				if( pMsg->GetMessage()==NCM_OPENSESSION ){
					pServStub->Open2();
					ASSERT( pServStub->IsEstablished() );
					pServStub->PostMessage( pMsg );
				}
			}else{
				CMessageTrash trash(pMsg);
				if( pMsg->GetMessage()==NCM_OPENSESSION ){
					//close the stub.
					pServStub->Close();
					//acknowledge the message.
					CMessageBase msg;
					msg.Init( NCF_ACK|NCM_OPENSESSION, GetAddr(), pMsg->GetSrcAddr(), E_NOHOST );
					SendMessage( &msg );
				}
			}
			continue;
		}
		++iter;
	}
}

void CServerProxy::RouteMessage()
{
	SOCKET sockpas;
	SOCKADDR_IN pasaddr;
	//setup the passive socket server and bind.
	sockpas = Socket( PF_INET, SOCK_STREAM, 0 );
	pasaddr.sin_family = AF_INET;
	pasaddr.sin_addr.s_addr = INADDR_ANY;
	pasaddr.sin_port = 0;
	Bind( sockpas, (SOCKADDR*)&pasaddr, sizeof(SOCKADDR_IN) );
	socklen_t len = sizeof(SOCKADDR_IN);
	GetSockName( sockpas, (SOCKADDR*)&pasaddr, &len );
	m_nPasPort = ntohs( pasaddr.sin_port );

	Listen( sockpas, 5 );
	list<SOCKET> DataSocks;

	//prepare to accept the socket connection
	int maxfds;
	fd_set rfds;
	fd_set wfds;
	fd_set efds;

	//prepare to accept the socket connection
	try{
		while( true ){
			FD_ZERO( &rfds ); FD_ZERO( &wfds );	FD_ZERO( &efds );
			FD_SET( m_sock, &rfds );
			FD_SET( sockpas, &rfds );
			maxfds = max(sockpas, m_sock);


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
			list<SOCKET>::iterator iter;
			for( iter=DataSocks.begin(); iter!=DataSocks.end(); ++iter ){
				FD_SET( *iter, &rfds );
				maxfds = max(*iter, maxfds );
			}
			maxfds = maxfds + 1;

			Select( maxfds, &rfds, &wfds, &efds, NULL );
			if( FD_ISSET(m_sock, &rfds) ){			//get message from client socket.
				//if no message from client, the socket is closed, so quite the loop
				if( !MessageFromClient() )break;
			}
			//peer socket is ready for connecton or exception happened.
			for( pos = m_PeerSocks.begin(); pos!=m_PeerSocks.end(); ){ //note the MessageFromServer may remove a node from m_PeerSocks
				SOCKET s = *(pos->second);
				++pos;
				if( FD_ISSET(s, &rfds) ){			//get message from peer socket.
					MessageFromServer( s );
					continue;
				}

				if( FD_ISSET(s, &efds) ){			//connection is failed.
					//set socket status to error.
					CProxySocket* pProxySock = FindPeerSocket( s );
					pProxySock->SetState( PS_ERROR );
					ASSERT( s==(*pProxySock) );

					PumpWaitingMessages( s, false );
				}

				if( FD_ISSET(s, &wfds) ){			//connection is ready now.
					//get the IN_ADDR of the sock
					SOCKADDR_IN addr;
					socklen_t len = sizeof(SOCKADDR_IN);
					//GetPeerName( s, (SOCKADDR*)&addr, &len );
					//for linux or unix, failed connection will set wfds, using getpeername to decide if it is actually connected.
					bool suc = (getpeername(s, (SOCKADDR*)&addr, &len)==0);
					if( !suc ){
						//connection failed.
						CProxySocket* pProxySock = FindPeerSocket( s );
						pProxySock->SetState( PS_ERROR );
					}else{
						//set socket status to opened.
						CProxySocket* pProxySock = GetPeerSocket( addr.sin_addr );
						pProxySock->SetState( PS_OPENED );
						ASSERT( s==(*pProxySock) );
						//set the connection back to block mode.
						SetSockNonBlock( s, false );
					}
					PumpWaitingMessages( s, suc );
				}

			}
			if( FD_ISSET(sockpas, &rfds) ){
				SOCKET sockdata = Accept( sockpas, NULL, 0 );
				DataSocks.push_back( sockdata );
			}
			for( iter=DataSocks.begin(); iter!=DataSocks.end(); ){
				SOCKET sockdata = *iter;
				list<SOCKET>::iterator it = iter++;
				if( FD_ISSET(sockdata, &rfds) ){
					if( !MessageFromStream( sockdata ) )closesocket(sockdata);
					DataSocks.erase( it );
				}
			}


		}//end loop while(ture)
	}catch( CSockException* e ){
		cdump<<"Proxy sock Error:"<<e->what()<<endl;
		e->Delete();
	}
	//close the socket, shutdown the sessions and destroy the proxy object.
	{
		CSockLocker<CServerProxy> locker(this);
		//close the socket
		if( m_sock!=0 ){
			closesocket( m_sock );
			m_sock = 0;
		}
	}
	closesocket( sockpas );
	for_each( DataSocks.begin(), DataSocks.end(), closesocket );
	ShutdownSessions();
	m_pServApp->DestroyServerProxy( this );
}

void CServerProxy::OnUnreachable( CMessageBase* pMsg )
{
	if( pMsg->IsOutband() )return;	//don't ack an OOB message

	if( !pMsg->IsAcking() ){
		CMessageBase msg;
		msg.Init( NCF_ACK|pMsg->GetCommand(), GetAddr(), pMsg->GetSrcAddr(), E_BROKENPIPE );
		SendMessage( &msg );
	}
}

void CServerProxy::DispatchMessage( CMessageBase* pMsg )
{
	if( pMsg->GetDstAddr()==GetAddr() ){
		//this is a message for the proxy
		CMessageTrash trash(pMsg);
		ProcessMessage( pMsg );
	}else{
		//this is a message for sessions
		CServerSession* pServSession = GetSession( pMsg->GetDstAddr() );
		if( pServSession==NULL ){
			//no sessions found, unreachable message
			CMessageTrash trash(pMsg);
			OnUnreachable( pMsg );
		}else{
			//post the message to the session.
			pServSession->PostMessage( pMsg );
		}
	}
}

void CServerProxy::ProcessMessage( CMessageBase* pMsg )
{
	switch( pMsg->GetCommand() )
	{
	case NCM_OPENSESSION:
		CMessage1Param<CSessionAddr>* pOpenMsg = (CMessage1Param<CSessionAddr>*)pMsg;
		pOpenMsg->ntoh( false );
		OnOpenSession( pMsg->GetSrcAddr(), pOpenMsg->GetParam() );
//		OpenServerSession( pMsg->GetSrcAddr(), pOpenMsg->GetParam() );
		break;
	};

	return;
}

CServerSession* CServerProxy::CreateSession( const CSessionAddr& addrServ )
{
	ASSERT( NULL!=m_pServApp );
	CProxyToken* pToken = m_pServApp->RetrieveToken( addrServ );
	if( NULL==pToken )return NULL;

	CServerSession* pServSession = NULL;
	if( (pToken->GetServAddr()==pToken->GetPeerAddr()) && (pToken->GetServAddr().m_addrHost==m_pServApp->GetAddr().m_addrHost) ){
		//this is the local server.
		pServSession = new CServerSessionLocal( pToken );
	}else{
		//this is a proxy stub
		pServSession = new CServerSessionStub( pToken );
	}

	cdump<<lock<<"Session Created, Id: "<<pServSession->GetAddr().m_nServId<<endl<<unlock;
	return pServSession;
}

void CServerProxy::DestroySession( CServerSession* pServSession, bool bSockError )
{
	RemoveSession( pServSession->GetAddr() );
	m_pServApp->ReturnToken( pServSession->GetProxyToken(), bSockError );

	cdump<<lock<<"Session Destroyed, Id: "<<pServSession->GetAddr().m_nServId<<endl<<unlock;

	delete pServSession;
}

void CServerProxy::OnOpenSession( const CSessionAddr& addrClnt, const CSessionAddr& addrServ )
{
	CServerSession* pServSession = CreateSession( addrServ );
	if( pServSession!=NULL ){
		InsertSession( pServSession );
		pServSession->Open( this, addrClnt );
	}

	//Ack the command
	if( pServSession==NULL ){
		CMessageBase msg;
		msg.Init( NCF_ACK|NCM_OPENSESSION, GetAddr(), addrClnt, E_NOHOST );
		SendMessage( &msg );
	}else{
		//post the OPENSESSION message to the session, it will acknowledge it
		CMessage1Param<CSessionAddr> msg;
		msg.Init( NCM_OPENSESSION, pServSession->GetClntAddr(), pServSession->GetAddr(), addrServ );
		if( pServSession->IsEstablished() ){	//post the message for established session
			pServSession->PostMessage( msg.Clone() );
		}else{									//put the message into the waiting list, if session is not ready
			m_WaitingMessages.push_back( msg.Clone() );
		}
	}
}

void CServerProxy::SetProcessPriorityClass(DWORD dwPriority)
{
	//get a snapshot of the current activate sessions and release the locker.
	CObjLocker<CServerProxy> locker(this);
	map<CSessionAddr, CServerSession*>::iterator iter;
	for( iter = m_ServSessions.begin(); iter!=m_ServSessions.end(); iter++ ){
		if( (iter->second)->IsLocal() ){
			CServerSession* pSession = iter->second;
			CMessageBase msg;
			if( dwPriority==-1 ){	//kill the process
				msg.Init( NCM_KILLPROCESS, GetAddr(), pSession->GetAddr() );
			}else{
				msg.Init( NCM_PROCPRIORITY, GetAddr(), pSession->GetAddr(), dwPriority );
			}
			pSession->PostMessage( msg.Clone() );
		}
	}
}

int CServerProxy::GetRunningProcesses()
{
	int count = 0;
	//get a snapshot of the current activate sessions and release the locker.
	CObjLocker<CServerProxy> locker(this);
	map<CSessionAddr, CServerSession*>::iterator iter;
	for( iter = m_ServSessions.begin(); iter!=m_ServSessions.end(); iter++ ){
		if( (iter->second)->IsLocal() ){
			CServerSessionLocal* pSession = (CServerSessionLocal*)iter->second;
			if( pSession->GetRunningProcess()!=NULL )count++;
		}
	}
	return count;
}

/*void CServerProxy::OpenServerSession( const CSessionAddr& addrClnt, const CSessionAddr& addrServ )
{
	//retrieve the session tocken from the CServerApp.
	CServerSession* pServSession = m_pServApp->RetrieveServerSession( addrServ );
	if( pServSession!=NULL ){
		InsertSession( pServSession );
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
	RemoveSession( pServSession->GetAddr() );
	m_pServApp->ReturnServerSession( pServSession );
}*/

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

struct stream_proxy_thread_param
{
	CSessionAddr addrProxy;
	CSessionAddr addrPeer;
	SOCKADDR_IN addrPas;
	SOCKET		sock_cltdata;
};

void CServerSessionStub::OpenStream( SOCKET sock )
{
	stream_proxy_thread_param* pParam = new stream_proxy_thread_param;
	pParam->addrPas.sin_family = AF_INET;
	pParam->addrPas.sin_addr = GetPeerAddr().m_addrHost;
	pParam->addrPas.sin_port = htons( m_nPeerPasPort );
	pParam->sock_cltdata = sock;
	pParam->addrProxy = GetAddr();
	pParam->addrPeer = GetPeerAddr();

	HANDLE hThread = CreateThread( (THREAD_ROUTINE)StreamProxyThread, pParam, 0 );
	CloseHandle( hThread );
}

int CServerSessionStub::StreamProxyThread( void* arg )
{
#define MAXBUF 2048
	stream_proxy_thread_param* pParam = (stream_proxy_thread_param*)arg;
	SOCKET sock_cltdata = pParam->sock_cltdata;
	SOCKADDR_IN addr = pParam->addrPas;
	CSessionAddr addrSrc = pParam->addrProxy;
	CSessionAddr addrDst = pParam->addrPeer;
	delete pParam;

	//setup the passive socket server and bind.
	SOCKET sock_srvdata = Socket( PF_INET, SOCK_STREAM, 0 );
	Connect( sock_srvdata, (SOCKADDR*)&addr, sizeof(SOCKADDR_IN) );

	CMessageBase msg;
	msg.Init( NCM_OPENSTREAM, addrSrc, addrDst );
	SendMessage( sock_srvdata, &msg );

	//prepare to accept the socket connection
	int maxfds;
	fd_set rfds;
	char buf[MAXBUF];
	while( true ){
		FD_ZERO( &rfds );
		FD_SET( sock_cltdata, &rfds ); FD_SET( sock_srvdata, &rfds );
		maxfds = max(sock_cltdata,sock_srvdata)+1;
		Select( maxfds, &rfds, NULL, NULL, 0 );

		if( FD_ISSET(sock_cltdata, &rfds) ){
			int nLen  = recv(sock_cltdata, buf, ELEMENTS(buf), 0 );
			if( nLen<=0 )break;
			SendBuffer( sock_srvdata, buf, nLen );
		}
		if( FD_ISSET(sock_srvdata, &rfds) ){
			int nLen  = recv(sock_srvdata, buf, ELEMENTS(buf), 0 );
			if( nLen<=0 )break;
			SendBuffer( sock_cltdata, buf, nLen );
		}
	}
	closesocket( sock_cltdata );
	closesocket( sock_srvdata );
	return 0;
}

//reflect the messages returned from server to the client.
void CServerSessionStub::PostReturnMessage( CMessageBase* pMsg )
{
	CMessageTrash trash( pMsg );	//will destroy the message

	//save the m_pServProxy, note Close() may set m_pServProxy to NULL;
	CServerProxy* pServProxy = m_pServProxy;

	//this is from server to client. route it to ServerProxy
	if( !(pMsg->GetSrcAddr()==GetPeerAddr()) ){
		cdump<<"Msg Id:"<<pMsg->GetMessage()<<"\t Src IP:"<<pMsg->GetSrcAddr().m_addrHost.s_addr<<"\t Src Id:"<<pMsg->GetSrcAddr().m_nServId<<endl;
		cdump<<"Peer IP:"<<GetPeerAddr().m_addrHost.s_addr<<"\t Peer Id:"<<GetPeerAddr().m_nServId<<"\t Resutl:"<<pMsg->GetResult()<<endl;
	}
	ASSERT( pMsg->GetSrcAddr()==GetPeerAddr() );
	pMsg->SetSrcAddr( GetAddr() );
	pMsg->SetDstAddr( GetClntAddr() );
	if( pMsg->IsAcking(NCM_OPENSESSION) ){
		//for acking opensession, this is a message returned from proxy.
		pMsg->SetSrcAddr( m_pServProxy->GetAddr() );
		if( pMsg->IsSucceeded() ){		//open session successeeded
			CMessage1Param<CSessionAddr>* pAddrMsg = (CMessage1Param<CSessionAddr>*)pMsg;
			pAddrMsg->ntoh( false );
			SetPeerAddr( pAddrMsg->GetParam() );
			pAddrMsg->SetParam( GetAddr() );
			pAddrMsg->hton( false );
		}else{							//open session failed
			Close();
		}
	}else if( pMsg->IsAcking(NCM_CLOSESESSION) ){
		Close();
	}else if( pMsg->IsAcking(NCM_PASSIVE) && pMsg->IsSucceeded() ){
		//override the ACK PASSIVE message port number.
		CMessage1Param<u_int>* pPortMsg = (CMessage1Param<u_int>*)pMsg;
		pPortMsg->ntoh( false );
		m_nPeerPasPort = (u_short)pPortMsg->GetParam();
		pPortMsg->SetParam( m_pServProxy->GetPassivePort() );
		pPortMsg->hton( false );
	}
		
	pServProxy->SendMessage( pMsg );
}

//virtual functions
//note PostMessage declare the ownership of pMsg. So pMsg must be dynamically allocated.
void CServerSessionStub::PostMessage( CMessageBase* pMsg )
{
	CMessageTrash trash( pMsg );	//will destroy the message

	switch( pMsg->GetMessage() ){
	case NCM_PROXYSHUTDOWN:
		Close();
		return;
	case NCM_OPENSESSION:
		CMessage1Param<CSessionAddr>* pOpenMsg = (CMessage1Param<CSessionAddr>*)pMsg;
		pOpenMsg->SetParam( m_pProxyToken->GetPeerAddr() );
		pOpenMsg->hton( false );
		break;
	}

	//this is a message from client(or client proxy) to server. route it to proxy socket.
	if( pMsg->GetSrcAddr()==GetClntAddr() ){
		pMsg->SetSrcAddr( GetAddr() );
	}else{
		//it is maybe a message from client proxy
		ASSERT( pMsg->GetSrcAddr()==CSessionAddr(GetClntAddr().m_addrHost,0) );
		pMsg->SetSrcAddr( m_pServProxy->GetAddr() );
	}
	pMsg->SetDstAddr( GetPeerAddr() );

	try{
		SendPeerMessage( pMsg );
	}catch( CSockException* e ){
		cdump<<"Stub "<<GetAddr().m_nServId<<" Error:"<<e->what()<<endl;
		e->Delete();
		Close();
	}
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

	bool bSockError = m_pProxySock->IsError();

	//close the peer socket if this is the last Stub using the socket.
	if( pPeerSock->Release()==0 ){
		m_pServProxy->RemovePeerSocket( m_addrPeer.m_addrHost );
	}

	m_addrPeer.m_nServId = 0;
	m_pProxySock = NULL;
	m_nState = SS_CLOSED;

	m_pServProxy->DestroySession( this, bSockError );
//	m_pServProxy->CloseServerSession( this );
	return true;
}


/**************************************************************************************************************
			CServerSession
***************************************************************************************************************/

void CServerSessionLocal::AckMessage( int nCmd, int nRet, SOCKET sock )
{
	CMessageBase msg;
	msg.Init( NCF_ACK|nCmd, GetAddr(), GetClntAddr(), nRet );
	if( sock!=INVALID_SOCKET ){			//user provides the socket, ack through the provided socket
		::SendMessage( sock, &msg );
	}else{								//user didn't provide the socket, use the default proxy socket.
		m_pServProxy->SendMessage( &msg );
	}
}

void CServerSessionLocal::PostQuitMessage()
{
	CMessageBase* pMsg = new CMessageBase( NCM_QUIT );
	PostMessage( pMsg );
}

void CServerSessionLocal::PostMessage( CMessageBase* pMsg )
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
		cdump<<lock<<"proxy is waiting for session:"<<GetAddr().m_nServId<<endl<<unlock;
		WaitForThread( hSessionThread );
		CloseHandle( hSessionThread );
		cdump<<lock<<"session wake up :"<<GetAddr().m_nServId<<endl<<unlock;
	}
}

void CServerSessionLocal::MessageLoop()
{
	CMessageBase* pMsg = NULL;
//	while( (pMsg=GetMessage())!=NULL ){
	while( true ){
		cdump<<"Waitng Message:"<<pMsg->GetCommand()<<endl;
		pMsg=GetMessage();
		if( pMsg==NULL )break;
		cdump<<"Got Message Id:"<<pMsg->GetCommand()<<endl;
		try{
			CMessageTrash trash(pMsg);
			ProcessMessage( pMsg );
		}catch( CSockException* e ){
			cdump<<"Server "<<GetAddr().m_nServId<<" Error:"<<e->what()<<endl;
			e->Delete();
		}catch( CRemoteException* e ){
			cdump<<"Error:"<<e->what()<<endl;
			e->Delete();
			cdump<<"Exception deleted"<<endl;
		}
	}
}

CMessageBase* CServerSessionLocal::GetMessage()
{
	CMessageBase* pMsg = m_MsgQueue.GetMessage();
	if( pMsg->IsQuitting() ){
		CMessageTrash trash(pMsg);
		m_nState = SS_CLOSED;
		m_pServProxy->DestroySession( this );
		//m_pServProxy->CloseServerSession( this );
		return NULL;
	}
	return pMsg;
}


CMessageBase* CServerSessionLocal::WaitForMessage( int nMessage )
{
	while( true ){
		CMessageBase* pMsg = m_MsgQueue.GetMessage();
		if( pMsg->GetMessage()==nMessage )return pMsg;

		CMessageTrash trash(pMsg);
		switch( pMsg->GetMessage() ){
		case NCM_PROCPRIORITY:
			if( m_hRunningProcess!=0 ){
				DWORD dwPriority = pMsg->GetResult();
				if( ::GetProcessPriority(m_hRunningProcess)!=dwPriority )SetProcessPriority(m_hRunningProcess, dwPriority);
			}
			break;
		case NCM_KILLPROCESS:
			if( m_hRunningProcess!=0 )KillProcess( m_hRunningProcess );
			break;
		case NCM_PROXYSHUTDOWN:
		case NCM_QUIT:
			m_MsgQueue.PutMessage( pMsg->Clone() );
		default:
			throw new CRemoteException( E_UNEXPECTED_MSG );
		}
	}
	return NULL;
}

/*CMessageHead* CServerSessionLocal::GetMessage( int nCmd )
{
	CMessageHead* pMsg = m_MsgQueue.GetMessage();
	if( pMsg->GetCommand()!=nCmd || pMsg->IsQuitting() ){
		//this function put the message back.
		PostMessage( pMsg );
		return NULL;
	}
	return pMsg;
}*/

void CServerSessionLocal::AckOpen()
{
	CMessage1Param<CSessionAddr> msg;
	msg.Init( NCF_ACK|NCM_OPENSESSION, m_pServProxy->GetAddr(), GetClntAddr(), GetAddr() );
	m_pServProxy->SendMessage( &msg );
}

void CServerSessionLocal::AckClose()
{
	CMessageBase msg;
	msg.Init( NCF_ACK|NCM_CLOSESESSION, GetAddr(), GetClntAddr() );
	m_pServProxy->SendMessage( &msg );
	m_nState = SS_CLOSED;
}

void CServerSessionLocal::OnLogon( CMessageBase* pMsg )
{
	CMessage2Param<char*, char*> *pLogonMsg = (CMessage2Param<char*, char*>*)pMsg;
	string strUser = pLogonMsg->GetParam1();
	string strPassword = pLogonMsg->GetParam2();
	AckLogon( strUser, strPassword );
}

void CServerSessionLocal::AckServerInfo()
{
	SERVERINFO sysinfo;
	bzero( &sysinfo, sizeof(SERVERINFO) );

#ifdef _WIN32
	sysinfo.nOpSystem = SI_WIN32;
#endif

#ifdef _UNIX
	sysinfo.nOpSystem = SI_UNIX;
#endif

#ifdef _LINUX
	sysinfo.nOpSystem = SI_LINUX;
#endif

#ifdef _SHARED_DISK
	sysinfo.bShareDisk = true;
#else
	sysinfo.bShareDisk = false;
#endif

	sysinfo.nSysStat = m_pServProxy->GetServerApp()->GetState();
	sysinfo.bSysIdle = IsSystemFree( sysinfo.nSysStat );
	sysinfo.nSessions = m_pServProxy->GetServerApp()->GetRunningSessions();
	sysinfo.nChildPorcs = m_pServProxy->GetServerApp()->GetRunningProcesses();
	sysinfo.ipAddr = m_pServProxy->GetAddr().m_addrHost;

	CMessage1Param<SERVERINFO> msg;
	msg.Init( NCF_ACK|NCM_SERVERINFO, GetAddr(), GetClntAddr(), sysinfo );
	m_pServProxy->SendMessage( &msg );
}

void CServerSessionLocal::OnSoftLink( CMessageBase* pMsg )
{
	CMessage2Param<char*, char*> *pLogonMsg = (CMessage2Param<char*, char*>*)pMsg;
	const char* pszSrcName = pLogonMsg->GetParam1();
	const char* pszLnkName = pLogonMsg->GetParam2();
	AckSoftLink( MakePath(pszSrcName), MakePath(pszLnkName) );
}

void CServerSessionLocal::AckLogon( string& strUser, string& strPassword )
{
	char path[_MAX_PATH];

	//get home path
#ifdef _WIN32
	GetModuleFileName( NULL, path, ELEMENTS(path) );
	char* p = strrchr( path, '\\' );
	if( p!=NULL )*p = '\0';
#else
	getcwd( path, ELEMENTS(path) );
#endif
	dir_cat( path, strUser.c_str(), path, ELEMENTS(path) );		//now path is the current home path.

	//make home directory if necessary
	int nRetCode = S_OK;
	if( !IsDirExist(path) ){
		if( MkDir( path )!=0 ){
			nRetCode = E_ACCES;
		}
	}
	if( SUCCEEDED(nRetCode) ){
		m_strHomePath = path;
		m_strCurPath = path;

		if( !IsDirExist(m_strHomePath.c_str()) )nRetCode = E_NOUSER;
	}
	AckMessage( NCM_LOGON, nRetCode );
}

void CServerSessionLocal::AckPassive()
{
	u_int nPasPort = m_pServProxy->GetPassivePort();

	CMessage1Param<u_int> msg;
	msg.Init( NCF_ACK|NCM_PASSIVE, GetAddr(), GetClntAddr(), nPasPort );
	m_pServProxy->SendMessage( &msg );
}

void CServerSessionLocal::AckSoftLink( const string& strSrc, const string& strLnk )
{
	int nRetCode = S_OK;
#ifdef _WIN32
	nRetCode = E_NOSUPT;
#else
	if( symlink( strSrc.c_str(), strLnk.c_str() )!=0 )nRetCode = E_ACCES;
#endif

	//acknowledge the message.
	AckMessage( NCM_SOFTLINK, nRetCode );
}

SOCKET CServerSessionLocal::WaitStreamSocket()
{
	CMessage1Param<SOCKET>* pSockMsg = (CMessage1Param<SOCKET>*)WaitForMessage( NCM_OPENSTREAM );
	CMessageTrash trash(pSockMsg);
	if( pSockMsg->IsFailed() ){					//stream can't be opened. ack through the message channel.
		AckMessage( NCM_OPENSTREAM, E_OPENSTREAM );
		return INVALID_SOCKET;
	}
	return (SOCKET)pSockMsg->GetParam();
}

void CServerSessionLocal::AckListDir( const string& strPath )
{
	//the file is already there and the client doesn't overwrite it.
	if( !IsDirExist( strPath.c_str() ) ){
		AckMessage( NCM_LISTDIR, E_EXIST );
		return;
	}

	//1. acknowledge the command, the client should start stream sending now.
	AckMessage( NCM_LISTDIR, S_OK );

	//2. expect the openstream message
	SOCKET sock_stream = WaitStreamSocket();
	if( sock_stream==INVALID_SOCKET )return;
	AckMessage( NCM_OPENSTREAM, S_OK, sock_stream );

	//3. send the stream and close it.
	int nLen = SendDirEntries( sock_stream, strPath.c_str() );
	closesocket( sock_stream );

	//4. get streamlenth message to verify the file was delivered correctly
	CMessageBase* pMsg = WaitForMessage( NCM_STREAMLENGTH );
	CMessageTrash trash(pMsg);

	int nRetCode = S_OK;
	if( nLen==-1 || pMsg->GetResult()!=nLen ){
		nRetCode = E_STREAMLENGTH;
	}

	//acknowledge the message.
	AckMessage( NCM_STREAMLENGTH, nRetCode );
}

void CServerSessionLocal::AckPutFile( const string& strFileName )
{
	//the file is already there and the client doesn't overwrite it.
/*	if( !bOverwrite && IsFileExist( strFileName.c_str() ) ){
		AckMessage( NCM_PUTFILE, E_EXIST );
		return;
	}*/

	//1. acknowledge the command, the client should start stream sending now.
	CMessage1Param<int> msgAck;
	msgAck.Init( NCF_ACK|NCM_PUTFILE, GetAddr(), GetClntAddr(), m_nFileMode );
	m_pServProxy->SendMessage( &msgAck );

	//2. expect the openstream message
	SOCKET sock_stream = WaitStreamSocket();
	if( sock_stream==INVALID_SOCKET )return;
	AckMessage( NCM_OPENSTREAM, S_OK, sock_stream );

	//3. receive the file stream and close it.
	int nLen = RecvFileEx( sock_stream, strFileName.c_str(), m_nFileMode );
	closesocket( sock_stream );

	//4. get streamlenth message to verify the file was delivered correctly
	CMessageBase* pMsg = WaitForMessage( NCM_STREAMLENGTH );
	CMessageTrash trash(pMsg);

	int nRetCode = S_OK;
	if( nLen==-1 || pMsg->GetResult()!=nLen ){
		nRetCode = E_STREAMLENGTH;
	}

	//acknowledge the message.
	AckMessage( NCM_STREAMLENGTH, nRetCode );
}

void CServerSessionLocal::AckGetFile( const string& strFileName )
{
	//the file is not exist.
	if( !IsFileExist( strFileName.c_str() ) ){
		AckMessage( NCM_GETFILE, E_EXIST );
		return;
	}

	//1. acknowledge the command, the client should start stream sending now.
	CMessage1Param<int> msgAck;
	msgAck.Init( NCF_ACK|NCM_GETFILE, GetAddr(), GetClntAddr(), m_nFileMode );
	m_pServProxy->SendMessage( &msgAck );

	//2. expect the openstream message
	SOCKET sock_stream = WaitStreamSocket();
	if( sock_stream==INVALID_SOCKET )return;
	AckMessage( NCM_OPENSTREAM, S_OK, sock_stream );

	//3. receive the file stream and close it.
	int nLen = SendFileEx( sock_stream, strFileName.c_str(), m_nFileMode );
	closesocket( sock_stream );

	//4. get streamlenth message to verify the file was delivered correctly
	CMessageBase* pMsg = WaitForMessage( NCM_STREAMLENGTH );
	CMessageTrash trash(pMsg);

	int nRetCode = S_OK;
	if( nLen==-1 || pMsg->GetResult()!=nLen ){
		nRetCode = E_STREAMLENGTH;
	}

	//acknowledge the message.
	AckMessage( NCM_STREAMLENGTH, nRetCode );
}

void CServerSessionLocal::AckFileInfo( const string& strFileName )
{
	//the file is not exist.
	if( !IsFileExist( strFileName.c_str() ) ){
		AckMessage( NCM_FILEINFO, E_EXIST );
		return;
	}
	NETF_STAT stat;
	GetNetfStat( strFileName.c_str(), &stat );

	CMessage1Param<NETF_STAT> msg;
	msg.Init( NCF_ACK|NCM_FILEINFO, GetAddr(), GetClntAddr(), stat );
	m_pServProxy->SendMessage( &msg );
}

void CServerSessionLocal::AckFileMode( int nFileMode )
{
	if( m_nFileMode!=nFileMode )m_nFileMode = nFileMode;

	CMessageBase msg;
	msg.Init( NCF_ACK|NCM_FILEMODE, GetAddr(), GetClntAddr() );
	m_pServProxy->SendMessage( &msg );
}

int MonitorProcessThread( void* arg )
{
	CServerSessionLocal* pSession = (CServerSessionLocal*)arg;

	DWORD dwExitCode = 0;
	WaitForProcess( pSession->m_hRunningProcess, &dwExitCode );
#ifdef _WIN32
	printf( "windows process exit code:%d\n", dwExitCode );
#else
	int status = dwExitCode;
	if( WIFEXITED(status) )printf( "normal termination, exit status=%d\n", WEXITSTATUS(status) );
	else if( WIFSIGNALED(status) )printf( "abnormal termination, signal number=%d\n", WTERMSIG(status) );
	else if( WIFSTOPPED(status) )printf( "child stopped, signal number=%d\n", WSTOPSIG(status) );
#endif
	//post process exit message to the main session thread
	CMessageBase msg;
	msg.Init( NCM_PROCESSEXIT, pSession->GetAddr(), pSession->GetAddr(), dwExitCode );
	pSession->PostMessage( msg.Clone() );
	return dwExitCode;
}

void CServerSessionLocal::AckRunImage( const string& strCommand, bool bSync )
{
	DWORD dwExitCode = 0;
	DWORD dwPriority = GetProcessPriority();
	if( dwPriority==-1 ){
		AckMessage( NCM_RUNIMAGE, E_ACCES );
		return;
	}

	BOOL bOk = TRUE;
	if( bSync ){	//synchronize running, register the handle and wait the process
		HANDLE hProcess;
		bOk = CreateProcess( strCommand.c_str(), m_strCurPath.c_str(), &hProcess );
		if( bOk ){
			m_hRunningProcess = hProcess;
			SetProcessPriority( hProcess, dwPriority );
			HANDLE hMonitorThread = CreateThread( (THREAD_ROUTINE)MonitorProcessThread, this, 0 );

			//wait for NCM_PROCESSEXIT message, it is possed from the monitor thread.
			CMessageBase* pMsg = NULL;
			try{
				pMsg = WaitForMessage( NCM_PROCESSEXIT );
			}catch( exception* e ){
				KillProcess( hProcess );
				CloseHandle( hProcess );
				m_hRunningProcess = NULL;
				WaitForObject( hMonitorThread );
				CloseHandle( hMonitorThread );
				throw;
			}
			CMessageTrash trash(pMsg);
			dwExitCode = pMsg->GetResult();		//NCM_PROCESSEXIT returns exit code.
			CloseHandle( hProcess );
			CloseHandle( hMonitorThread );
			m_hRunningProcess = NULL;
		}
	}else{			//just run the process and return.
		bOk = CreateProcess( strCommand.c_str(), m_strCurPath.c_str(), NULL );
	}

	if( !bOk ){
		AckMessage( NCM_RUNIMAGE, E_NOEXEC );
		return;
	}else{
		CMessage1Param<int> msg;
		msg.Init( NCF_ACK|NCM_RUNIMAGE, GetAddr(), GetClntAddr(), dwExitCode, 0 );
		m_pServProxy->SendMessage( &msg );
		return;
	}
}

//chmod of a file access priviledge.
void CServerSessionLocal::AckChMod( const string& strFileName, int nMode )
{
	//translate nMode to system independent mode;
	int nSysMode = 0;
	if( isset( nMode, NETS_IREAD ) )nSysMode |= S_IREAD;
	if( isset( nMode, NETS_IWRITE ) )nSysMode |= S_IWRITE;
	if( isset( nMode, NETS_IEXEC ) )nSysMode |= S_IEXEC;

	int nRetCode = S_OK;
	if( chmod( strFileName.c_str(), nSysMode )!=0 )nRetCode = E_NOENT;

	AckMessage( NCM_CHMOD, nRetCode );
}

//remove a file from the server
void CServerSessionLocal::AckRemove( const string& strFileName )
{
	int nRetCode = S_OK;
	if( remove( strFileName.c_str() )!=0 )nRetCode = E_ACCES;

	//acknowledge the command
	AckMessage( NCM_REMOVE, nRetCode );
}


string CServerSessionLocal::ParsePathMessage( CMessageBase* pMsg )
{
	//parse the path parameters
	char* pPathName = (char*)pMsg->GetData();
	if( strcmp( pPathName, "~")==0 ){
		return m_strHomePath;
	}

	return MakePath( pPathName );
}

string CServerSessionLocal::MakePath( const char* pPathName )
{
	if( IsAbsDir( pPathName ) ){
		return pPathName;
	}else{
		char path[_MAX_PATH];
		expandpath( path, m_strCurPath.c_str(), pPathName, ELEMENTS(path) );
		//dir_cat( m_strCurPath.c_str(), pPathName, path, ELEMENTS(path) );
		return path;
	}
}

void CServerSessionLocal::AckMkDir( const string& strPath )
{
	int nRetCode = S_OK;
	//make the directory
	if( MkDir( strPath.c_str() )!=0 ){
		nRetCode = S_EXIST;
	}

	AckMessage( NCM_MKDIR, nRetCode );
}

void CServerSessionLocal::AckChDir( const string& strPath )
{
	int nRetCode = S_OK;
	//the path must be in the home directory
	if( IsSubDir(strPath.c_str(), m_strHomePath.c_str()) ){
		if( IsDirExist(strPath.c_str()) ){
			m_strCurPath = strPath;
		}else{
			nRetCode = E_NOENT;
		}
	}else{
		nRetCode = E_ACCES;
	}

	AckMessage( NCM_CHDIR, nRetCode );
}

void CServerSessionLocal::AckRmDir( const string& strPath )
{
	int nRetCode = S_OK;
	//make the directory
	if( rmdir( strPath.c_str() )!=0 ){
		nRetCode = E_EXIST;
	}

	AckMessage( NCM_RMDIR, nRetCode );
}

void CServerSessionLocal::AckEmptyDir( const string& strPath )
{
	int nRetCode = S_OK;
	if( erase_dir( strPath.c_str(), false )!=0 ){
		nRetCode = E_ACCES;
	}

	AckMessage( NCM_EMPTYDIR, nRetCode );
}


void CServerSessionLocal::ProcessMessage( CMessageBase* pMsg )
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
	case NCM_LOGON:
		OnLogon( pMsg );
		break;
	case NCM_MKDIR:
		AckMkDir( ParsePathMessage(pMsg) );
		break;
	case NCM_RMDIR:
		AckRmDir( ParsePathMessage(pMsg) );
		break;
	case NCM_CHDIR:
		AckChDir( ParsePathMessage(pMsg) );
		break;
	case NCM_EMPTYDIR:
		AckEmptyDir( ParsePathMessage(pMsg) );
		break;
	case NCM_LISTDIR:
		AckListDir( ParsePathMessage(pMsg) );
		break;
	case NCM_PASSIVE:
		AckPassive();
		break;
	case NCM_PUTFILE:
		AckPutFile( ParsePathMessage(pMsg) );
		break;
	case NCM_GETFILE:
		AckGetFile( ParsePathMessage(pMsg) );
		break;
	case NCM_REMOVE:
		AckRemove( ParsePathMessage(pMsg) );
		break;
	case NCM_CHMOD:
		AckChMod( ParsePathMessage(pMsg), pMsg->GetResult() );
		break;
	case NCM_FILEMODE:
		AckFileMode( pMsg->GetResult() );
		break;
	case NCM_FILEINFO:
		AckFileInfo( ParsePathMessage(pMsg) );
		break;
	case NCM_SERVERINFO:
		AckServerInfo();
		break;
	case NCM_SOFTLINK:
		OnSoftLink( pMsg );
		break;
	case NCM_RUNIMAGE:
		AckRunImage( ParsePathMessage(pMsg), pMsg->GetResult()!=0 );
		break;
	default:
		DefaultMessage( pMsg );
		break;
	}
}

void CServerSessionLocal::DefaultMessage( CMessageBase* pMsg )
{
	if( !pMsg->IsOutband() && !pMsg->IsAcking() )AckMessage( pMsg->GetCommand(), E_IMPLEMENT );
}

int CServerSessionLocal::ServerSessionThread( void* arg )
{
	CServerSessionLocal* pServSession = (CServerSessionLocal*)arg;
	pServSession->MessageLoop();
	return 0;
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

void CServerSessionLocal::OpenStream( SOCKET sock )
{
	CMessage1Param<SOCKET> msg;
	msg.Init( NCM_OPENSTREAM, GetAddr(), m_pServProxy->GetAddr(), sock );
	PostMessage( msg.Clone() );
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
	CSessionAddr addrProxy( GetLocalInAddr(), 0 );
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

CProxyToken* CServerApp::RetrieveToken( const CSessionAddr& addrServ )
{
	CObjLocker<CServerApp> locker(this);
	//search the server token
	map<CSessionAddr, CProxyToken*>::iterator pos = m_ServTokens.find( addrServ );
	if( pos==m_ServTokens.end() )return NULL;

	CProxyToken* pToken = pos->second;
	ASSERT( NULL!=pToken );

	//if( pToken->IsDead() )
	pToken->SetState( CTS_RUNNING );
	m_ServTokens.erase( pos );

	return pToken;
}

void CServerApp::ReturnToken( CProxyToken* pToken, bool bSockError )
{
	CObjLocker<CServerApp> locker(this);

	if( bSockError ){
		IN_ADDR addrHost = pToken->m_addrServ.m_addrHost;
		//mark all the existing idle tokens from the host as dead
		map<CSessionAddr, CProxyToken*>::iterator pos = m_PeerTokens.lower_bound( CSessionAddr(addrHost,0) );
		while( pos!=m_PeerTokens.end() && ( pos->second->GetPeerAddr().m_addrHost==addrHost) ){
			if( pos->second->IsIdle() ){
				++pos;
				m_PeerTokens.erase( pToken->GetPeerAddr() );
				m_ServTokens.erase( pToken->GetServAddr() );
				delete pToken;
			}
		}
	}else{
		//return token to the server token map
		pToken->SetState( CTS_IDLE );
		m_ServTokens[ pToken->GetServAddr() ] = pToken;
	}
}

/*CServerSession* CServerApp::RetrieveServerSession( const CSessionAddr& addrServ )
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
}*/

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

void CServerApp::SetState( int nState )
{
	if( m_nState!=nState ){
		DWORD dwPriority = MapProcessPriority( nState );
		CObjLocker<CServerApp> locker(this );
		list<CServerProxy*>::iterator pos = m_ServerProxies.begin();
		while( pos!=m_ServerProxies.end() ){
			(*pos)->SetProcessPriorityClass(dwPriority);
			++pos;
		}
		m_nState = nState;
	}
}

void CServerApp::RunServer()
{		
	m_addr.m_addrHost = GetLocalInAddr();
	m_addr.m_nServId = 0;

	if( IsServer() ){
		CPU_INFO cpuinfo;
        GetCpuInfo( &cpuinfo );
		for( int i=0; i<cpuinfo.nCpus; i++ ){
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
			char buf[_MAX_LINE];
			ifg.getline( buf, ELEMENTS(buf) );
			if( ifg.fail() )break;
			if( *trim(buf)=='\0' )continue;
			IN_ADDR addrHost = GetHostInAddr( buf );
			if( m_HostAddrs.find(addrHost)==m_HostAddrs.end() ){
				m_HostAddrs.insert( addrHost );
			}
		}
	}
	m_hPollThread = CreateThread( (THREAD_ROUTINE)PollThread, this, 0 );

#ifndef _WIN32
	StartLoginMonitorThread();
#endif

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

	try{
		//prepare to accept the socket connection
		while( true ){
			SOCKET sockclt = Accept( socksrv, (SOCKADDR*)NULL, NULL );
			CServerProxy* pServProxy = CreateServerProxy( sockclt );
			if( pServProxy )pServProxy->StartServerProxy();
		}
	}catch( CRemoteException* e ){
		cdump<<"Error:"<<e->what()<<endl;
		e->Delete();
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
		CMessageBase msg;
		msg.Init( NCM_POLLSERVER, CSessionAddr(), CSessionAddr(), NCP_SERVER_PING_PORT );
		dstaddr.sin_port = htons( NCP_SERVER_PING_PORT );
		dstaddr.sin_addr = arrAddrs[i];
		cdump<<"polling host:"<<inet_ntoa(arrAddrs[i])<<endl;
		SendMessageTo( sock, &msg, dstaddr );
	}
}

void CServerApp::AckPoll( SOCKET sock, SOCKADDR_IN& addr ){
	CObjLocker<CServerApp> locker(this);
	int nLength = sizeof(CMessageBase)+m_ServTokens.size()*sizeof(TOKEN_INFO);
	CMessageBase* pMsg = CMessageBase::Alloc( nLength );
	TOKEN_INFO* pinfo = (TOKEN_INFO*)(pMsg+1);
	CMessageTrash trash(pMsg);

	pMsg->Init( NCF_ACK+NCM_POLLSERVER, GetAddr(), CSessionAddr(addr.sin_addr, 0), m_ServTokens.size() );
	map<CSessionAddr, CProxyToken*>::iterator iter;
	for( iter=m_ServTokens.begin(); iter!=m_ServTokens.end(); ++iter ){
		pinfo->nServId = iter->first.m_nServId;
		pinfo->nCpuType = iter->second->GetCpuType();
		pinfo->nCpuMhz = iter->second->GetCpuMhz();
		pinfo->hton();
		pinfo++;
	}
	pMsg->SetLength( nLength );
	SendMessageTo( sock, pMsg, addr );
}

void CServerApp::OnAckPoll( CMessageBase* pMsg ){
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
			delete pToken;
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

#ifdef _WIN32
	SetUdpBlock( socksrv );
#endif

	CMessageBase msg;
	if( pServApp->IsProxy() ){
		pServApp->DoPoll( socksrv );
		msg.Init( NCM_POLLSERVER, CSessionAddr(), CSessionAddr(), NCP_SERVER_PING_PORT );
	}else if( pServApp->IsServer() ){
		msg.Init( NCM_SERVERSTART, CSessionAddr(), CSessionAddr() );
//		msg.Init( NCM_POLLSERVER, CSessionAddr(), CSessionAddr() );
	}
	if( pServApp->IsProxy() || pServApp->IsServer() ){
		SOCKET sockbroad = Socket( PF_INET, SOCK_DGRAM, 0 );
//		msg.Broadcast( sockbroad, NCP_SERVER_PING_PORT );
		closesocket( sockbroad );
	}

	while( true ){
		fd_set rfds;
		FD_ZERO( &rfds );
		FD_SET( socksrv, &rfds );
		int maxfds = socksrv + 1;

		TIMEVAL tmout, *pto;
		if( pServApp->IsProxy() ){
			tmout.tv_sec = 10;
			tmout.tv_usec = 0;
			pto = &tmout;
		}else pto = NULL;

		int nRet = Select( maxfds, &rfds, NULL, NULL, pto );

		if( nRet==0 ){	//select timeout, should I do a poll for the remote hosts?
			if( pServApp->IsProxy() )pServApp->DoPoll( socksrv );
		}else if( FD_ISSET(socksrv, &rfds) ){
			SOCKADDR_IN srcaddr;
			CMessageBase* pMsg = RecvMessageFrom<CMessageBase>( socksrv, srcaddr );
			CMessageTrash trash(pMsg);
//			cdump<<inet_ntoa(srcaddr.sin_addr)<<" with message id:"<<pMsg->GetMessage()<<endl;
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
					CMessageBase msg;
					msg.Init( NCM_POLLSERVER, CSessionAddr(), CSessionAddr(), NCP_SERVER_PING_PORT );
					srcaddr.sin_port = htons( NCP_SERVER_PING_PORT );
					SendMessageTo( socksrv, &msg, srcaddr );
				}
			}
		}
	}
	closesocket( socksrv );

	return 0;
}

#ifndef _WIN32
void CServerApp::StartLoginMonitorThread()
{
#if _SHARE_PRIORITY<3
	HANDLE hThread = CreateThread( (THREAD_ROUTINE)LoginMonitorThread, this, 0 );
	CloseHandle( hThread );
#endif
}

int CServerApp::LoginMonitorThread( void* arg )
{
	CServerApp* pServApp = (CServerApp*)arg;
	ASSERT( pServApp!=NULL );
	char buf[5*_MAX_LINE];
	//retrieve the service login name
	char username[_MAX_LINE];
	strcpy( username, getlogin() );

	while( true ){
		MilliSleep( 20*1000 );

		bool bMoreLogon = false;

		FILE* pf = popen( "users", "r" );
		if( pf!=NULL && !feof(pf) ){
			fgets( buf, ELEMENTS(buf), pf );
			istrstream istr(buf, strlen(buf)+1);
			//check if other user is logged in
			while( true ){
				char login_name[_MAX_LINE];
				istr>>login_name;
				if( istr.fail() || istr.eof() )break;
				if( strcmp(username, login_name)!=0 ){
					bMoreLogon = true;
					break;
				}
			}
		}
		if( pf!=NULL )pclose( pf );

		int nSysState = pServApp->GetState();
		if( bMoreLogon ){
#if _SHARE_PRIORITY==2
			setbit( nSysState, SYSTEM_USER_LOGON );
#else
			setbit( nSysState, SYSTEM_SUSPEND_SERVICE );
#endif
		}else{
#if _SHARE_PRIORITY==2
			clrbit( nSysState, SYSTEM_USER_LOGON );
#else
			clrbit( nSysState, SYSTEM_SUSPEND_SERVICE );
#endif
		}
		pServApp->SetState( nSysState );
	}
	return 0;
}
#endif

int CServerApp::GetRunningProcesses()
{
	int count = 0;
	CObjLocker<CServerApp> locker(this);
	list<CServerProxy*>::iterator iter;
	for( iter=m_ServerProxies.begin(); iter!=m_ServerProxies.end(); ++iter ){
		count += (*iter)->GetRunningProcesses();
	}
	return count;
}


