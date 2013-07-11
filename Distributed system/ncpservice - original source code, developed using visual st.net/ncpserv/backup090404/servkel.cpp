#include "../ncphead.h"
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
	list<CServerSession*>::iterator iter = find_if( m_ServSessions.begin(), m_ServSessions.end(), op_serv_addr(addr) );
	if( iter==m_ServSessions.end() )return NULL;

	return *iter;
}

void CServerProxy::RouteMessage()
{
	CMessageHead* pMsg=NULL;
	while( true ){
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
			vector<CServerSession*> tmpSessions;
			{
				CObjLocker<CServerProxy> locker(this);
				//notify the sessions to quit
				tmpSessions.resize( m_ServSessions.size() );
				list<CServerSession*>::iterator iter;
				CMessageHead msg(NCM_PROXYSHUTDOWN);
				for( iter = m_ServSessions.begin(); iter!=m_ServSessions.end(); iter++ ){
					(*iter)->PostMessage( msg.Clone() );
				}
				copy( m_ServSessions.begin(), m_ServSessions.end(), tmpSessions.begin() );
			}

			//wait for the sessions to quit
			for( int i=0; i<tmpSessions.size(); i++ )tmpSessions[i]->WaitForObject();

			//close and quite the routing loop
			m_pServApp->DestroyServerProxy( this );
			break;
		}

		DispatchMessage( pMsg );
	}
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
		{
			CObjLocker<CServerProxy> locker(this);
			m_ServSessions.push_back( pServSession );
		}
		pServSession->Open( this, addrClnt );
	}

	//Ack the command
	if( pServSession==NULL ){
		CMessageHead msg;
		msg.Init( NCF_ACK+NCM_OPENSESSION, GetAddr(), addrClnt, E_NOHOST );
		SendMessage( &msg );
	}else{
		CMessage1Param<CSessionAddr> msg;
		msg.Init( NCF_ACK+NCM_OPENSESSION, GetAddr(), addrClnt, pServSession->GetAddr() );
		msg.hton();
		SendMessage( msg.GetMessageHead() );
	}
}

void CServerProxy::CloseServerSession( CServerSession* pServSession )
{
	{
		CObjLocker<CServerProxy> locker(this);
		list<CServerSession*>::iterator iter = find( m_ServSessions.begin(), m_ServSessions.end(), pServSession );
		if( iter==m_ServSessions.end() )return;

		m_ServSessions.erase( iter );
	}
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
	m_hProxyThread = CreateThread( (LPTHREAD_START_ROUTINE)ServerProxyThread, this, 0 );
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
			CServerSession
***************************************************************************************************************/

void CServerSession::PostQuitMessage()
{
	CMessageHead* pMsg = new CMessageHead( NCM_QUIT );
	PostMessage( pMsg );
}

void CServerSession::PostMessage( CMessageHead* pMsg )
{
	m_MsgQueue.PutMessage( pMsg );
}

void CServerSession::MessageLoop()
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

CMessageHead* CServerSession::GetMessage()
{
	CMessageHead* pMsg = m_MsgQueue.GetMessage();
	if( pMsg->IsQuitting() ){
		pMsg->Destroy();
		return NULL;
	}
	return pMsg;
}

CMessageHead* CServerSession::GetMessage( int nCmd )
{
	CMessageHead* pMsg = m_MsgQueue.GetMessage();
	if( pMsg->GetCommand()!=nCmd || pMsg->IsQuitting() ){
		//this function put the message back.
		PostMessage( pMsg );
		return NULL;
	}
	return pMsg;
}

void CServerSession::ProcessMessage( CMessageHead* pMsg )
{
	switch( pMsg->GetCommand() ){
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

void CServerSession::DefaultMessage( CMessageHead* pMsg )
{
	CMessageHead msg;
	msg.Init( pMsg->GetCommand()+NCF_ACK, GetAddr(), GetClntAddr(), E_IMPLEMENT );
	m_pServProxy->SendMessage( &msg );
}

int CServerSession::ServerSessionThread( void* arg )
{
	CServerSession* pServSession = (CServerSession*)arg;
	pServSession->MessageLoop();
	return 0;
}

void CServerSession::WaitForObject()
{
	if( m_hSessionThread==NULL )return;

	WaitForThread( m_hSessionThread, NULL );
	CloseHandle( m_hSessionThread );
	m_hSessionThread = NULL;
}

bool CServerSession::StartServerSession()
{
	ASSERT( m_hSessionThread==NULL );
	m_hSessionThread = CreateThread( (LPTHREAD_START_ROUTINE)ServerSessionThread, this, 0 );
	if( m_hSessionThread==NULL )return false;

	return true;
}

bool CServerSession::Open( CServerProxy* pServProxy, const CSessionAddr& addrClnt )
{
	WaitForObject();
	m_pServProxy = pServProxy;
	m_addrClnt = addrClnt;
	return StartServerSession();
}

bool CServerSession::Close()
{
	m_pServProxy->CloseServerSession( this );

	CMessageHead msg;
	msg.Init( NCF_ACK+NCM_CLOSESESSION, GetAddr(), GetClntAddr() );
	m_pServProxy->SendMessage( &msg );

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
	return pServSession;
}

void CServerApp::ReturnServerSession( CServerSession* pServSession )
{
	CObjLocker<CServerApp> locker(this);
	m_SessionTockens.push_back( pServSession );
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
	m_SessionTockens.push_back( new CServerSession( CSessionAddr(GetLocalHostAddr(), 1) ) );
	m_SessionTockens.push_back( new CServerSession( CSessionAddr(GetLocalHostAddr(), 2) ) );

	SOCKET socksrv;
	SOCKADDR_IN srvaddr;

	//setup the tcp server socket and bind.
	socksrv = Socket( PF_INET, SOCK_STREAM, 0 );

	srvaddr.sin_family = AF_INET;
	srvaddr.sin_addr.s_addr = INADDR_ANY;

	srvaddr.sin_port = htons( NCP_SERVER_PORT );
	Bind( socksrv, (SOCKADDR*)&srvaddr, sizeof(SOCKADDR_IN) );
	Listen( socksrv, 5 );

	HANDLE hShutdownThread = CreateThread( (LPTHREAD_START_ROUTINE)ShutdownThread, this, 0 );

	//prepare to accept the socket connection
	while( true ){
		SOCKET sockclt = Accept( socksrv, (SOCKADDR*)NULL, NULL );
		CServerProxy* pServProxy = CreateServerProxy( sockclt );
		if( pServProxy )pServProxy->StartServerProxy();
	}

	closesocket( socksrv );
}

