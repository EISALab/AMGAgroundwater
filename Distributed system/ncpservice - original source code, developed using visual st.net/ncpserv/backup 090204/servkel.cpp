#include "../ncphead.h"
#include "servkel.h"
#include <algorithm>

using namespace std;

class op_serv_id
{
	int m_nServId;
public:
	op_serv_id( int nServId ) : m_nServId(nServId){}
	bool operator()( CServerSession* pServSession ){
		return pServSession->GetServId()==m_nServId;
	}
};

void CServerRouter::SendMessage( CMessageHead* pMsg )
{
	CSockLocker<CServerRouter> locker(this);

	//clear the outgoing bit, it is used only for local servers
	clrbit( pMsg->m_nFlags, MHF_OUTGOINGMSG );
	pMsg->Send( m_skMsg );
}

void CServerRouter::PostMessage( CMessageHead* pMsg )
{
	CMessageHead* pMsgClone = pMsg->Clone();

	//set the outgoing bit, so that RouteMessage will send the message out
	setbit(pMsgClone->m_nFlags, MHF_OUTGOINGMSG);
	m_MsgQueue.PutMessage( pMsgClone );
}

CMessageHead* CServerRouter::GetMessage()
{
	CMessageHead* pMsg = m_MsgQueue.GetMessage();
	if( pMsg->IsQuitting() ){
		pMsg->Destroy();
		return NULL;
	}
	return pMsg;
}

CServerSession* CServerRouter::GetServerSession( int nServId )
{
	list<CServerSession*>::iterator iter = find_if( m_ServSessions.begin(), m_ServSessions.end(), op_serv_id(nServId) );
	if( iter==m_ServSessions.end() )return NULL;

	return *iter;
}

bool CServerRouter::DispatchMessage( CMessageHead* pMsg )
{
	int nServId = pMsg->GetServId();
	if( nServId==GetServId() ){
		ProcessMessage( pMsg );
		pMsg->Destroy();
		return true;
	}else{
		CServerSession* pServSession = GetServerSession( nServId );
		if( pServSession==NULL )return false;

		pServSession->GetMessageQueue()->PutMessage( pMsg );
		return true;
	}
}

void CServerRouter::RouteMessage()
{
	CMessageHead* pMsg = NULL;
	while( (pMsg=GetMessage())!=NULL ){
		if( pMsg->IsIncoming() ){
			DispatchMessage( pMsg );
		}else{
			SendMessage( pMsg );
			pMsg->Destroy();
		}
	}
}

bool CServerRouter::ProcessMessage( CMessageHead* pMsg )
{
	COneIntMessage* pOneMsg = (COneIntMessage*)pMsg;

	switch( pMsg->GetCommand() )
	{
	case CM_OPENSESSION:
		OpenServSession( pMsg->GetClntId() );
		break;
	case CM_CLOSESESSION:
		COneIntMessage* pOneMsg = (COneIntMessage*)pMsg;
		pOneMsg->ntoh();
		CloseServSession( pOneMsg->GetInt() );
		break;
	};

	return FALSE;
}

bool CServerRouter::OpenServSession( int nClntId )
{
	//create the servier session
	int nServId = GetNextServId();
	CServerSession* pServSession = new CServerSession( this, nServId, nClntId );
	m_ServSessions.push_back( pServSession );
	pServSession->StartSession();

	//Ack the command
	COneIntMessage msg;
	msg.Init( CM_ACK, nClntId, GetServId(), nServId );
	msg.hton();

	SendMessage( msg.GetMessageHead() );
	return true;
}

bool CServerRouter::CloseServSession( int nServId ){
	list<CServerSession*>::iterator iter = find_if( m_ServSessions.begin(), m_ServSessions.end(), op_serv_id(nServId) );
	if( iter==m_ServSessions.end() )return false;

	CServerSession* pServSession = *iter;
	m_ServSessions.erase( iter );

	pServSession->WaitSession();
	delete pServSession;
	return true;
}


int CServerRouter::RouterThread( void* arg )
{
	CServerRouter* pServRouter = (CServerRouter*)arg;
	pServRouter->RouteMessage();
	return 0;
}

bool CServerRouter::StartRouter()
{
	ASSERT( m_hRouterThread==NULL );
	m_hRouterThread = CreateThread( (LPTHREAD_START_ROUTINE)RouterThread, this, 0 );
	if( m_hRouterThread==NULL )return false;
	return true;
}

void CServerRouter::WaitRouter()
{
	if( m_hRouterThread==NULL )return;

	WaitForThread( m_hRouterThread, NULL );
	CloseHandle( m_hRouterThread );
	m_hRouterThread = NULL;
}


void CServerSession::SendMessage( CMessageHead* pMsg )
{
	pMsg->SetServId( m_nServId );
	pMsg->SetClntId( m_nClntId );
	m_pServRouter->SendMessage( pMsg );
}

void CServerSession::PostMessage( CMessageHead* pMsg )
{
	pMsg->SetServId( m_nServId );
	pMsg->SetClntId( m_nClntId );
	m_pServRouter->PostMessage( pMsg );
}

void CServerSession::MessageLoop()
{
	try{
		CMessageHead* pMsg = NULL;
		while( (pMsg=GetMessage())!=NULL ){
			ProcessMessage( pMsg );
			pMsg->Destroy();
		}
	}catch(...){
	}
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

bool CServerSession::ProcessMessage( CMessageHead* pMsg )
{
	switch( pMsg->GetCommand() ){
	case CM_CLOSESESSION:
		Close();
		break;
	}
	return true;
}

bool CServerSession::StartSession()
{
	ASSERT( m_hSessionThread==NULL );
	m_hSessionThread = CreateThread( (LPTHREAD_START_ROUTINE)SessionThread, this, 0 );
	if( m_hSessionThread==NULL )return false;

	return true;
}

void CServerSession::WaitSession()
{
	if( m_hSessionThread==NULL )return;

	WaitForThread( m_hSessionThread, NULL );
	CloseHandle( m_hSessionThread );
	m_hSessionThread = NULL;
}

int CServerSession::SessionThread( void* arg )
{
	CServerSession* pServSession = (CServerSession*)arg;
	pServSession->MessageLoop();
	return 0;
}

bool CServerSession::Close()
{
	CMessageHead* pMsg = new CMessageHead( CM_QUIT );
	m_MsgQueue.PutMessage( pMsg );

	COneIntMessage msg;
	msg.Init( CM_CLOSESESSION, GetClntId(), m_pServRouter->GetServId(), GetServId() );
	msg.hton();
	m_pServRouter->GetMessageQueue()->PutMessage( msg.GetMessageHead()->Clone() );

	//acknowlege the command request
	CMessageHead msgAck;
	msgAck.Init( CM_ACK, GetClntId(), GetServId() );
	SendMessage( &msgAck );

	return true;
}


void CServerApp::RunServer()
{
	SOCKET socksrv;
	SOCKADDR_IN srvaddr;

	//setup the tcp server socket and bind.
	socksrv = Socket( PF_INET, SOCK_STREAM, 0 );

	srvaddr.sin_family = AF_INET;
	srvaddr.sin_addr.s_addr = INADDR_ANY;

	srvaddr.sin_port = htons( NCP_SERVER_PORT );
	Bind( socksrv, (SOCKADDR*)&srvaddr, sizeof(SOCKADDR_IN) );
	Listen( socksrv, 5 );

	//prepare to accept the socket connection
	int maxfds;
	fd_set rfds;

	//prepare to accept the socket connection
	while( true ){
		FD_ZERO( &rfds );
		FD_SET( socksrv, &rfds );

		maxfds = 0;
		map<SOCKET, CServerRouter*>::iterator pos;
		for( pos = m_SockMap.begin(); pos!=m_SockMap.end(); pos++ ){
			FD_SET( pos->first, &rfds );
			maxfds = max( pos->first, maxfds );
		}
		maxfds = max( socksrv, maxfds )+1;

		Select( maxfds, &rfds, NULL, NULL, NULL );
		if( FD_ISSET( socksrv, &rfds ) ){
			SOCKET sockclt = Accept( socksrv, (SOCKADDR*)NULL, NULL );
			CServerRouter* pServRouter = new CServerRouter( sockclt );
			pServRouter->StartRouter();
			m_SockMap[sockclt] = pServRouter;
		}

		for( pos = m_SockMap.begin(); pos!=m_SockMap.end(); pos++ ){
			if( FD_ISSET(pos->first, &rfds) ){
				CServerRouter* pServRouter = pos->second;
				CMessageHead* pMsg = RecvMessage( pos->first );
				pServRouter->GetMessageQueue()->PutMessage( pMsg );
			}
		}
	}

	closesocket( socksrv );
}