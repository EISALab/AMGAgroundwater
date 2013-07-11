#include "clntkel.h"
#include "uwkelobj.h"
#include "netstd.h"
#include "netsock.h"
#include <map>

#define MAXFILEBUF		0x8000
#define MAXCMDBUF		512
#define MAXPATH			256
#define MAXLINE			256
#define MAXRETRY		5

extern FILE* pfout;

CMessageBase* CClientSession::WaitForMessage( int nMessage )
{
	CMessageBase* pMsg = m_MsgQueue.GetMessage();
	if( pMsg->GetMessage()==nMessage )return pMsg;

	CMessageTrash trash(pMsg);
	switch( pMsg->GetMessage() ){
	case NCM_PROXYSHUTDOWN:
	case NCM_SERVERSHUTDOWN:
	default:
		throw new CRemoteException( E_UNEXPECTED_MSG );
	}
}

bool CClientSession::Open()
{
	if( m_pClntProxy->OpenChannel(this) ){
		CMessage1Param<CSessionAddr> msg;
		msg.Init( NCM_OPENSESSION, GetAddr(), GetServAddr(), m_pServToken->GetServAddr() );
		m_pClntProxy->SendMessage( &msg );

		CMessageBase* pAck = WaitForMessage(NCF_ACK|NCM_OPENSESSION);
		CMessageTrash trash( pAck );

		if( pAck!=NULL && pAck->IsSucceeded() ){
			CMessage1Param<CSessionAddr>* pAckOpen = (CMessage1Param<CSessionAddr>*)pAck;
			pAckOpen->ntoh( false );
			m_addrServ = pAckOpen->GetParam();
			SetState( CSS_ESTABLISHED );
			return true;
		}else{
			cdump<<lock<<"open "<<GetToken()->m_addrServ.m_nServId<<" failed:"<<(pAck==NULL? 0 : pAck->GetResult())<<endl<<unlock;
			m_pServToken->SetDead();
		}

	}
	return false;
}

bool CClientSession::Close()
{
	CMessageBase* pAck = NULL;
	if( IsEstablished() ){
		CMessageBase msg;
		msg.Init( NCM_CLOSESESSION, GetAddr(), GetServAddr() );
		m_pClntProxy->SendMessage( &msg );

		pAck = WaitForMessage(NCF_ACK|NCM_CLOSESESSION);
		SetState( CSS_CLOSED );
	}

	m_pClntProxy->CloseChannel( this );
	CMessageTrash trash( pAck );
	return pAck!=NULL && pAck->IsSucceeded();
}

HRESULT CClientSession::DoLogon( const string& strUser, const string& strPassword )
{
	CMessage2Param<char*, char*>* pMsg = (CMessage2Param<char*, char*>*)CMessageBase::Alloc( sizeof(CMessageBase)+strUser.length()+strPassword.length()+2 );
	pMsg->Init( NCM_LOGON, GetAddr(), GetServAddr(), strUser.c_str(), strPassword.c_str() );
	CMessageTrash trash(pMsg);

	m_pClntProxy->SendMessage( pMsg );

	CMessageBase* pAck = WaitForMessage( NCF_ACK|NCM_LOGON );
	CMessageTrash trash2( pAck );
	return pAck->GetResult();
}

HRESULT CClientSession::DoMkDir( const string& strPath )
{
	CMessage1Param<char*>* pMsg = (CMessage1Param<char*>*)CMessageBase::Alloc( sizeof(CMessageBase)+strPath.length()+1 );
	pMsg->Init( NCM_MKDIR, GetAddr(), GetServAddr(), strPath.c_str() );
	CMessageTrash trash(pMsg);

	m_pClntProxy->SendMessage( pMsg );

	CMessageBase* pAck = WaitForMessage( NCF_ACK|NCM_MKDIR );
	CMessageTrash trash2( pAck );
	return pAck->GetResult();
}

HRESULT CClientSession::DoChDir( const string& strPath )
{
	CMessage1Param<char*>* pMsg = (CMessage1Param<char*>*)CMessageBase::Alloc( sizeof(CMessageBase)+strPath.length()+1 );
	pMsg->Init( NCM_CHDIR, GetAddr(), GetServAddr(), strPath.c_str() );
	CMessageTrash trash(pMsg);

	m_pClntProxy->SendMessage( pMsg );

	CMessageBase* pAck = WaitForMessage( NCF_ACK|NCM_CHDIR );
	CMessageTrash trash2( pAck );
	return pAck->GetResult();
}
	

HRESULT CClientSession::DoRmDir( const string& strPath )
{
	CMessage1Param<char*>* pMsg = (CMessage1Param<char*>*)CMessageBase::Alloc( sizeof(CMessageBase)+strPath.length()+1 );
	pMsg->Init( NCM_RMDIR, GetAddr(), GetServAddr(), strPath.c_str() );
	CMessageTrash trash(pMsg);

	m_pClntProxy->SendMessage( pMsg );

	CMessageBase* pAck = WaitForMessage( NCF_ACK|NCM_RMDIR );
	CMessageTrash trash2( pAck );
	return pAck->GetResult();
}
	
HRESULT CClientSession::DoEmptyDir( const string& strPath )
{
	CMessage1Param<char*>* pMsg = (CMessage1Param<char*>*)CMessageBase::Alloc( sizeof(CMessageBase)+strPath.length()+1 );
	pMsg->Init( NCM_EMPTYDIR, GetAddr(), GetServAddr(), strPath.c_str() );
	CMessageTrash trash(pMsg);

	m_pClntProxy->SendMessage( pMsg );

	CMessageBase* pAck = WaitForMessage( NCF_ACK|NCM_EMPTYDIR );
	CMessageTrash trash2( pAck );
	return pAck->GetResult();
}
	
HRESULT CClientSession::DoPassive( u_int& nPasPort )
{
	CMessageBase msg;
	msg.Init( NCM_PASSIVE, GetAddr(), GetServAddr() );
	m_pClntProxy->SendMessage( &msg );

	CMessage1Param<u_int>* pMsg = (CMessage1Param<u_int>*)WaitForMessage( NCF_ACK|NCM_PASSIVE );
	CMessageTrash trash( pMsg );
	if( pMsg->IsSucceeded() ){
		pMsg->ntoh( false );
		nPasPort = pMsg->GetParam();
	}
	return pMsg->GetResult();
}

SOCKET CClientSession::OpenStream( u_int nPasPort )
{
	SOCKADDR_IN addr;
	addr.sin_family = AF_INET;
	addr.sin_addr = GetServAddr().m_addrHost;
	addr.sin_port = htons( (u_short)nPasPort );

	SOCKET sock_stream = Socket( PF_INET, SOCK_STREAM, 0 );
	ASSERT( sock_stream!=INVALID_SOCKET );

	//import, must retry the socket initilization a few times.
	try{
		if( RedundantConnect( sock_stream, addr )!=0 )throw new CSockException();

		//open the data stream channel.
		CMessageBase msg;
		msg.Init( NCM_OPENSTREAM, GetAddr(), GetServAddr() );
		SendMessage( sock_stream, &msg );
		CMessageBase* pMsg = RecvMessage<CMessageBase>( sock_stream );
		if( pMsg==NULL )throw new CRemoteException(E_BROKENPIPE);

		CMessageTrash trash(pMsg);
		if( pMsg->IsFailed() )throw new CRemoteException( pMsg->GetResult() );
	}catch( ... ){
		CMessageBase msg;
		msg.Init( NCM_OPENSTREAM, GetAddr(), GetServAddr(), E_OPENSTREAM );
		m_pClntProxy->SendMessage( &msg );
		CMessageTrash trash(WaitForMessage( NCF_ACK|NCM_OPENSTREAM ) );
		closesocket( sock_stream );
		throw;
		return INVALID_SOCKET;
	}

	return sock_stream;
}


HRESULT CClientSession::DoPutFile( const string& strSrcFile, const string& strDstFile )
{
	if( !IsFileExist( strSrcFile.c_str() ) ){
		return E_NOENT;
	}

	//step 1. request passive mode to get the data channel address
	u_int nPasPort = 0;
	HRESULT hRet = DoPassive( nPasPort );
	if( FAILED(hRet) )return hRet;

	//step 2. send the put file command.
	CMessage1Param<char*>* pMsgPut = (CMessage1Param<char*>*)CMessageBase::Alloc( sizeof(CMessageBase)+strDstFile.length()+1 );
	pMsgPut->Init( NCM_PUTFILE, GetAddr(), GetServAddr(), strDstFile.c_str() );
	CMessageTrash trash1(pMsgPut);
	m_pClntProxy->SendMessage( pMsgPut );

	CMessage1Param<int>* pMsgAck = (CMessage1Param<int>*)WaitForMessage( NCF_ACK|NCM_PUTFILE );
	CMessageTrash trash2(pMsgAck);
	if( !pMsgAck->IsSucceeded() )return pMsgAck->GetResult();

	pMsgAck->ntoh( false );
	int nFileMode = pMsgAck->GetParam();

	//step 3. now the server agrees on the file transfer, connect the data channel and send file
	SOCKET sock_stream = OpenStream( nPasPort );
	if( sock_stream==INVALID_SOCKET )return E_OPENSTREAM;

	//open the data stream channel.
/*	CMessageBase msg;
	msg.Init( NCM_OPENSTREAM, GetAddr(), GetServAddr() );
	SendMessage( sock_stream, &msg );
	CMessageBase* pMsg = RecvMessage<CMessageBase>( sock_stream );
	if( pMsg==NULL ){
		closesocket( sock_stream );
		return E_OPENSTREAM;
	}

	CMessageTrash trash3(pMsg);
	if( pMsg->IsFailed() ){
		closesocket( sock_stream );
		return pMsg->GetResult();
	}*/

	//send the file stream
	int nLen = SendFileEx( sock_stream, strSrcFile.c_str(), nFileMode );
	shutdown( sock_stream, SD_BOTH );
	closesocket( sock_stream );

	//step 4. exchange the error code.
	CMessageBase msg;
	msg.Init( NCM_STREAMLENGTH, GetAddr(), GetServAddr(), nLen );
	m_pClntProxy->SendMessage( &msg );

	CMessageBase* pMsg = WaitForMessage( NCF_ACK|NCM_STREAMLENGTH );
	CMessageTrash trash4(pMsg);

	return pMsg->GetResult();
}

HRESULT CClientSession::DoGetFile( const string& strFileSrc, const string& strFileDst )
{
	//step 1. request passive mode to get the data channel address
	u_int nPasPort = 0;
	HRESULT hRet = DoPassive( nPasPort );
	if( FAILED(hRet) )return hRet;

	//step 2. send the put file command.
	CMessage1Param<char*>* pMsgGet = (CMessage1Param<char*>*)CMessageBase::Alloc( sizeof(CMessageBase)+strFileSrc.length()+1 );
	pMsgGet->Init( NCM_GETFILE, GetAddr(), GetServAddr(), strFileSrc.c_str() );
	CMessageTrash trash1(pMsgGet);
	m_pClntProxy->SendMessage( pMsgGet );

	CMessage1Param<int>* pMsgAck = (CMessage1Param<int>*)WaitForMessage( NCF_ACK|NCM_GETFILE );
	CMessageTrash trash2(pMsgAck);
	if( !pMsgAck->IsSucceeded() )return pMsgAck->GetResult();

	pMsgAck->ntoh( false );
	int nFileMode = pMsgAck->GetParam();

	//step 3. now the server agrees on the file transfer, connect the data channel and send file
	SOCKET sock_stream = OpenStream( nPasPort );
	if( sock_stream==INVALID_SOCKET )return E_OPENSTREAM;

	//open the data stream channel.
/*	CMessageBase msg;
	msg.Init( NCM_OPENSTREAM, GetAddr(), GetServAddr() );
	SendMessage( sock_stream, &msg );
	CMessageBase* pMsg = RecvMessage<CMessageBase>( sock_stream );
	if( pMsg==NULL ){
		closesocket( sock_stream );
		return E_OPENSTREAM;
	}

	CMessageTrash trash3(pMsg);
	if( pMsg->IsFailed() ){
		closesocket( sock_stream );
		return pMsg->GetResult();
	}*/

	//send the file stream
	int nLen = RecvFileEx( sock_stream, strFileDst.c_str(), nFileMode );
	shutdown( sock_stream, SD_BOTH );
	closesocket( sock_stream );

	//step 4. exchange the error code.
	CMessageBase msg;
	msg.Init( NCM_STREAMLENGTH, GetAddr(), GetServAddr(), nLen );
	m_pClntProxy->SendMessage( &msg );

	CMessageBase* pMsg = WaitForMessage( NCF_ACK|NCM_STREAMLENGTH );
	CMessageTrash trash4(pMsg);

	return pMsg->GetResult();
}

HRESULT CClientSession::DoRemove( const string& strFileName )
{
	CMessage1Param<char*>* pMsg = (CMessage1Param<char*>*)CMessageBase::Alloc( sizeof(CMessageBase)+strFileName.length()+1 );
	pMsg->Init( NCM_REMOVE, GetAddr(), GetServAddr(), strFileName.c_str() );
	CMessageTrash trash(pMsg);

	m_pClntProxy->SendMessage( pMsg );
	CMessageBase* pAck = WaitForMessage( NCF_ACK|NCM_REMOVE );
	CMessageTrash trash2(pAck);
	return pAck->GetResult();
}

HRESULT CClientSession::DoChMod( const string& strFileName, int nMode )
{
	CMessage1Param<char*>* pMsg = (CMessage1Param<char*>*)CMessageBase::Alloc( sizeof(CMessageBase)+strFileName.length()+1 );
	pMsg->Init( NCM_CHMOD, GetAddr(), GetServAddr(), strFileName.c_str(), nMode );
	CMessageTrash trash(pMsg);

	m_pClntProxy->SendMessage( pMsg );
	CMessageBase* pAck = WaitForMessage( NCF_ACK|NCM_CHMOD );
	CMessageTrash trash2(pAck);
	return pAck->GetResult();
}

HRESULT CClientSession::DoCommand( const string& strCommand, bool bSync, DWORD& dwExitCode )
{
	CMessage1Param<char*>* pMsg = (CMessage1Param<char*>*)CMessageBase::Alloc( sizeof(CMessageBase)+strCommand.length()+1 );
	pMsg->Init( NCM_RUNIMAGE, GetAddr(), GetServAddr(), strCommand.c_str(), bSync );
	CMessageTrash trash1(pMsg);
	m_pClntProxy->SendMessage( pMsg );

	CMessage1Param<int>* pAck = (CMessage1Param<int>*)WaitForMessage( NCF_ACK|NCM_RUNIMAGE );
	CMessageTrash trash2( pAck );
	if( pAck->IsSucceeded() ){
		pAck->ntoh( false );
		dwExitCode = pAck->GetParam();
	}
	return pAck->GetResult();
}

HRESULT CClientSession::DoKillProcess()
{
	m_pClntProxy->KillProcess( GetServAddr() );
	return S_OK;
}

HRESULT CClientSession::DoSoftLink( const string& strSrc, const string& strLnk )
{
	CMessage2Param<char*, char*>* pMsg = (CMessage2Param<char*, char*>*)CMessageBase::Alloc( sizeof(CMessageBase)+strSrc.length()+strLnk.length()+2 );
	pMsg->Init( NCM_SOFTLINK, GetAddr(), GetServAddr(), strSrc.c_str(), strLnk.c_str() );
	CMessageTrash trash(pMsg);

	m_pClntProxy->SendMessage( pMsg );

	CMessageBase* pAck = WaitForMessage( NCF_ACK|NCM_SOFTLINK );
	CMessageTrash trash2( pAck );
	return pAck->GetResult();
}

HRESULT CClientSession::DoServerInfo( SERVERINFO& info )
{
	CMessageBase msg;
	msg.Init( NCM_SERVERINFO, GetAddr(), GetServAddr() );
	m_pClntProxy->SendMessage( &msg );

	CMessage1Param<SERVERINFO>* pAck = (CMessage1Param<SERVERINFO>*)WaitForMessage( NCF_ACK|NCM_SERVERINFO );
	CMessageTrash trash( pAck );
	pAck->ntoh( false );
	info = pAck->GetParam();
	return pAck->GetResult();
}

HRESULT CClientSession::DoFileMode( int nFileMode )
{
	CMessageBase msg;
	msg.Init( NCM_FILEMODE, GetAddr(), GetServAddr(), nFileMode );
	m_pClntProxy->SendMessage( &msg );

	CMessageBase* pAck = WaitForMessage( NCF_ACK|NCM_FILEMODE );
	CMessageTrash trash( pAck );
	return pAck->GetResult();
}

HRESULT CClientSession::DoFileInfo( const string& strFileInfo, NETF_STAT& fstat )
{
	CMessage1Param<char*>* pMsg = (CMessage1Param<char*>*)CMessageBase::Alloc( sizeof(CMessageBase)+strFileInfo.length()+1 );
	pMsg->Init( NCM_FILEINFO, GetAddr(), GetServAddr(), strFileInfo.c_str() );
	CMessageTrash trash1(pMsg);
	m_pClntProxy->SendMessage( pMsg );

	CMessage1Param<NETF_STAT>* pAck = (CMessage1Param<NETF_STAT>*)WaitForMessage( NCF_ACK|NCM_FILEINFO );
	CMessageTrash trash2( pAck );
	if( pAck->IsFailed() )return pAck->GetResult();

	pAck->ntoh( false );
	fstat = pAck->GetParam();
	return pAck->GetResult();
}

HRESULT CClientSession::DoListDir( const string& strPath, EnumNetfEntryProc pEnumProc )
{
	//step 1. request passive mode to get the data channel address
	u_int nPasPort = 0;
	HRESULT hRet = DoPassive( nPasPort );
	if( FAILED(hRet) )return hRet;

	//step 2. send the put file command.
	CMessage1Param<char*>* pMsgList = (CMessage1Param<char*>*)CMessageBase::Alloc( sizeof(CMessageBase)+strPath.length()+1 );
	pMsgList->Init( NCM_LISTDIR, GetAddr(), GetServAddr(), strPath.c_str() );
	CMessageTrash trash1(pMsgList);
	m_pClntProxy->SendMessage( pMsgList );

	CMessageBase* pMsgAck = WaitForMessage( NCF_ACK|NCM_LISTDIR );
	CMessageTrash trash2(pMsgAck);
	if( !pMsgAck->IsSucceeded() )return pMsgAck->GetResult();

	//step 3. now the server agrees on the file transfer, connect the data channel and send file
	SOCKET sock_stream = OpenStream( nPasPort );
	if( sock_stream==INVALID_SOCKET )return E_OPENSTREAM;

	//open the data stream channel.
/*	CMessageBase msg;
	msg.Init( NCM_OPENSTREAM, GetAddr(), GetServAddr() );
	SendMessage( sock_stream, &msg );
	CMessageBase* pMsg = RecvMessage<CMessageBase>( sock_stream );
	if( pMsg==NULL ){
		closesocket( sock_stream );
		return E_OPENSTREAM;
	}

	CMessageTrash trash3(pMsg);
	if( pMsg->IsFailed() ){
		closesocket( sock_stream );
		return pMsg->GetResult();
	}*/

	NETF_ENTRY entry;
	int nLen = 0;
	while( true ){
		if( RecvBuffer( sock_stream, (char*)&entry, sizeof(NETF_ENTRY) )<sizeof(NETF_ENTRY) )break;

		(*pEnumProc)( &entry );
		nLen += sizeof(NETF_ENTRY);
	}
	closesocket( sock_stream );

	//step 4. exchange the error code.
	CMessageBase msg;
	msg.Init( NCM_STREAMLENGTH, GetAddr(), GetServAddr(), nLen );
	m_pClntProxy->SendMessage( &msg );

	CMessageBase* pMsg = WaitForMessage( NCF_ACK|NCM_STREAMLENGTH );
	CMessageTrash trash4(pMsg);

	return pMsg->GetResult();
}

bool CClientSession::CopyTo( const string& strSrc, const string& strDst, int how )
{
	HRESULT hRet = S_OK;

	bool bCopy = false;
	switch( how ){
	case COPY_DONTCARE:
		bCopy = true;
		break;
	case COPY_NO_OVERWRITE:
	case COPY_OLD_OVERWRITE:
		{
			NETF_STAT fs_dst;
			if( FAILED(DoFileInfo(strDst, fs_dst)) ){
				bCopy = true;
				break;
			}
			if( how==COPY_OLD_OVERWRITE ){
				NETF_STAT fs_src;
				if( GetNetfStat( strSrc.c_str(), &fs_src )!=0 )return false;
				if( fs_src.nfs_mtime > fs_dst.nfs_mtime ){
					bCopy=true;
				}
			}
			break;
		}
	default:
		ASSERT(false);
	}
	if( bCopy ){
//		cdump<<"copying now"<<endl;
		return SUCCEEDED( DoPutFile( strSrc, strDst ) );
	}else{
		return true;
	}
}

bool CClientSession::CopyFrom( const string& strSrc, const string& strDst, int how )
{
	HRESULT hRet = S_OK;

	bool bCopy = false;
	switch( how ){
	case COPY_DONTCARE:
		bCopy = true;
		break;
	case COPY_NO_OVERWRITE:
	case COPY_OLD_OVERWRITE:
		{
			NETF_STAT fs_dst;
			if( GetNetfStat(strDst.c_str(), &fs_dst)==0 ){
				bCopy = true;
				break;
			}
			if( how==COPY_OLD_OVERWRITE ){
				NETF_STAT fs_src;
				if( FAILED(DoFileInfo(strSrc, fs_src)) )return false;
				if( fs_src.nfs_mtime > fs_dst.nfs_mtime ){
					bCopy=true;
					break;
				}
			}
		}
	default:
		ASSERT(false);
	}
	if( bCopy ){
		return SUCCEEDED( DoGetFile( strSrc, strDst ) );
	}else{
		return true;
	}
}

bool CClientSession::RemoveDir( const string& strPath, bool bRecursive )
{
	if( !bRecursive )return SUCCEEDED(DoRmDir(strPath));

	if( SUCCEEDED(DoEmptyDir(strPath)) ){
		return SUCCEEDED(DoRmDir(strPath));
	}else{
		return false;
	}
}

bool CClientSession::EmptyDir( const string& strPath )
{	return SUCCEEDED(DoEmptyDir(strPath));	}
bool CClientSession::Logon( const string& strUser, const string& strPassword )
{	return SUCCEEDED(DoLogon(strUser, strPassword)); }
bool CClientSession::RunCommand( const string& strCommand, bool bSync, DWORD& dwExitCode )
{	return SUCCEEDED(DoCommand(strCommand, bSync, dwExitCode)); }

/********************************************************************************************
										CClientProxy
*********************************************************************************************/
bool CClientProxy::OpenChannel( CClientSession* pSession )
{
//	InsertSession( pSession);

	{
		CSockLocker<CClientProxy> locker(this);
		if( m_sock.IsClosed() ){
			//initialize socket.
			SOCKADDR_IN srvAddr;
			bcopy( &m_addrServ, &srvAddr.sin_addr, sizeof(IN_ADDR) );
			srvAddr.sin_family = AF_INET;
			srvAddr.sin_port = htons( NCP_SERVER_PORT );
			int nRet = connect( m_sock, (SOCKADDR*)&srvAddr, sizeof(SOCKADDR_IN) );
			m_sock.SetState( nRet==0 ? PS_OPENED : PS_ERROR );
			//start the client proxy.
			if( m_sock.IsOpened() )StartClientProxy();
		}
	}
	return m_sock.IsOpened();
}


bool CClientProxy::CloseChannel( CClientSession* pSession )
{
//	RemoveSession( pSession );
	return true;
}

void CClientProxy::Shutdown()
{
	m_sock.Close();
}

void CClientProxy::KeepAlive()
{
	CMessageBase msg;
	msg.Init( NCM_KEEPALIVE, GetAddr(), CSessionAddr(GetPeerInAddr(m_sock),0) );
	SendMessage( &msg );
}

void CClientProxy::RouteMessage()
{
	try{
		double tmLastTick = GetMilliTime();
		while( true ){
			TIMEVAL tmout;
			tmout.tv_sec = TMMS_KEEPALIVE/1000;
			tmout.tv_usec = 0;

			fd_set rfds;
			FD_ZERO( &rfds );
			FD_SET( m_sock, &rfds );
			int nRet = Select( m_sock+1, &rfds, NULL, NULL, &tmout );

			double tmNow = GetMilliTime();
			if( nRet==0 || tmNow-tmLastTick>TMMS_KEEPALIVE ){	//time out, send the keep alive message
				KeepAlive();
				tmLastTick = tmNow;
			}
			if( nRet!=0 && FD_ISSET(m_sock, &rfds) ){
				CMessageBase* pMsg = RecvMessage<CMessageBase>( m_sock );
				if( pMsg==NULL )break;	//sock shutdown.
				CClientSession* pSession = GetSession( pMsg->GetDstAddr() );
				if( pSession ){
					pSession->PostMessage( pMsg );
				}else{
					CMessageTrash trash(pMsg);
				}
			}
		}
	}catch( CSockException* e ){
		cout<<"Client Proxy Sock:"<<e->what()<<endl;
		e->Delete();
	}

	ShutdownSessions();
	closesocket( m_sock );
}

int CClientProxy::ClientProxyThread( void* arg )
{
	CClientProxy* pProxy = (CClientProxy*)arg;
	pProxy->RouteMessage();
/*	try{
		while( true ){
			double tmLastTick = GetMilliTime();

			TIMEVAL tmout, *pto;
			if( pServApp->IsProxy() ){
				tmout.tv_sec = 10;
				tmout.tv_usec = 0;
				pto = &tmout;
			}else pto = NULL;

			fd_set rfds;
			FD_ZERO( &rfds );
			FD_SET( socksrv, &rfds );
			int nRet = Select( maxfds, &rfds, NULL, NULL, pto );

			double tmNow = GetMilliTime();

			CMessageBase* pMsg = RecvMessage<CMessageBase>( pProxy->m_sock );
			if( pMsg==NULL )break;	//sock shutdown.
			CClientSession* pSession = pProxy->GetSession( pMsg->GetDstAddr() );
			if( pSession ){
				pSession->PostMessage( pMsg );
			}else{
				CMessageTrash trash(pMsg);
			}
		}
	}catch( CSockException* e ){
		cout<<"Client Proxy Sock:"<<e->what()<<endl;
		e->Delete();
	}

	pProxy->ShutdownSessions();
	closesocket( pProxy->m_sock );*/
	return 0;
}

bool CClientProxy::StartClientProxy()
{
	ASSERT( m_hProxyThread==NULL );
	m_hProxyThread = CreateThread( (THREAD_ROUTINE)ClientProxyThread, this, 0 );
	if( m_hProxyThread==NULL )return false;
	return true;
}

void CClientProxy::WaitForObject()
{
	if( m_hProxyThread==NULL )return;

	WaitForThread( m_hProxyThread );
	CloseHandle( m_hProxyThread );
	m_hProxyThread = NULL;
}


//********************************************************************************************************
class op_addr_host
{
public:
	set< IN_ADDR >&	 m_HostAddrs;
public:
	op_addr_host( set<IN_ADDR>& addr_set ):m_HostAddrs(addr_set){}
	void operator() (IN_ADDR addrHost)
	{
		if( m_HostAddrs.find(addrHost)==m_HostAddrs.end() ){
			m_HostAddrs.insert( addrHost );
		}
	}
};

void CClientManager::LoadHostFile( const char* strNodeFile )
{
	CObjLocker<CClientManager> locker(this);
	m_HostAddrs.clear();
	ParseHostFile( strNodeFile, op_addr_host(m_HostAddrs) );
}

void CClientManager::AddHost( const char* strHost )
{
	m_HostAddrs.insert( GetHostInAddr(strHost) );
}

void CClientManager::AddHost( IN_ADDR addr )
{
	m_HostAddrs.insert( addr );
}


void CClientManager::AddToken( const CSessionAddr& addr, int nCpuType, int nCpuMhz )
{
	CServerToken* pToken = new CServerToken( addr, nCpuType, nCpuMhz );
	m_PeerTokens[ pToken->GetServAddr() ] = pToken;
	m_ServTokens.insert( pToken );
	ReleaseSemaphore( m_hSema );
}


CClientSession* CClientManager::RetrieveSession( int nAlgorithm )
{
	CServerToken* pToken = NULL;
	while( true ){
		WaitForSemaphore( m_hSema );
		{
			CObjLocker<CClientManager> locker(this);
			multiset<CServerToken*, TokenCpuCmp>::iterator pos;
			switch( nAlgorithm ){
			case CMRS_FASTEST:
				pos = m_ServTokens.end();
				advance( pos, -1 );
				break;
			case CMRS_SLOWEST:
				pos = m_ServTokens.begin();
				break;
			}
			ASSERT( pos!=m_ServTokens.end() );
			pToken = *pos;
			m_ServTokens.erase( pos );
			if( pToken->IsDead() ){			//remove the dead token
				m_PeerTokens.erase( pToken->GetServAddr() );
				pToken = NULL;
			}
		}
		if( pToken!=NULL )break;
	}

	pToken->SetBusey();
	CClientSession* pSession = new CClientSession( CSessionAddr(GetLocalInAddr(), GetNextSessionId()), pToken );

	//get the proxy pointer, create the proxy if necessary
	IN_ADDR addrServ = pToken->GetServAddr().m_addrHost;
	CClientProxy* pClntProxy = NULL;
	{
		CObjLocker<CClientManager> locker(this);
		map<IN_ADDR, CClientProxy*>::iterator pos = m_ClntProxies.find( addrServ );
		if( pos!=m_ClntProxies.end() )pClntProxy = pos->second;
		else{
			pClntProxy = new CClientProxy( this, CSessionAddr( GetLocalInAddr(), 0 ), addrServ );
			m_ClntProxies[addrServ] = pClntProxy;
		}
	}
	//establish the relationship of proxy and session
	pClntProxy->InsertSession( pSession );
	pSession->SetProxy( pClntProxy );

	return pSession;
}

void CClientManager::ReturnSession( CClientSession* pSession )
{
	CObjLocker<CClientManager> locker(this);

	CServerToken* pToken = pSession->GetToken();
	CClientProxy* pClntProxy = pSession->GetProxy();

	if( pClntProxy->IsSockError() ){	//mark the dead tokens.
		CSessionAddr addr( pToken->GetServAddr().m_addrHost, 0 );
		map<CSessionAddr, CServerToken*>::iterator pos = m_PeerTokens.lower_bound( addr );
		while( pos!=m_PeerTokens.end() && ( pos->second->GetServAddr().m_addrHost==pToken->GetServAddr().m_addrHost) ){
			pos->second->SetDead();
			++pos;
		}
	}

	//delete the proxy if necessary
	pClntProxy->RemoveSession( pSession );
	IN_ADDR addrServ = pToken->GetServAddr().m_addrHost;
	if( pClntProxy->IsEmpty() ){
		m_ClntProxies.erase( addrServ );
		pClntProxy->Shutdown();
		pClntProxy->WaitForObject();
		delete pClntProxy;
	}

	//either return the token to session tokes or remove the tokens from server tokens.
	if( pToken->IsDead() ){
		m_PeerTokens.erase( pToken->GetServAddr() );
		delete pToken;
	}else{
		pToken->SetIdle();
		m_ServTokens.insert( pToken );
		ReleaseSemaphore( m_hSema );
	}
	delete pSession;
}

//poll all the registered hosts
void CClientManager::DoPoll( SOCKET sock, unsigned short sPort ){
	vector<IN_ADDR> arrAddrs;
	{
		CObjLocker<CClientManager> locker(this);
		arrAddrs.resize( m_HostAddrs.size() );
		copy( m_HostAddrs.begin(), m_HostAddrs.end(), arrAddrs.begin() );
	}
	SOCKADDR_IN dstaddr;
	dstaddr.sin_family = AF_INET;
	dstaddr.sin_port = htons( NCP_SERVER_PING_PORT );

	for( int i=0; i<arrAddrs.size(); i++ ){
		CMessageBase msg(NCM_POLLSERVER);
		msg.Init( NCM_POLLSERVER, CSessionAddr(), CSessionAddr(), sPort );
		dstaddr.sin_addr = arrAddrs[i];
		SendMessageTo( sock, &msg, dstaddr );
	}
}

void CClientManager::OnAckPoll( CMessageBase* pMsg ){
	CObjLocker<CClientManager> locker(this);
	TOKEN_INFO* pinfo = (TOKEN_INFO*)(pMsg+1);

	IN_ADDR addrHost = pMsg->GetSrcAddr().m_addrHost;
	//update the host set
	if( m_HostAddrs.find( addrHost )==m_HostAddrs.end() )m_HostAddrs.insert( addrHost );

	//mark all the existing idle tokens from the host as dead
	map<CSessionAddr, CServerToken*>::iterator pos = m_PeerTokens.lower_bound( CSessionAddr(addrHost,0) );
	while( pos!=m_PeerTokens.end() && ( pos->second->GetServAddr().m_addrHost==addrHost) ){
		if( pos->second->IsIdle() )pos->second->SetDead();
		++pos;
	}

	//reset the tokens to idle
	int nInfos = pMsg->GetResult();
	for( int i=0; i<nInfos; i++ ){
		pinfo[i].ntoh();
		CSessionAddr addrPeer( pMsg->GetSrcAddr().m_addrHost, pinfo[i].nServId );
		pos = m_PeerTokens.find( addrPeer );
		if( pos==m_PeerTokens.end() ){
			AddToken( addrPeer, pinfo[i].nCpuType, pinfo[i].nCpuMhz );
		}else{
			if( pos->second->IsIdle() )pos->second->SetSound();
		}
	}
	cdump<<"get poll back from host:"<<inet_ntoa(addrHost)<<" with "<<nInfos<<" tokens"<<endl;
}

int CClientManager::PollThread( void* arg )
{
	CClientManager* pManager = (CClientManager*)arg;

	//start the poll server and bind to specific port.
	SOCKET socksrv = Socket( PF_INET, SOCK_DGRAM, 0 );

	SOCKADDR_IN srvaddr;
	srvaddr.sin_family = AF_INET;
	srvaddr.sin_port = htons( 0 );
	srvaddr.sin_addr.s_addr = INADDR_ANY;
	Bind( socksrv, (SOCKADDR*)&srvaddr, sizeof(SOCKADDR_IN) );
	socklen_t len = sizeof(SOCKADDR_IN);
	getsockname( socksrv, (SOCKADDR*)&srvaddr, &len );

#ifdef _WIN32
	SetUdpBlock( socksrv );
#endif

	CMessageBase msg;
	msg.Init( NCM_POLLSERVER, CSessionAddr(), CSessionAddr(), ntohs(srvaddr.sin_port) );
	SOCKET sockbroad = Socket( PF_INET, SOCK_DGRAM, 0 );
//	msg.Broadcast( sockbroad, NCP_SERVER_PING_PORT );
	closesocket( sockbroad );
	pManager->DoPoll( socksrv, ntohs(srvaddr.sin_port) );

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
			pManager->DoPoll( socksrv, ntohs(srvaddr.sin_port) );
		}else if( FD_ISSET(socksrv, &rfds) ){
			SOCKADDR_IN srcaddr;
			CMessageBase* pMsg = RecvMessageFrom<CMessageBase>( socksrv, srcaddr );
			if( pMsg==NULL )continue;	//ignore NULL message.

			CMessageTrash trash(pMsg);
			if( pMsg->IsAcking(NCM_POLLSERVER) ){			//the server is acking our poll
				pManager->OnAckPoll( pMsg );
			}else if( pMsg->GetMessage()==NCM_SERVERSTART ){	//the server is starting, poll the server
				CMessageBase msg;
				msg.Init( NCM_POLLSERVER, CSessionAddr(), CSessionAddr(), NCP_SERVER_PING_PORT );
				srcaddr.sin_port = htons( NCP_SERVER_PING_PORT );
				SendMessageTo( socksrv, &msg, srcaddr );
			}
		}
	}//end of while
	closesocket( socksrv );
	return 0;
}

void CClientManager::ShutdownServer( IN_ADDR addrHost, bool bForward )
{
	CSocket sock;
	sock.Socket( SOCK_DGRAM );

	if( addrHost.s_addr==INADDR_ANY ){
		//send shutdown message to all the known host addresses
		CObjLocker<CClientManager> locker(this);
		set<IN_ADDR>::iterator pos;
		for( pos=m_HostAddrs.begin(); pos!=m_HostAddrs.end(); ++pos ){
			CMessageBase msg;
			msg.Init( NCM_SHUTDOWN, CSessionAddr(GetLocalInAddr(), 0), CSessionAddr(*pos, 0), bForward );
			CSockAddrIn dstaddr( *pos, NCP_SERVER_PING_PORT );
			SendMessageTo( sock, &msg, dstaddr );
		}
	}else{
		//send shutdown to a specific addresse
		CMessageBase msg;
		msg.Init( NCM_SHUTDOWN, CSessionAddr(GetLocalInAddr(), 0), CSessionAddr(addrHost, 0), bForward );
		CSockAddrIn dstaddr( addrHost, NCP_SERVER_PING_PORT );
		SendMessageTo( sock, &msg, dstaddr );
	}
}

/*
//log on and log off operations
int CClientSession::DoLogOn( const char* strUser, const char* strPass )
{
	char buf[MAXCMDBUF];
	CNcpMessage* pMsg = (CNcpMessage*)buf;

	pMsg->Init( CM_LOGON, m_nFileMode );
	strcpy( pMsg->data, strUser );
	strcpy( pMsg->data+strlen(strUser)+1, strPass );
	pMsg->SetDataSize( strlen(strUser)+strlen(strPass)+2 );
	pMsg->Send( m_sock );

	//wait for the return code
	if( !pMsg->Recv( m_sock, ELEMENTS(buf) ) ){
		return E_BROKEN;		//broken connection
	}

	ASSERT( pMsg->IsAck() );
	return pMsg->GetRet();
}

int CClientSession::DoLogOff()
{
	char buf[MAXCMDBUF];
	CNcpMessage* pMsg = (CNcpMessage*)buf;

	pMsg->Init( CM_LOGOFF );
	pMsg->Send( m_sock );

	//wait for the return code
	if( !pMsg->Recv( m_sock, ELEMENTS(buf) ) ){
		return E_BROKEN;		//broken connection
	}

	ASSERT( pMsg->IsAck() );
	return pMsg->GetRet();
}

//passive send CM_PASSIVE command, the server should return the passive data channel address
int CClientSession::DoPassive( short* pport )
{
	char buf[MAXCMDBUF];
	CNcpMessage* pMsg = (CNcpMessage*)buf;

	pMsg->Init( CM_PASSIVE );
	pMsg->Send( m_sock );

	//wait for the return code
	if( !pMsg->Recv( m_sock, ELEMENTS(buf) ) ){
		return E_BROKEN;		//broken connection
	}

	ASSERT( pMsg->IsAck() );
	if( pMsg->IsFailed() )return pMsg->GetRet();

	ASSERT( pMsg->GetSize()==sizeof(CNcpMessage)+sizeof(short) );

    //the data area is the port number in network bytes
	*pport = ntohs( *(short*)( pMsg->GetData() ) );
	
	return S_OK;
}

int CClientSession::DoPutFile( const char* strSrc, const char* strDst, bool bOverwrite )
{
	char buf[MAXPATH+sizeof(CNcpMessage)];
	CNcpMessage* pMsg = (CNcpMessage*)buf;

	if( !IsFileExist( strSrc ) ){
		return E_NOENT;
	}

	//step 1. request passive mode to get the data channel address
	short dataport = 0;

	int nRet = DoPassive( &dataport );
	if( FAILED(nRet) )return nRet;

	//step 2. send the put file command.
	pMsg->Init( CM_PUTFILE );
	pMsg->m_nParamRet = bOverwrite;
	strcpy( pMsg->GetData(), strDst );
	pMsg->SetDataSize( strlen(strDst)+1 );

	pMsg->Send( m_sock );

	//wait for the return code and check it
	if( !pMsg->Recv( m_sock, ELEMENTS(buf) ) ){
		return E_BROKEN;		//broken connection
	}

	ASSERT( pMsg->IsAck() );
	if( pMsg->IsFailed() || pMsg->m_nParamRet==S_EXIST ){
		return pMsg->GetRet();
	}

	//step 3. now the server agrees on the file transfer, connect the data channel and send file
	SOCKADDR_IN addr;
	socklen_t nlen = sizeof(SOCKADDR_IN);
	GetPeerName( m_sock, (SOCKADDR*)&addr, &nlen );
	addr.sin_port = htons( dataport );

	SOCKET sockdata;
	//import, must retry the socket initilization a few times.
	int i;
	for( i=0; i<MAXRETRY; i++ ){
		sockdata = Socket( PF_INET, SOCK_STREAM, 0 );
		ASSERT( sockdata!=INVALID_SOCKET );

		if( ::connect( sockdata, (SOCKADDR*)&addr, sizeof(SOCKADDR_IN) )==0 )break;
		closesocket( sockdata );
	}
	if( i>=MAXRETRY )throw new CSockException();

	int nLen = SendFileEx( sockdata, strSrc, m_nFileMode );
	closesocket( sockdata );

	//step 4. exchange the error code.
	pMsg->Init( CM_ACK, nLen );
	pMsg->Send( m_sock );

	//wait for the return code and check it
	if( !pMsg->Recv( m_sock, ELEMENTS(buf) ) ){
		return E_BROKEN;		//broken connection
	}

	ASSERT( pMsg->IsAck() );
	return pMsg->GetRet();
}

int CClientSession::DoGetFile( const char* strSrc, const char* strDst )
{
	char buf[MAXPATH+sizeof(CNcpMessage)];
	CNcpMessage* pMsg = (CNcpMessage*)buf;

	//step 1. request passive mode to get the data channel address
	short dataport=0;

	int nRet = DoPassive( &dataport );
	if( FAILED(nRet) )return nRet;

	//step 2. send the put file command.
	pMsg->Init( CM_GETFILE );
	strcpy( pMsg->GetData(), strSrc );
	pMsg->SetDataSize( strlen(strSrc)+1 );
	pMsg->Send( m_sock );

	//wait for the return code and check it
	if( !pMsg->Recv( m_sock, ELEMENTS(buf) ) ){
		return E_BROKEN;		//broken connection
	}

	ASSERT( pMsg->IsAck() );
	if( pMsg->IsFailed() ){
		return pMsg->GetRet();
	}

	//step 3. now the server agrees on the file transfer, connect the data channel and send file
	SOCKADDR_IN addr;
	socklen_t nlen = sizeof(SOCKADDR_IN);
	GetPeerName( m_sock, (SOCKADDR*)&addr, &nlen );
	addr.sin_port = htons( dataport );

	SOCKET sockdata;
	//import, must retry the socket initilization a few times. 
	int i;
	for( i=0; i<MAXRETRY; i++ ){
		sockdata = Socket( PF_INET, SOCK_STREAM, 0 );
		ASSERT( sockdata!=INVALID_SOCKET );

		if( ::connect( sockdata, (SOCKADDR*)&addr, sizeof(SOCKADDR_IN) )==0 )break;
		closesocket( sockdata );
	}
	if( i>=MAXRETRY )throw new CSockException();

	int nLen = RecvFileEx( sockdata, strDst, m_nFileMode );
	closesocket( sockdata );

	//step 4. exchange the error code.
	pMsg->Init( CM_ACK, nLen );
	pMsg->Send( m_sock );

	//wait for the return code and check it
	if( !pMsg->Recv( m_sock, ELEMENTS(buf) ) ){
		return E_BROKEN;		//broken connection
	}

	ASSERT( pMsg->IsAck() );
	return pMsg->GetRet();
}

int CClientSession::DoRemFile( const char* strName )
{
	char buf[MAXCMDBUF];
	CNcpMessage* pMsg = (CNcpMessage*)buf;

	pMsg->Init( CM_REMFILE );
	strcpy( pMsg->GetData(), strName );
	pMsg->SetDataSize( strlen(strName)+1 );
	pMsg->Send( m_sock );

	//wait for the return code and check it
	if( !pMsg->Recv( m_sock, ELEMENTS(buf) ) ){
		return E_BROKEN;		//broken connection
	}

	ASSERT( pMsg->IsAck() );
	return pMsg->GetRet();
}

//chmod of a remote file, pMsg->m_nParamRet is the nMode parameters, pMsg->data is the file name
int CClientSession::DoChMod( const char* strName, int nMode )
{
	char buf[MAXCMDBUF];
	CNcpMessage* pMsg = (CNcpMessage*)buf;

	pMsg->Init( CM_CHMOD, nMode );
	strcpy( pMsg->GetData(), strName );
	pMsg->SetDataSize( strlen(strName)+1 );
	pMsg->Send( m_sock );

	//wait for the return code and check it
	if( !pMsg->Recv( m_sock, ELEMENTS(buf) ) ){
		return E_BROKEN;		//broken connection
	}

	ASSERT( pMsg->IsAck() );
	return pMsg->GetRet();
}

int CClientSession::DoSoftLink( const char* strSrc, const char* strLink )
{
	char buf[2*MAXPATH+sizeof(CNcpMessage)];
	CNcpMessage* pMsg = (CNcpMessage*)buf;

	pMsg->Init( CM_SOFTLINK );
	strcpy( pMsg->data, strSrc );
	strcpy( pMsg->data+strlen(strSrc)+1, strLink );
	pMsg->SetDataSize( strlen(strSrc)+strlen(strLink)+2 );
	pMsg->Send( m_sock );

	//wait for the return code and check it
	if( !pMsg->Recv( m_sock, ELEMENTS(buf) ) ){
		return E_BROKEN;		//broken connection
	}

	ASSERT( pMsg->IsAck() );
	return pMsg->GetRet();
}

int CClientSession::DoFileMode( int nFileMode )
{
	char buf[MAXCMDBUF];
	CNcpMessage* pMsg = (CNcpMessage*)buf;

	pMsg->Init( CM_FILEMODE, nFileMode );
	pMsg->Send( m_sock );

	//wait for the return code and check it
	if( !pMsg->Recv( m_sock, ELEMENTS(buf) ) ){
		return E_BROKEN;		//broken connection
	}

	ASSERT( pMsg->IsAck() );
	if( pMsg->IsSucceeded() ){
		m_nFileMode = nFileMode;
	}
	return pMsg->GetRet();
}

int CClientSession::DoMkDir( const char* strDir )
{
	char buf[MAXCMDBUF];
	CNcpMessage* pMsg = (CNcpMessage*)buf;

	pMsg->Init( CM_MKDIR );
	strcpy( pMsg->GetData(), strDir );
	pMsg->SetDataSize( strlen(strDir)+1 );
	pMsg->Send( m_sock );

	//wait for the return code and check it
	if( !pMsg->Recv( m_sock, ELEMENTS(buf) ) ){
		return E_BROKEN;		//broken connection
	}

	ASSERT( pMsg->IsAck() );
	return pMsg->GetRet();
}

int CClientSession::DoChDir( const char* strDir )
{
	char buf[MAXCMDBUF];
	CNcpMessage* pMsg = (CNcpMessage*)buf;

	pMsg->Init( CM_CHDIR );
	strcpy( pMsg->GetData(), strDir );
	pMsg->SetDataSize( strlen(strDir)+1 );
	pMsg->Send( m_sock );

	//wait for the return code and check it
	if( !pMsg->Recv( m_sock, ELEMENTS(buf) ) ){
		return E_BROKEN;		//broken connection
	}

	ASSERT( pMsg->IsAck() );
	return pMsg->GetRet();
}

int CClientSession::DoRmDir( const char* strDir )
{
	char buf[MAXCMDBUF];
	CNcpMessage* pMsg = (CNcpMessage*)buf;

	pMsg->Init( CM_RMDIR );
	strcpy( pMsg->GetData(), strDir );
	pMsg->SetDataSize( strlen(strDir)+1 );
	pMsg->Send( m_sock );

	//wait for the return code and check it
	if( !pMsg->Recv( m_sock, ELEMENTS(buf) ) ){
		return E_BROKEN;		//broken connection
	}

	ASSERT( pMsg->IsAck() );
	return pMsg->GetRet();
}

int CClientSession::DoRunImage( const char* strImage, bool bSync )
{
	char buf[MAXPATH+sizeof(CNcpMessage)];
	CNcpMessage* pMsg = (CNcpMessage*)buf;

	pMsg->Init( CM_RUNIMAGE );

	//bSync is in the nParamRet
	pMsg->m_nParamRet = (int)bSync;

	//then is the image name.
	strcpy( pMsg->GetData(), strImage );

	pMsg->SetDataSize( strlen(strImage)+1 );
	pMsg->Send( m_sock );

	//wait for the return code and check it
	if( !pMsg->Recv( m_sock, ELEMENTS(buf) ) ){
		return E_BROKEN;		//broken connection
	}

	ASSERT( pMsg->IsAck() );
	return pMsg->GetRet();
}

int CClientSession::DoGetSysInfo( SYSINFO* pSysInfo )
{
	char buf[MAXPATH+sizeof(CNcpMessage)];
	CNcpMessage* pMsg = (CNcpMessage*)buf;

	pMsg->Init( CM_GETSYSINFO );
	pMsg->Send( m_sock );

	//wait for the return code and check it
	if( !pMsg->Recv( m_sock, ELEMENTS(buf) ) ){
		return E_BROKEN;		//broken connection
	}

	ASSERT( pMsg->IsAck() );
	if( pMsg->IsSucceeded() ){
		bcopy( pMsg->GetData(), pSysInfo, sizeof(SYSINFO) );
		sysinfo_ntoh( pSysInfo );
	}

	return pMsg->GetRet();
}

int CClientSession::DoKillProcs( )
{
	char buf[MAXPATH+sizeof(CNcpMessage)];
	CNcpMessage* pMsg = (CNcpMessage*)buf;

	pMsg->Init( CM_KILLPROCS );
	pMsg->Send( m_sock );

	//wait for the return code and check it
	if( !pMsg->Recv( m_sock, ELEMENTS(buf) ) ){
		return E_BROKEN;		//broken connection
	}

	ASSERT( pMsg->IsAck() );
	return pMsg->GetRet();
}

int CClientSession::DoSetIdle( BOOL bIdle )
{
	char buf[MAXPATH+sizeof(CNcpMessage)];
	CNcpMessage* pMsg = (CNcpMessage*)buf;

	pMsg->Init( CM_SETIDLE, bIdle );
	pMsg->Send( m_sock );

	//wait for the return code and check it
	if( !pMsg->Recv( m_sock, ELEMENTS(buf) ) ){
		return E_BROKEN;		//broken connection
	}

	ASSERT( pMsg->IsAck() );
	return pMsg->GetRet();
}*/

//publiced user functions .............................................................
/*CClientSession::CClientSession( )
{
	m_sock = 0;
//	m_nFileMode = O_BINARY;
}

CClientSession::~CClientSession()
{
	if( m_sock )closesocket( m_sock );
}

int CClientSession::Connect( SOCKADDR_IN* srvAddr, const char* strUser, const char* strPass )
{
	ASSERT( m_sock==0 );
	int nRet = 0;

	//initialize socket.
	m_sock = Socket( PF_INET, SOCK_STREAM, 0 );

	u_long nonblock = 1;
	nRet = ioctlsocket( m_sock, FIONBIO, &nonblock );

	nRet = connect( m_sock, (SOCKADDR*)srvAddr, sizeof(SOCKADDR_IN) );

	int maxfds;
	fd_set wfds;
	fd_set efds;

	FD_ZERO( &wfds );
	FD_ZERO( &efds );
	FD_SET( m_sock, &wfds );
	FD_SET( m_sock, &efds );
	Select( m_sock+1, NULL, &wfds, &efds, NULL );

	if( FD_ISSET(m_sock, &wfds) ){
		nonblock = 0;
		nRet = ioctlsocket( m_sock, FIONBIO, &nonblock );
	}
	if( FD_ISSET(m_sock, &efds) ){
		return 0;
	}

	//make the connection.
	//nRet = ::Connect( m_sock, (SOCKADDR*)srvAddr, sizeof(SOCKADDR_IN) );

	CMessageHead msg;
	msg.Init( NCM_OPENSESSION, GetAddr(), CSessionAddr(srvAddr->sin_addr, 0) );
	msg.Send( m_sock );

	m_addr.m_nServId = 2;
	msg.Init( NCM_OPENSESSION, GetAddr(), CSessionAddr(srvAddr->sin_addr, 0) );
	msg.Send( m_sock );

	m_addr.m_nServId = 3;
	msg.Init( NCM_OPENSESSION, GetAddr(), CSessionAddr(srvAddr->sin_addr, 0) );
	msg.Send( m_sock );

	m_addr.m_nServId = 4;
	msg.Init( NCM_OPENSESSION, GetAddr(), CSessionAddr(srvAddr->sin_addr, 0) );
	msg.Send( m_sock );

	CMessage1Param<CSessionAddr>* pMsg = (CMessage1Param<CSessionAddr>*)RecvMessage( m_sock );
	ASSERT( pMsg->GetMessageHead()->IsAcking() );
	pMsg->ntoh();
	m_addrServ = pMsg->GetParam();

	cout<<GetHostByAddr( m_addrServ.m_addrHost )<<endl;

	pMsg = (CMessage1Param<CSessionAddr>*)RecvMessage( m_sock );
	ASSERT( pMsg->GetMessageHead()->IsAcking() );
	pMsg->ntoh();
	m_addrServ = pMsg->GetParam();

	cout<<GetHostByAddr( m_addrServ.m_addrHost )<<endl;

	pMsg = (CMessage1Param<CSessionAddr>*)RecvMessage( m_sock );
	ASSERT( pMsg->GetMessageHead()->IsAcking() );
	pMsg->ntoh();
	m_addrServ = pMsg->GetParam();

	cout<<GetHostByAddr( m_addrServ.m_addrHost )<<endl;

	pMsg = (CMessage1Param<CSessionAddr>*)RecvMessage( m_sock );
	ASSERT( pMsg->GetMessageHead()->IsAcking() );
	pMsg->ntoh();
	m_addrServ = pMsg->GetParam();

	cout<<GetHostByAddr( m_addrServ.m_addrHost )<<endl;

	m_addr.m_nServId = 1;
	m_addrServ.m_nServId = 1;
	msg.Init( NCM_CLOSESESSION, GetAddr(), GetServAddr() );
	msg.Send( m_sock );

	m_addr.m_nServId = 2;
	m_addrServ.m_nServId = 2;
	msg.Init( NCM_CLOSESESSION, GetAddr(), GetServAddr() );
	msg.Send( m_sock );

	m_addr.m_nServId = 3;
	m_addrServ.m_nServId = 3;
	msg.Init( NCM_CLOSESESSION, GetAddr(), GetServAddr() );
	msg.Send( m_sock );

	m_addr.m_nServId = 4;
	m_addrServ.m_nServId = 4;
	msg.Init( NCM_CLOSESESSION, GetAddr(), GetServAddr() );
	msg.Send( m_sock );


	pMsg = (CMessage1Param<CSessionAddr>*)RecvMessage( m_sock );
	ASSERT( pMsg->GetMessageHead()->IsAcking() );
	pMsg->ntoh();
	m_addrServ = pMsg->GetParam();

	pMsg = (CMessage1Param<CSessionAddr>*)RecvMessage( m_sock );
	ASSERT( pMsg->GetMessageHead()->IsAcking() );
	pMsg->ntoh();
	m_addrServ = pMsg->GetParam();

	pMsg = (CMessage1Param<CSessionAddr>*)RecvMessage( m_sock );
	ASSERT( pMsg->GetMessageHead()->IsAcking() );
	pMsg->ntoh();
	m_addrServ = pMsg->GetParam();


	return 0;

//	if( nRet!=0 )return E_NOHOST;

//	return DoLogOn( strUser, strPass );
}

int CClientSession::Disconnect( )
{
	ASSERT( m_sock!=0 );


	CMessageHead msg;
	msg.Init( NCM_CLOSESESSION, GetAddr(), GetServAddr() );
	msg.Send( m_sock );

	CMessageHead* pMsg = RecvMessage( m_sock );

	closesocket( m_sock );
	m_sock = 0;

	return 0;
}

void CClientSession::KillConnect()
{
	if( m_sock!=0 )closesocket( m_sock );
	m_sock = 0;
}*/


//file operation
/*int CClientSession::PutFile( const char* strSrc, const char* strDst, bool bOverwrite )
{
	return DoPutFile( strSrc, strDst, bOverwrite );
}

int CClientSession::GetFile( const char* strSrc, const char* strDst )
{
	return DoGetFile( strSrc, strDst );
}

int CClientSession::Remove( const char* strName )
{
	return DoRemFile( strName );
}

//create a link linking to strSrc.
int CClientSession::SoftLink( const char* strSrc, const char* strLink )
{
	return DoSoftLink( strSrc, strLink );
}

int CClientSession::FileMode( int nFileMode )
{
	if( m_nFileMode!=nFileMode ){
		return DoFileMode( nFileMode );
	}
	return S_OK;
}

int CClientSession::ChMod( const char* strName, int nMode )
{
	return DoChMod( strName, nMode );
}

//directory operation
int CClientSession::ChDir( const char* strDir )
{
	return DoChDir( strDir );
}

int CClientSession::MkDir( const char* strDir )
{
	return DoMkDir( strDir );
}

int CClientSession::RmDir( const char* strDir )
{
	return DoRmDir( strDir );
}

//run executable.
int CClientSession::RunImage( const char* strImage, bool bSync )
{
	return DoRunImage( strImage, bSync );
}


//system information
int CClientSession::GetSysInfo( SYSINFO* pSysInfo )
{
	return DoGetSysInfo( pSysInfo );
}

int CClientSession::KillProcs()
{
	return DoKillProcs();
}

int CClientSession::SetIdle( BOOL bIdle )
{
	return DoSetIdle( bIdle );
}*/
