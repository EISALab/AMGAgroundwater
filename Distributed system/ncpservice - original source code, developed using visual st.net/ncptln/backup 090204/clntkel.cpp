#include "stdafx.h"
#include "clntkel.h"

#define MAXFILEBUF		0x8000
#define MAXCMDBUF		512
#define MAXPATH			256
#define MAXLINE			256
#define MAXRETRY		5

extern FILE* pfout;


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

/*int CClientSession::DoPutFile( const char* strSrc, const char* strDst, bool bOverwrite )
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
CClientSession::CClientSession( )
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
	msg.Init( CM_OPENSESSION, 1, -1 );
	msg.Send( m_sock );

	CMessageHead* pMsg = RecvMessage( m_sock );
	ASSERT( pMsg->IsAcking() );

	COneIntMessage* pOneMsg = (COneIntMessage*)pMsg;
	pOneMsg->ntoh();
	m_nServId = pOneMsg->GetInt();

	return 0;

//	if( nRet!=0 )return E_NOHOST;

//	return DoLogOn( strUser, strPass );
}

int CClientSession::Disconnect( )
{
	ASSERT( m_sock!=0 );


	CMessageHead msg;
	msg.Init( CM_CLOSESESSION, 1, m_nServId );
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
}


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
