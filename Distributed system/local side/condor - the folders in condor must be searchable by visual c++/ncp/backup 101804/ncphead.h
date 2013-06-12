#pragma once

#include "netbase.h"
#include "uwkelobj.h"

//compitable head files for win32, unix and linux.
#ifdef _WIN32
#include <direct.h>
#endif

#include <string>
#include <iostream>

#ifndef _WIN32
#define S_OK				0
#define SUCCEEDED(status)	((int)(status) >= 0)
#define FAILED(status)		((int)(status) < 0)
#define HRESULT				unsigned int
#endif

#define	NCP_SERVER_PORT			4400
#define NCP_SERVER_PING_PORT	5500
#define NCP_HOST_REGIST_PORT	2600

#define NCP_PING_SERVER			0x20
#define	NCP_HOST_REGIST			0x10


//ncp version and protocol definition.
#define NCP_VERSION			0x10
#define NCP_PROTOCOL		('N'+'C'+'P')


//general error code returned by network calls
#define	E_NOHOST			0x80000002			//the host is unknown
#define E_BROKEN			0x80000001			//the connection is broken
#define E_UNKNOWN			0x8fffffff			//unknown error

#define E_EXIST				0x80000100			//the file or diretory is already exist
#define S_EXIST				0x00000100			

#define E_NOENT				0x80000200			//the file or directory is not exist
#define S_NOENT				0x00000200		

#define E_NOSPC				0x80010000			//no enough space on the disk
#define E_NOEXEC			0x80020000			//the executalbe format error
#define E_IMGEXIT			0x80020001			//the executable gives failing exit code

#define E_NOSUPT			0x80030000			//the system doesn't support the command or service.
#define E_ACCES				0x80040000			//no such permission
#define E_NOCHN				0x80050000			//data channel is not created

#define E_NOUSER			0x80060000			//no such a user
#define	E_NOPASS			0x80070000			//invalid password

#define E_STREAMLENGTH		0x80080000			//the sender and receiver got different length for the stream.
#define E_OPENSTREAM		0x80090000			//can't get the stream channel for data transmission

#define E_BROKENPIPE		0x80080000			//message can't reach destination because client server connection is broken

#define E_IMPLEMENT			0x81000000			//the command is not implemented

/*************************************************************************************************
									ncp command
**************************************************************************************************/

//logon logoff
#define NCM_LOGON			1
#define NCM_PASSIVE			5				//request a passive data channel
											//return the IN_ADDR + port number.

//file operation commands
#define NCM_STREAMLENGTH	102				//exchange the stream length.

#define NCM_PUTFILE			10				//upload file to the server
											//data:			filename
											//S_EXIST		file is already exist
											//E_NOCHN		if passive is not issued. (no data channel)
											//E_NOSPC		no space on the disk

#define NCM_GETFILE			11				//download file from the server
											//data:			filename
											//E_NOCHN		if passive is not issued. (no data channel)
											//E_NOENT		file is not exists

#define NCM_REMOVE			12				//remove a file from the server
											//data:			filename
											//S_NOENT		file is not there

#define NCM_SOFTLINK		14				//create a soft link for a file for unix system
											//data:			filename
											//E_NOSUP

#define	NCM_CHMOD			16				//change the rw mode of a file

#define NCM_FILEMODE		17				//change the file transfer mode to ascii or binary
											//data: O_TEXT, O_BINARY

//execution commands
#define NCM_RUNIMAGE		20				//run an executalbe image file on the server
											//E_NOENT		file is not exist
											//E_NOEXEC		executable format error

//directory operation commands
#define NCM_MKDIR			30				//create a directory on the server
											//S_EXIST		the directory is already there
											
#define NCM_CHDIR			31				//change the current directory on the server
											//E_NOENT		no such file or directory

#define NCM_RMDIR			32				//remove a directory on the server
											//E_NOENT		no such a directory
											//E_EXIST		there are files in the directory
#define NCM_EMPTYDIR		33				//empty but not remove a directory on the server

#define NCM_FILEINFO		1005			//get file information.
#define NCM_LISTDIR			1006			//list dir

//query system command
#define NCM_SERVERINFO		40				//get server system information
#define CM_KILLPROCS		41				//kill all the waiting processes	//only root can do it.
#define CM_SETIDLE			42				//set the system state to idle. TRUE--idle, FALSE--busy


#define CM_CLEARALL			13

#define CM_RUNSYSCMD		21

#define CM_RESETSERVICE		100
#define CM_SETSERVICESTAT	101


#define NCM_POLLSERVER		4000
#define NCM_SERVERSTART		4001

#define NCM_OPENSESSION		1000
#define NCM_CLOSESESSION	1001
#define NCM_OPENSTREAM		1200
//#define NCM_PASSIVE			1201
#define NCF_ACK				0x80000000
#define NCF_OOB				0x40000000

//OOB messages
#define NCM_KILLPROCESS		(NCF_OOB|2000)
#define NCM_PROCESSEXIT		(NCF_OOB|2001)
#define NCM_PROCPRIORITY	(NCF_OOB|2002)
#define NCM_SERVERSHUTDOWN	(NCF_OOB|2003)
#define NCM_PROXYSHUTDOWN	(NCF_OOB|2004)
#define NCM_QUIT			(NCF_OOB|2005)
#define NCM_KEEPALIVE		(NCF_OOB|2006)

#define TMMS_KEEPALIVE		(60*1000)		//keep server alive message is sent every one minute


template< typename T >
void hton( T& t )
{ t.hton(); }

template< typename T >
void ntoh( T& t )
{ t.ntoh(); }

inline void hton( int& t )
{ t = htonl(t); }
inline void ntoh( int& t )
{ t = ntohl(t); }

inline void ntoh( u_int& t )
{ t = ntohl(t); }
inline void hton( u_int& t )
{ t = htonl(t); }


inline void hton( short& t )
{ t = htons( t ); }
inline void ntoh( short& t )
{ t = ntohs( t ); }

inline void ntoh( u_short& t )
{ t = ntohs(t); }
inline void hton( u_short& t )
{ t = htons(t); }

class CSessionAddr
{
public:
	IN_ADDR	m_addrHost;
	int		m_nServId;
public:
	CSessionAddr() : m_nServId(0){ m_addrHost.s_addr=0; }
	CSessionAddr( IN_ADDR addrHost, int nServId ) : m_addrHost(addrHost), m_nServId(nServId){}
	CSessionAddr( const CSessionAddr& other ) : m_addrHost(other.m_addrHost), m_nServId(other.m_nServId){};
	bool operator < ( const CSessionAddr& other )const {
		if( m_addrHost.s_addr<other.m_addrHost.s_addr )return true;
		else if( m_addrHost.s_addr==other.m_addrHost.s_addr )return m_nServId < other.m_nServId;
		else return false;
	}
	bool operator == ( const CSessionAddr& other )const {
		return m_addrHost.s_addr == other.m_addrHost.s_addr && m_nServId == other.m_nServId;
	}
	const CSessionAddr& operator = (const CSessionAddr& other )
	{
		m_addrHost = other.m_addrHost;
		m_nServId = other.m_nServId;
		return *this;
	}
	void hton(){
		m_nServId = htonl(m_nServId);
	}
	void ntoh(){
		m_nServId = ntohl( m_nServId );
	}
};

template <typename T>
class CSockLocker
{
private:
	T *		m_pSockClass;
public:
	CSockLocker( T* pSockClass ) : m_pSockClass( pSockClass ){ m_pSockClass->LockSock(); }
	~CSockLocker(){ m_pSockClass->UnlockSock(); }
};

template <typename T>
class CObjLocker
{
private:
	T*		m_pLockObj;
public:
	CObjLocker( T* pLockObj ) : m_pLockObj( pLockObj ){ m_pLockObj->Lock(); }
	~CObjLocker(){ m_pLockObj->Unlock(); }
};

#define E_UNEXPECTED_MSG		1000		//server is waiting for a specific message, but wrong message arrived

class CRemoteException : public exception
{
public:
	int m_nErrorCode;				//the error code that caused the excepton
	string m_strError;
public:
	CRemoteException( int nErrorCode ):m_nErrorCode(nErrorCode){
		char buf[256];
		sprintf( buf, "Remote execution exception:%x", nErrorCode );
		m_strError = buf;
	}
	CRemoteException( int nErrorCode, const char* pszErrMsg ):m_nErrorCode(nErrorCode){
		m_strError = pszErrMsg;
	}
	virtual ~CRemoteException() throw(){}

	int GetErrorCode(){ return m_nErrorCode; }

	virtual const char* what(){
		return m_strError.c_str();
	}
	void Delete(){ 
//		cdump<<"delete remote exception:"<<(void*)this<<endl;
		delete this; 
	}
};


#define	PS_CLOSED		0
#define PS_OPENED		1
#define PS_ERROR		-1

class CProxySocket
{
public:
	SOCKET	m_sock;
	int		m_nState;
	int		m_nReference;
public:
	CProxySocket(){
		//initialize socket.
		m_sock = Socket( PF_INET, SOCK_STREAM, 0 );
		m_nState = PS_CLOSED;
		m_nReference = 0;
	}
	void Close(){
		if( m_sock ){
			shutdown( m_sock, SD_BOTH );
			closesocket(m_sock);
		}
		m_sock=0;
		m_nState = PS_CLOSED;
	}
	//property.
	operator SOCKET(){
		return m_sock;
	}
	bool IsClosed(){ return m_nState==PS_CLOSED; }
	bool IsOpened(){ return m_nState==PS_OPENED; }
	bool IsError(){ return m_nState==PS_ERROR; }
	void SetState( int nState ){ m_nState=nState; }

	//reference counting.
	int AddRef(){
		return ++m_nReference;
	}
	int Release(){
		if( m_nReference==1 ){
//			cdump<<"delete proxy sock:"<<(void*)this<<endl;
			delete this;
			return 0;
		}
		return --m_nReference;
	}
	~CProxySocket(){
		Close();
	}
};


#pragma pack(1)

class CMessageBase{
public:
	unsigned short	m_sProtocol;
	unsigned short	m_sVersion;				//protocol and version
	unsigned int	m_nLength;				//size of the message package
	CSessionAddr	m_addrSrc;				//source address of the message
	CSessionAddr	m_addrDst;				//destionation address of the message
	unsigned int	m_nServerStatus;		//server status, a free ride from server
	unsigned int	m_nMessage;				//command of the message
	unsigned int	m_nResult;				//HRESULT for function returns
public:
	CMessageBase() : m_sProtocol(NCP_PROTOCOL), m_sVersion(NCP_VERSION), m_nLength(sizeof(CMessageBase)){}
	CMessageBase( unsigned int nCommand ): m_sProtocol(NCP_PROTOCOL), m_sVersion(NCP_VERSION), m_nLength(sizeof(CMessageBase)), m_nMessage(nCommand){}
	void Init()
	{
		bzero( this, sizeof(CMessageBase) );
		m_sProtocol = NCP_PROTOCOL;
		m_sVersion = NCP_VERSION;
		m_nLength = sizeof(CMessageBase);
	}
	void Init( unsigned int nCmd ){
		Init();
		m_nMessage = nCmd;
	}
	void Init( unsigned int nCmd, const CSessionAddr& addrSrc, const CSessionAddr& addrDst, HRESULT nRet=S_OK ){
		Init();
		m_nMessage = nCmd;
		m_nResult = nRet;
		m_addrSrc = addrSrc;
		m_addrDst = addrDst;
	}
	void SetLength( int nLen ){
		m_nLength = (unsigned int) nLen;
	}
	int GetLength(){
		return (int)m_nLength;
	}
	void SetMessage( unsigned int nMsg ){
		m_nMessage = nMsg;
	}
	unsigned int GetMessage(){
		return m_nMessage;
	}
	void SetCommand( unsigned int nCmd ){
		m_nMessage = nCmd;
	}
	unsigned int GetCommand(){
		unsigned int nCmd = m_nMessage;
		clrbit( nCmd, NCF_ACK );
		return nCmd;
	}
	const CSessionAddr& GetSrcAddr(){
		return m_addrSrc;
	}
	void SetSrcAddr( const CSessionAddr& addrSrc ){
		m_addrSrc = addrSrc;
	}
	const CSessionAddr& GetDstAddr(){
		return m_addrDst;
	}
	void SetDstAddr( const CSessionAddr& addrDst ){
		m_addrDst = addrDst;
	}
	char* GetData(){
		return (char*)this + sizeof(CMessageBase);
	}
	void SetAcking( bool bAck ){
		if( bAck )setbit(m_nMessage, NCF_ACK);
		else clrbit(m_nMessage, NCF_ACK);
	}
	bool IsQuitting(){ return GetCommand()==NCM_QUIT; }
	bool IsAcking(){
		return isset( m_nMessage, NCF_ACK );
	}
	bool IsAcking( int nCmd ){
		return GetMessage()==(NCF_ACK|nCmd);
	}
	bool IsOutband(){ return isset(m_nMessage, NCF_OOB); }
	CMessageBase* Clone(){
		CMessageBase* pNewMsg = (CMessageBase*) new char[GetLength()];
		bcopy( this, pNewMsg, GetLength() );
		return pNewMsg;
	}
	void CopyTo( char* buf ){
		bcopy( this, buf, GetLength() );
	}
	void Destroy(){
//		cdump<<"delete msg:"<<(void*)this<<endl;
		delete [] (char*)this;
	}

	bool IsSucceeded(){
		return SUCCEEDED( m_nResult );
	}
	bool IsFailed(){
		return FAILED( m_nResult );
	}
	void SetResult( HRESULT nResult ){
		m_nResult = nResult;
	}
	HRESULT GetResult(){
		return (HRESULT)m_nResult;
	}
	static CMessageBase* Alloc(size_t size){
		return (CMessageBase*)::new char[size];
	}
	static void Dealloc( CMessageBase* pMsg ){
//		cdump<<"delete msg:"<<(void*)pMsg<<endl;
		::delete[] (char*)pMsg;
	}
	void* operator new(size_t size){
		return ::new char[size];
	}
	void operator delete(void* p){
//		cdump<<"delete msg:"<<(void*)p<<endl;
		::delete[] (char*)p;
	}

	void ntoh(){
		m_sProtocol		= ntohs( m_sProtocol );
		m_sVersion		= ntohs( m_sVersion );
		m_nLength		= ntohl( m_nLength );
		m_nServerStatus	= ntohl( m_nServerStatus );
		m_nMessage		= ntohl( m_nMessage );
		m_nResult		= ntohl( m_nResult );
		m_addrSrc.ntoh();
		m_addrDst.ntoh();

	}
	void hton(){
		m_sProtocol		= htons( m_sProtocol );
		m_sVersion		= htons( m_sVersion );
		m_nLength		= htonl( m_nLength );
		m_nServerStatus = htonl( m_nServerStatus );
		m_nMessage		= htonl( m_nMessage );
		m_nResult		= htonl( m_nResult );
		m_addrSrc.hton();
		m_addrDst.hton();
	}
};

template <typename T>
class CMessage1Param : public CMessageBase
{
public:
	T				m_Param;
public:
	CMessage1Param(){};
	CMessage1Param( const T& t ) : m_Param(t) {};

	void Init( int nCmd, const CSessionAddr& addrSrc, const CSessionAddr& addrDst, const T& t, HRESULT nRet=S_OK )
	{
		CMessageBase::Init( nCmd, addrSrc, addrDst, nRet );
		SetLength( sizeof(CMessage1Param<T>) );
		m_Param = t;
	}
	const T& GetParam(){ return m_Param; }
	void SetParam( const T& t ){ m_Param = t; }

	void ntoh( bool bInherit=true ){ if( bInherit )CMessageBase::ntoh(); ::ntoh(m_Param); }
	void hton( bool bInherit=true ){ if( bInherit )CMessageBase::ntoh(); ::hton(m_Param); }
};

template<>
class CMessage1Param<char*> : public CMessageBase
{
public:
	void Init( int nCmd, const CSessionAddr& addrSrc, const CSessionAddr& addrDst, const char* str, HRESULT nRet=S_OK )
	{
		CMessageBase::Init( nCmd, addrSrc, addrDst, nRet );
		SetLength( sizeof(CMessageBase)+strlen(str)+1 );
		strcpy( GetData(), str );
	}
	const char* GetParam(){ return (char*)GetData(); }
	void SetParam( const char* str ){ strcpy( (char*)GetData(), str ); }

	void ntoh( bool bInherit=true ){ if( bInherit )CMessageBase::ntoh(); }
	void hton( bool bInherit=true ){ if( bInherit )CMessageBase::hton(); }
};

template <typename T1, typename T2>
class CMessage2Param : public CMessageBase
{
public:
	T1				m_Param1;
	T2				m_Param2;
public:
	CMessage2Param(){};
	CMessage2Param( const T1& t1, const T2& t2 ) : m_Param1(t1), m_Param2(t2) {};

	void Init( int nCmd, const CSessionAddr& addrSrc, const CSessionAddr& addrDst, const T1& t1, const T2& t2, HRESULT nRet=S_OK )
	{
		CMessageBase::Init( nCmd, addrSrc, addrDst, nRet );
		SetLength( sizeof(CMessage2Param<T1, T2>) );
		m_Param1 = t1;
		m_Param2 = t2;
	}
	const T1& GetParam1(){ return m_Param1; }
	void SetParam1( const T1& t ){ m_Param1 = t; }

	const T2& GetParam2(){ return m_Param2; }
	void SetParam2( const T2& t ){ m_Param2 = t; }

	void ntoh( bool bInherit=true ){ if(bInherit)CMessageBase::ntoh(); ::ntoh(m_Param1); ::ntoh(m_Param2); }
	void hton( bool bInherit=true ){ if(bInherit)CMessageBase::hton(); ::hton(m_Param1); ::hton(m_Param2); }
};

template <>
class CMessage2Param<char*, char*> : public CMessageBase
{
public:
	void Init( int nCmd, const CSessionAddr& addrSrc, const CSessionAddr& addrDst, const char* str1, const char* str2, HRESULT nRet=S_OK )
	{
		CMessageBase::Init( nCmd, addrSrc, addrDst, nRet );
		SetLength( sizeof(CMessageBase)+strlen(str1)+strlen(str2)+2 );
		strcpy( GetData(), str1 );
		strcpy( GetData()+strlen(str1)+1, str2 );
	}
	const char* GetParam1(){ return GetData(); }
	const char* GetParam2(){ return GetData()+strlen(GetData())+1; }

	void SetParams( const char* str1, const char* str2 ){
		strcpy( GetData(), str1 );
		strcpy( GetData()+strlen(str1)+1, str2 );
	}

	void ntoh( bool bInherit=true ){ if(bInherit)CMessageBase::ntoh(); }
	void hton( bool bInherit=true ){ if(bInherit)CMessageBase::hton(); }
};

template <typename T1, typename T2, typename T3>
class CMessage3Param : public CMessageBase
{
public:
	T1				m_Param1;
	T2				m_Param2;
	T3				m_Param3;
public:
	CMessage3Param(){};
	CMessage3Param( const T1& t1, const T2& t2, const T3& t3 ) : m_Param1(t1), m_Param2(t2), m_Param3(t3) {};

	void Init( int nCmd, const CSessionAddr& addrSrc, const CSessionAddr& addrDst, const T1& t1, const T2& t2, const T3& t3, HRESULT nRet=S_OK )
	{
		CMessageBase::Init( nCmd, addrSrc, addrDst, nRet );
		SetLength( sizeof(CMessage3Param<T1, T2, T3>) );
		m_Param1 = t1;
		m_Param2 = t2;
		m_Param3 = t3;
	}
	const T1& GetParam1(){ return m_Param1; }
	void SetParam1( const T1& t ){ m_Param1 = t; }

	const T2& GetParam2(){ return m_Param2; }
	void SetParam2( const T2& t ){ m_Param2 = t; }

	const T3& GetParam3(){ return m_Param3; }
	void SetParam3( const T3& t ){ m_Param3 = t; }

	void ntoh(bool bInherit=true){ if(bInherit)CMessageBase::ntoh(); ::ntoh(m_Param1); ::ntoh(m_Param2); ::ntoh(m_Param3); }
	void hton(bool bInherit=true){ if(bInherit)CMessageBase::hton(); ::hton(m_Param1); ::hton(m_Param2); ::hton(m_Param3); }
};

template <>
class CMessage3Param<char*, char*, char*> : public CMessageBase
{
public:
	void Init( int nCmd, const CSessionAddr& addrSrc, const CSessionAddr& addrDst, const char* str1, const char* str2, const char* str3, HRESULT nRet=S_OK )
	{
		CMessageBase::Init( nCmd, addrSrc, addrDst, nRet );
		SetLength( sizeof(CMessageBase)+strlen(str1)+strlen(str2)+strlen(str3)+3 );
		strcpy( GetData(), str1 );
		strcpy( GetData()+strlen(str1)+1, str2 );
		strcpy( GetData()+strlen(str1)+strlen(str2)+2, str3 );
	}
	const char* GetParam1(){ return GetData(); }
	const char* GetParam2(){ return GetData()+strlen(GetData())+1; }
	const char* GetParam3(){ 
		int len1, len2; 
		len1 = strlen(GetData());
		len2 = strlen(GetData()+len1+1);
		return GetData()+len1+len2+2;
	}
	void SetParams( const char* str1, const char* str2, const char* str3 )
	{ 
		strcpy( GetData(), str1 );
		strcpy( GetData()+strlen(str1)+1, str2 );
		strcpy( GetData()+strlen(str1)+strlen(str2)+2, str3 );
	}

	void ntoh( bool bInherit=true ){ if(bInherit)CMessageBase::ntoh(); }
	void hton( bool bInherit=true ){ if(bInherit)CMessageBase::hton(); }
};

//CMessageTrash is used to delete an allocated message, it guarantees the message being freed even when exception is thrown
class CMessageTrash
{
private:
	CMessageBase* m_pMsg;
public:
	CMessageTrash( CMessageBase* pMsg ):m_pMsg(pMsg){}
	~CMessageTrash(){ if( m_pMsg!=NULL )delete m_pMsg; }
};


//SendMessage send CMessageBase or it's derived class through sock.
//The function will throw an exception if the sock fails
template< typename MSG_TYPE >
void SendMessage( SOCKET sock, MSG_TYPE* pMsg )
{
	int nLength = pMsg->GetLength();
	pMsg->hton();
	SendBuffer( sock, (char*)pMsg, nLength );
//	SendLenBuffer( sock, (char*)pMsg, offsetof(MSG_TYPE, m_nLength) );
}

//SendMessage send CMessageBase or it's derived class through sock to addr.
//The function will throw an exception if the sock fails
template< typename MSG_TYPE >
void SendMessageTo( SOCKET sock, MSG_TYPE* pMsg, SOCKADDR_IN& addr )
{
	int nLength = pMsg->GetLength();
	pMsg->hton();
	SendBufferTo( sock, (char*)pMsg, nLength, (SOCKADDR*)&addr, sizeof(SOCKADDR_IN) );
}

template< typename MSG_TYPE >
void BroadcastMessage( SOCKET sock, MSG_TYPE* pMsg, short nPort )
{
	//enable broadcast
	BOOL bVal = TRUE;
	int nRet = setsockopt( sock, SOL_SOCKET, SO_BROADCAST, (char*)&bVal, sizeof(BOOL) );

	//broadcast the server poll message to local net at startup.
	//create the broadcast address
	SOCKADDR_IN dstaddr;
	dstaddr.sin_family = AF_INET;
	dstaddr.sin_port = htons( nPort );
	dstaddr.sin_addr.s_addr = INADDR_BROADCAST;

	SendMessageTo( sock, pMsg, dstaddr );

	//disable broadcast
	bVal = FALSE;
	nRet = setsockopt( sock, SOL_SOCKET, SO_BROADCAST, (char*)&bVal, sizeof(BOOL) );
}

/*
RecvLenBuffer receive a message from a stream sock. 
the template parameter <MSG_TYPE> determines what kind message it is.
The function calls ntoh() for this type of message to translate network byte order to machine byte order.
Return: the function reutnrn NULL in case the stream is closed.
Exception: the function throws CSockException in case of error happens
*/
template< typename MSG_TYPE >
MSG_TYPE* RecvMessage( SOCKET sock )
{
	CMessageBase msgBase;
	//peek the head bytes from receive buffer but don't remove them
	int nRet = recv( sock, (char*)&msgBase, sizeof(CMessageBase), MSG_PEEK );
	if( nRet == 0 )return NULL;
	if( nRet<0 )throw new CSockException();

	ASSERT( nRet==sizeof(CMessageBase) );
	msgBase.ntoh();
	int nLen = msgBase.GetLength();
	ASSERT( nLen>=sizeof(CMessageBase) );

	//now receive the stream indicated by nLen
	CMessageBase* pMsg = CMessageBase::Alloc( nLen );
	try{
		nRet = RecvBuffer( sock, (char*)pMsg, nLen );
		if( nRet<nLen ){	//the socket closed during trasmission
			CMessageBase::Dealloc( pMsg );
			return NULL;
		}

		ASSERT( nRet==nLen );
		if( nLen==sizeof(CMessageBase) )pMsg->ntoh();
		else ((MSG_TYPE*)pMsg)->ntoh();
		return (MSG_TYPE*)pMsg;
	}catch( CSockException* e ){
		CMessageBase::Dealloc( pMsg );
		throw;
	}
}

/*
RecvLenBuffer receive a message from a stream or package sock. 
the template parameter <MSG_TYPE> determines what kind message it is.
The function calls ntoh() for this type of message to translate network byte order to machine byte order.
Return: the function reutnrn NULL in case the stream is closed.
Exception: the function throws CSockException in case of error happens
*/
template< typename MSG_TYPE >
MSG_TYPE* RecvMessageFrom( SOCKET sock, SOCKADDR_IN& srcaddr )
{
	socklen_t addrlen = sizeof(SOCKADDR_IN);
	CMessageBase msgBase;
	int nRet = recvfrom( sock, (char*)&msgBase, sizeof(CMessageBase), MSG_PEEK, (SOCKADDR*)&srcaddr, &addrlen );
	if( nRet==0 )return NULL;
#ifdef _WIN32
	if( nRet<0 && WSAGetLastError()!=WSAEMSGSIZE ){
		throw new CSockException();
	}
	nRet=sizeof(CMessageBase);
#else
	if( nRet<0 )throw new CSockException();
#endif

	ASSERT( nRet==sizeof(CMessageBase) );
	ASSERT( addrlen==sizeof(SOCKADDR_IN) );
	msgBase.ntoh();
	int nLen = msgBase.GetLength();
	ASSERT( nLen>=sizeof(CMessageBase) );

	//now receive the stream indicated by nLen
	CMessageBase* pMsg = CMessageBase::Alloc( nLen );
	try{
		nRet = RecvBufferFrom( sock, (char*)pMsg, nLen, (SOCKADDR*)&srcaddr, &addrlen );
		if( nRet<nLen ){	//the socket closed during trasmission
			CMessageBase::Dealloc( pMsg );
			return NULL;
		}

		ASSERT( nRet==nLen );
		if( nLen==sizeof(CMessageBase) )pMsg->ntoh();
		else ((MSG_TYPE*)pMsg)->ntoh();
		return (MSG_TYPE*)pMsg;
	}catch( CSockException* e ){
		CMessageBase::Dealloc( pMsg );
		throw;
	}
}

/*//RecvLenBuffer receive a self length encoded stream. 
//The (buf+nLenOffset) must be a size field in network byte order indicating the length of the receiving stream
//the nLenOffset must less than MAXOFFSET (which is a minimum package size on internet)
//the function return false if the nLenOffset is not within the range or there is no enough buffer space to hold the whole stream.
inline CMessageBase* RecvMessage( SOCKET sock )
{
	char buf[ sizeof(CMessageBase) ];
	CMessageBase* pMsgBuf = (CMessageBase*)buf;

	//peek the head bytes from receive buffer but don't remove them
	int nRet = recv( sock, buf, sizeof(CMessageBase), MSG_PEEK );
	if( nRet <= 0 )return NULL;

	if( nRet<=0 )throw new CSockException();
	ASSERT( nRet>=sizeof(CMessageBase) );

	//now receive the stream indicated by nLen
	pMsgBuf->ntoh();
	int nLen = pMsgBuf->GetLength();

	char* pbuf = new char[nLen];

	RecvBuffer( sock, pbuf, nLen );

	((CMessageBase*)pbuf)->ntoh();
	return (CMessageBase*)pbuf;
}

inline CMessageBase* RecvMsgFrom( SOCKET sock, SOCKADDR_IN& srcaddr )
{
	socklen_t addrlen = sizeof(SOCKADDR_IN);
	CMessageBase msg;
	int nLen = recvfrom( sock, (char*)&msg, sizeof(CMessageBase), MSG_PEEK, (SOCKADDR*)&srcaddr, &addrlen );
	if( nLen==-1 ){
		cout<<"recvfrom might error"<<endl;
		//int nRet = WSAGetLastError();
		//return NULL;
		//nRet = nRet;
	}
	ASSERT( nLen>=sizeof(CMessageBase) );
	ASSERT( addrlen==sizeof(SOCKADDR_IN) );

	msg.ntoh();
	char* pbuf = new char[ msg.GetLength() ];
	nLen = recvfrom( sock, pbuf, msg.GetLength(), 0, (SOCKADDR*)&srcaddr, &addrlen );
	((CMessageBase*)pbuf)->ntoh();
	ASSERT( nLen!=-1 );
	return (CMessageBase*)pbuf;
}*/

typedef struct token_info
{
	int		nServId;
	short	nCpuMhz;
	short	nCpuType;
public:
	void ntoh(){
		nServId = ntohl( nServId );
		nCpuMhz = ntohs( nCpuMhz );
		nCpuType = ntohs( nCpuType );
	}
	void hton(){
		nServId = htonl( nServId );
		nCpuMhz = htons( nCpuMhz );
		nCpuType = htons( nCpuType );
	}
}TOKEN_INFO;

#pragma pack()


#define CTCPU_INTEL		0
#define CTCPU_SPARC		1

#define CTS_IDLE		0x00000000
#define CTS_RUNNING		0x00000001

#define CTS_E_SOCK		0x00000002
#define CTF_DEAD		0x80000000
#define CTF_SOUND		0x40000000

class CServerToken
{
public:
	CSessionAddr	m_addrServ;
	int				m_nCpuType;
	int				m_nCpuMhz;
	int				m_nState;
public:
	CServerToken( const CSessionAddr& addrServ, int nCpuType, int nCpuMhz, int nState=CTS_IDLE )
		:m_addrServ(addrServ), m_nCpuType(nCpuType), m_nCpuMhz(nCpuMhz), m_nState(nState){}
	const CSessionAddr& GetServAddr() { return m_addrServ; }
	int  GetCpuType(){ return m_nCpuType; }
	int	 GetCpuMhz(){ return m_nCpuMhz; }
	bool IsIdle(){ return m_nState==CTS_IDLE; }
	bool IsRunning(){ return m_nState==CTS_RUNNING; }
	bool IsDead(){ return isset(m_nState,CTF_DEAD); }
	bool IsHostDead(){ return (m_nState==(CTF_DEAD|CTS_E_SOCK)); }
	void SetState( int nState ){ m_nState = nState; }

public:
	bool LessByCpuMhz( const CServerToken& other )const { return m_nCpuMhz < other.m_nCpuMhz; }
};

class CProxyToken : public CServerToken
{
public:
	CSessionAddr m_addrPeer;
public:
	CProxyToken( const CSessionAddr& addrServ, const CSessionAddr& addrPeer, int nCpuType, int nCpuMhz, int nState=CTS_IDLE )
		: CServerToken( addrServ, nCpuType, nCpuMhz, nState ), m_addrPeer(addrPeer){}
	const CSessionAddr& GetPeerAddr(){ return m_addrPeer; }
};

class TokenCpuCmp
{
public:
	bool operator() ( const CServerToken* pTok1, const CServerToken* pTok2 ){
		return pTok1->LessByCpuMhz( *pTok2 );
	}
};

//type of operating system (nOpSystem)
#define SI_WIN32			1
#define SI_UNIX				2
#define SI_LINUX			3

//the system information structure used by GetSysInfo
typedef struct _SERVER_INFO{
	int		nOpSystem;			//the operating system type.
	int		bShareDisk;			//if the hard drive is shared by multiple hosts
	int		nSysStat;			//the system status.
	int		bSysIdle;			//the system is idle
	int		nSessions;			//the number of running sessions
	int		nChildPorcs;		//the number of running processes
	IN_ADDR	ipAddr;				//server ip address
}SERVERINFO;

inline void ntoh( SERVERINFO& info )
{
	info.nOpSystem = ntohl(info.nOpSystem);
	info.bShareDisk = ntohl(info.bShareDisk);
	info.nSysStat = ntohl( info.nSysStat );
	info.bSysIdle = ntohl( info.bSysIdle );
	info.nSessions = ntohl( info.nSessions );
	info.nChildPorcs = ntohl( info.nChildPorcs );
}

inline void hton( SERVERINFO& info )
{
	info.nOpSystem = htonl(info.nOpSystem);
	info.bShareDisk = htonl(info.bShareDisk);
	info.nSysStat = htonl( info.nSysStat );
	info.bSysIdle = htonl( info.bSysIdle );
	info.nSessions = htonl( info.nSessions );
	info.nChildPorcs = htonl( info.nChildPorcs );
}