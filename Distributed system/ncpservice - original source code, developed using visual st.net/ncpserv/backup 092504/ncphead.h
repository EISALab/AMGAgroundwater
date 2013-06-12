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
#define	NCP_REGISTER_EXIT		0x40


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

#define E_UNKNOWNDST		0x80080000			//message can't reach destination.

#define E_IMPLEMENT			0x81000000			//the command is not implemented

/*************************************************************************************************
									ncp command
**************************************************************************************************/
#define CM_ACK				0xffffffff

//logon logoff
#define NCM_LOGON			1
#define CM_LOGOFF			2

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

#define NCM_REMFILE			12				//remove a file from the server
											//data:			filename
											//S_NOENT		file is not there

#define NCM_SOFTLINK			14				//create a soft link for a file for unix system
											//data:			filename
											//E_NOSUP

#define CM_LIST				15				//list a folder
#define	NCM_CHMOD			16				//change the rw mode of a file

#define NCM_FILEMODE			17				//change the file transfer mode to ascii or binary
											//data: O_TEXT, O_BINARY

//execution commands
#define NCM_RUNIMAGE			20				//run an executalbe image file on the server
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

#define NCM_FILEINFO		1005			//get file information.
#define NCM_LISTDIR			1006			//list dir

//query system command
#define CM_GETSYSINFO		40				//get server system information
#define CM_KILLPROCS		41				//kill all the waiting processes	//only root can do it.
#define CM_SETIDLE			42				//set the system state to idle. TRUE--idle, FALSE--busy


#define CM_CLEARALL			13

#define CM_RUNSYSCMD		21

#define CM_RESETSERVICE		100
#define CM_SETSERVICESTAT	101

#define NCM_PROCESSEXIT		10000

#define NCM_POLLSERVER		4000
#define NCM_SERVERSTART		4001

#define NCM_OPENSESSION		1000
#define NCM_CLOSESESSION	1001
#define NCM_PROXYSHUTDOWN	1002
#define NCM_OPENSTREAM		1200
//#define NCM_PASSIVE			1201
#define NCM_QUIT			1003
#define NCF_ACK				0x10000000

#define NCM_KILLPROCESS		4005

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
		else if( m_addrHost.s_addr==other.m_addrHost.s_addr )return m_nServId - other.m_nServId < 0;
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

	virtual const char* what(){
		return m_strError.c_str();
	}
	void Delete(){ delete this; }
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
		if( m_sock )closesocket(m_sock);
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

class CMessageHead{
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
	CMessageHead() : m_sProtocol(NCP_PROTOCOL), m_sVersion(NCP_VERSION), m_nLength(sizeof(CMessageHead)){}
	CMessageHead( unsigned int nCommand ): m_sProtocol(NCP_PROTOCOL), m_sVersion(NCP_VERSION), m_nLength(sizeof(CMessageHead)), m_nMessage(nCommand){}
	void Init()
	{
		bzero( this, sizeof(CMessageHead) );
		m_sProtocol = NCP_PROTOCOL;
		m_sVersion = NCP_VERSION;
		m_nLength = sizeof(CMessageHead);
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
		return (char*)this + sizeof(CMessageHead);
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
	CMessageHead* Clone(){
		CMessageHead* pNewMsg = (CMessageHead*) new char[GetLength()];
		bcopy( this, pNewMsg, GetLength() );
		return pNewMsg;
	}
	void CopyTo( char* buf ){
		bcopy( this, buf, GetLength() );
	}
	void Destroy(){
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
	bool Send( SOCKET sock ){
		hton();
		char* buf = (char*)this;
		return SendLenBuffer( sock, buf, offsetof(CMessageHead, m_nLength) );
	}
	bool SendTo( SOCKET sock, SOCKADDR_IN& addr ){
		int nLength = GetLength();
		hton();
		int nRet = sendto( sock, (char*)this, nLength, 0, (SOCKADDR*)&addr, sizeof(SOCKADDR_IN) );
		if( nRet==SOCKET_ERROR ){
			cdump<<"sendto error"<<endl;
			//cdump<<"error:"<<WSAGetLastError()<<endl;
		}
		return true;
	}
	bool Broadcast( SOCKET sock, short nPort ){
		//enable broadcast
		BOOL bVal = TRUE;
		int nRet = setsockopt( sock, SOL_SOCKET, SO_BROADCAST, (char*)&bVal, sizeof(BOOL) );

		//broadcast the server poll message to local net at startup.
		//create the broadcast address
		SOCKADDR_IN dstaddr;
		dstaddr.sin_family = AF_INET;
		dstaddr.sin_port = htons( nPort );
		dstaddr.sin_addr.s_addr = INADDR_BROADCAST;

		int nLength = GetLength();
		hton();
		nRet = sendto( sock, (char*)this, nLength, 0, (SOCKADDR*)&dstaddr, sizeof(SOCKADDR_IN) );

		//disable broadcast
		bVal = FALSE;
		nRet = setsockopt( sock, SOL_SOCKET, SO_BROADCAST, (char*)&bVal, sizeof(BOOL) );
		return true;
	}
	bool Recv( SOCKET sock, int nBufSize ){
		char* buf = (char*)this;
		bool bOk = RecvLenBuffer( sock, buf, nBufSize, offsetof(CMessageHead, m_nLength) );
		if( bOk ){
			ntoh();
			if( m_sProtocol!=NCP_PROTOCOL )return false;
			if( m_sVersion !=NCP_VERSION ) return false;
		}
		return bOk;
	}
};

class CMessageTrash
{
private:
	CMessageHead* m_pMsg;
public:
	CMessageTrash( CMessageHead* pMsg ):m_pMsg(pMsg){}
	~CMessageTrash(){ if( m_pMsg!=NULL )m_pMsg->Destroy(); }
};

template <typename T>
class CMessage1Param : public CMessageHead
{
public:
//	CMessageHead	m_MsgHead;
	T				m_Param;
public:
	CMessage1Param(){};
	CMessage1Param( const T& t ) : m_Param(t) {};
//	CMessageHead* GetMessageHead(){ return &m_MsgHead; }

	void Init( int nCmd, const CSessionAddr& addrSrc, const CSessionAddr& addrDst, const T& t, HRESULT nRet=S_OK )
	{
		CMessageHead::Init( nCmd, addrSrc, addrDst, nRet );
		SetLength( sizeof(CMessage1Param<T>) );
		//m_MsgHead.Init( nCmd, addrSrc, addrDst, nRet );
		//m_MsgHead.SetLength( sizeof(CMessage1Param<T>) );
		m_Param = t;
	}
	const T& GetParam(){ return m_Param; }
	void SetParam( const T& t ){ m_Param = t; }

	void ntoh(){ ::ntoh(m_Param); }
	void hton(){ ::hton(m_Param); }
};

template <typename T1, typename T2>
class CMessage2Param
{
public:
	CMessageHead	m_MsgHead;
	T1				m_Param1;
	T2				m_Param2;
public:
	CMessage2Param(){};
	CMessage2Param( const T1& t1, const T2& t2 ) : m_Param1(t1), m_Param2(t2) {};
	CMessageHead* GetMessageHead(){ return &m_MsgHead; }

	void Init( int nCmd, const CSessionAddr& addrSrc, const CSessionAddr& addrDst, const T1& t1, const T2& t2, HRESULT nRet=S_OK )
	{
		m_MsgHead.Init( nCmd, addrSrc, addrDst, nRet );
		m_MsgHead.SetLength( sizeof(CMessage2Param<T1, T2>) );
		m_Param1 = t1;
		m_Param2 = t2;
	}
	const T1& GetParam1(){ return m_Param1; }
	void SetParam1( const T1& t ){ m_Param1 = t; }

	const T2& GetParam2(){ return m_Param2; }
	void SetParam2( const T2& t ){ m_Param2 = t; }

	void ntoh(){ ::ntoh(m_Param1); ::ntoh(m_Param2); }
	void hton(){ ::hton(m_Param1); ::hton(m_Param2); }
};

template <typename T1, typename T2, typename T3>
class CMessage3Param
{
public:
	CMessageHead	m_MsgHead;
	T1				m_Param1;
	T2				m_Param2;
	T3				m_Param3;
public:
	CMessage3Param(){};
	CMessage3Param( const T1& t1, const T2& t2, const T3& t3 ) : m_Param1(t1), m_Param2(t2), m_Param3(t3) {};
	CMessageHead* GetMessageHead(){ return &m_MsgHead; }

	void Init( int nCmd, const CSessionAddr& addrSrc, const CSessionAddr& addrDst, const T1& t1, const T2& t2, const T3& t3, HRESULT nRet=S_OK )
	{
		m_MsgHead.Init( nCmd, addrSrc, addrDst, nRet );
		m_MsgHead.SetLength( sizeof(CMessage3Param<T1, T2, T3>) );
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

	void ntoh(){ ::ntoh(m_Param1); ::ntoh(m_Param2); ::ntoh(m_Param3); }
	void hton(){ ::hton(m_Param1); ::hton(m_Param2); ::hton(m_Param3); }
};


//RecvLenBuffer receive a self length encoded stream. 
//The (buf+nLenOffset) must be a size field in network byte order indicating the length of the receiving stream
//the nLenOffset must less than MAXOFFSET (which is a minimum package size on internet)
//the function return false if the nLenOffset is not within the range or there is no enough buffer space to hold the whole stream.
inline CMessageHead* RecvMessage( SOCKET sock )
{
	char buf[ sizeof(CMessageHead) ];
	CMessageHead* pMsgBuf = (CMessageHead*)buf;

	//peek the head bytes from receive buffer but don't remove them
	int nRet = recv( sock, buf, sizeof(CMessageHead), MSG_PEEK );
	if( nRet <= 0 )return NULL;

	if( nRet<=0 )throw new CSockException();
	ASSERT( nRet>=sizeof(CMessageHead) );

	//now receive the stream indicated by nLen
	pMsgBuf->ntoh();
	int nLen = pMsgBuf->GetLength();

	char* pbuf = new char[nLen];

	RecvBuffer( sock, pbuf, nLen );

	((CMessageHead*)pbuf)->ntoh();
	return (CMessageHead*)pbuf;
}

inline CMessageHead* RecvMsgFrom( SOCKET sock, SOCKADDR_IN& srcaddr )
{
	socklen_t addrlen = sizeof(SOCKADDR_IN);
	CMessageHead msg;
	int nLen = recvfrom( sock, (char*)&msg, sizeof(CMessageHead), MSG_PEEK, (SOCKADDR*)&srcaddr, &addrlen );
	if( nLen==-1 ){
		cout<<"recvfrom might error"<<endl;
		//int nRet = WSAGetLastError();
		//return NULL;
		//nRet = nRet;
	}
	ASSERT( nLen>=sizeof(CMessageHead) );
	ASSERT( addrlen==sizeof(SOCKADDR_IN) );

	msg.ntoh();
	char* pbuf = new char[ msg.GetLength() ];
	nLen = recvfrom( sock, pbuf, msg.GetLength(), 0, (SOCKADDR*)&srcaddr, &addrlen );
	((CMessageHead*)pbuf)->ntoh();
	ASSERT( nLen!=-1 );
	return (CMessageHead*)pbuf;
}

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

class CNcpMessage{
public:
	unsigned short m_sProtocol, m_sVersion;	//protocol and version
	unsigned int m_nSize;					//size of the message package
	unsigned int m_nCmd;					//command of the message
	unsigned int m_nParamRet;				//it could be a parameter for command or a ret code for ack
	char data[0];
public:
	CNcpMessage(){
		bzero( this, sizeof(CNcpMessage) );
		m_sProtocol = NCP_PROTOCOL;
		m_sVersion = NCP_VERSION;
		m_nSize = sizeof(CNcpMessage);
	}
	CNcpMessage( int nCmd ){
		bzero( this, sizeof(CNcpMessage) );
		m_sProtocol = NCP_PROTOCOL;
		m_sVersion = NCP_VERSION;
		m_nCmd = nCmd;
		m_nSize = sizeof(CNcpMessage);
	}
	void Init( int nCmd, int nRet=S_OK ){
		bzero( this, sizeof(CNcpMessage) );
		m_sProtocol = NCP_PROTOCOL;
		m_sVersion = NCP_VERSION;
		m_nCmd = nCmd;
		m_nSize = sizeof(CNcpMessage);
		m_nParamRet = nRet;
	}
	int GetCmd(){
		return m_nCmd;
	}
	int GetSize(){
		return m_nSize;
	}
	void SetSize( int nSize ){
		m_nSize = nSize;
	}
	void SetDataSize( int nDataSize ){
		m_nSize = nDataSize + sizeof(CNcpMessage);
	}
	int GetRet(){
		return m_nParamRet;
	}
	void SetRet( int nRet ){
		m_nParamRet = nRet;
	}
	char* GetData(){
		return data;
	}
	void ntoh(){
		m_sProtocol = ntohs( m_sProtocol );
		m_sVersion = ntohs( m_sVersion );
		m_nSize = ntohl( m_nSize );
		m_nCmd = ntohl( m_nCmd );
		m_nParamRet = ntohl( m_nParamRet );
	}
	void hton(){
		m_sProtocol = htons( m_sProtocol );
		m_sVersion = htons( m_sVersion );
		m_nSize = htonl( m_nSize );
		m_nCmd = htonl( m_nCmd );
		m_nParamRet = htonl( m_nParamRet );
	}
	bool IsAck(){
		return m_nCmd==CM_ACK;
	}
	bool IsSucceeded(){
		return SUCCEEDED(m_nParamRet);
	}
	bool IsFailed(){
		return FAILED(m_nParamRet);
	}
	bool Send( SOCKET sock ){
		hton();
		char* buf = (char*)this;
		return SendLenBuffer( sock, buf, offsetof(CNcpMessage, m_nSize) );
	}
	bool Recv( SOCKET sock, int nBufSize ){
		char* buf = (char*)this;
		bool bOk = RecvLenBuffer( sock, buf, nBufSize, offsetof(CNcpMessage, m_nSize) );
		if( bOk ){
			ntoh();
			if( m_sProtocol!=NCP_PROTOCOL )return false;
			if( m_sVersion !=NCP_VERSION ) return false;
		}
		return bOk;
	}
};
#pragma pack()


#define CTCPU_INTEL		0
#define CTCPU_SPARC		1

#define CTS_IDLE		0
#define CTS_RUNNING		1
#define CTS_DEAD		2

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
	bool IsDead(){ return m_nState==CTS_DEAD; }
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
typedef struct _SYSINFO{
	int nOpSystem;			//the operating system type.
	int bShareDisk;			//if the hard drive is shared by multiple hosts
	int nSysStat;			//the system status.
	int bSysIdle;			//the system is idle
	int nSessions;			//the number of running sessions
	int nChildPorcs;		//the number of running processes
}SYSINFO;

inline void sysinfo_ntoh( SYSINFO* pinfo )
{
	pinfo->nOpSystem = ntohl(pinfo->nOpSystem);
	pinfo->bShareDisk = ntohl(pinfo->bShareDisk);
	pinfo->nSysStat = ntohl( pinfo->nSysStat );
	pinfo->bSysIdle = ntohl( pinfo->bSysIdle );
	pinfo->nSessions = ntohl( pinfo->nSessions );
	pinfo->nChildPorcs = ntohl( pinfo->nChildPorcs );
}

inline void sysinfo_hton( SYSINFO* pinfo )
{
	pinfo->nOpSystem = htonl(pinfo->nOpSystem);
	pinfo->bShareDisk = htonl(pinfo->bShareDisk);
	pinfo->nSysStat = htonl( pinfo->nSysStat );
	pinfo->bSysIdle = htonl( pinfo->bSysIdle );
	pinfo->nSessions = htonl( pinfo->nSessions );
	pinfo->nChildPorcs = htonl( pinfo->nChildPorcs );
}