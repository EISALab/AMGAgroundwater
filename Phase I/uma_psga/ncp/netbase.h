#pragma once

//compitable head files for win32, unix and linux.
#ifdef _WIN32
#include <Winsock2.h>
#include <io.h>

#define CRTDBG_MAP_ALLOC
#include <stdlib.h>
#include <crtdbg.h>


#else
#include <unistd.h>
#include <ctype.h>
#include <sys/socket.h>
#include <sys/types.h>
#include <sys/time.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <netdb.h>
#include <strings.h>
#include <signal.h>
#include <fcntl.h>

#endif	//end of #ifdef _WIN32

#include <stdio.h>
#include <stdlib.h>
#include <memory.h>
#include <string.h>
#include <time.h>
#include <exception>

#include "commdef.h"
#include "ofdump.h"

//compitable macros for win32, unix and linux
#ifdef _WIN32		//win 32 macros
#define bzero( addr, size ) memset( (addr), 0, (size) )
#define bcopy( addrsrc, addrdst, size ) memcpy( (addrdst), (addrsrc), (size) )

#define WSVERS	MAKEWORD(2,0)
typedef int socklen_t;

#else
typedef unsigned int	UINT;
typedef char*			LPSTR;
typedef const char*		LPCSTR;

typedef struct sockaddr_in	SOCKADDR_IN;
typedef SOCKADDR_IN*		LPSOCKADDR_IN;
typedef struct sockaddr		SOCKADDR;
typedef SOCKADDR*			LPSOCKADDR;
typedef struct timeval		TIMEVAL;
typedef int 				SOCKET;
#define SOCKET_ERROR		-1
#define INVALID_SOCKET		(SOCKET)(~0)

typedef struct in_addr		IN_ADDR;
typedef IN_ADDR*			LPIN_ADDR;

typedef struct hostent		HOSTENT;
typedef HOSTENT*			LPHOSTENT;
typedef unsigned long		DWORD;

#ifndef INADDR_NONE
#define INADDR_NONE			((in_addr_t) 0xffffffff)
#endif

/*
 * WinSock 2 extension -- manifest constants for shutdown()
 */
#define SD_RECEIVE      0x00
#define SD_SEND         0x01
#define SD_BOTH         0x02

#define BOOL		int
//typedef int BOOL;
#define TRUE				1
#define FALSE				0
#define FAR
#define closesocket			close

#ifndef min
#define min(a,b) ((a)<(b)?(a):(b))
#endif
#ifndef max
#define max(a,b) ((a)>(b)?(a):(b))
#endif

inline char* strlwr( char* string )
{
	for( char* pstr=string; *pstr; pstr++ )
		if( *pstr>='A' && *pstr<='Z' )*pstr+=32;
	return string;
}

inline char* strupr( char *string )
{
	for( char* pstr=string; *pstr; pstr++ )
		if( *pstr>='a' && *pstr<='z' )*pstr-=32;
	return string;
}

#endif			//_WIN32

#ifndef ASSERT
	#if     _MSC_VER > 1000
			#include <crtdbg.h>
			#define ASSERT _ASSERT
	#else
			#include <assert.h>
			#define ASSERT assert
	#endif

	#define ASSERT_VALID(p) ASSERT(p!=NULL)
#endif

using namespace std;

#ifndef ELEMENTS
	#define ELEMENTS(array)		(sizeof(array)/sizeof((array)[0]))	//elements number of an arrray
#endif

#define MAXOFFSET	512-sizeof(int)

inline bool operator < (IN_ADDR a, IN_ADDR b)
{
	return a.s_addr < b.s_addr;
}

inline bool operator == (IN_ADDR a, IN_ADDR b)
{
	return a.s_addr==b.s_addr;
}

inline bool Inet_InAddr( LPCSTR lpszHostAddress, IN_ADDR& in_addr )
{
	ASSERT(lpszHostAddress != NULL);

	IN_ADDR tmp_addr;
	tmp_addr.s_addr = inet_addr(lpszHostAddress);

	if (tmp_addr.s_addr == INADDR_NONE)
	{
		LPHOSTENT lphost;
		lphost = gethostbyname(lpszHostAddress);
		if (lphost != NULL){
			in_addr.s_addr = ((LPIN_ADDR)lphost->h_addr)->s_addr;
			return true;
		}

		return false;
	}

	in_addr.s_addr = tmp_addr.s_addr;
	return true;
}


inline string GetErrorString( int nErrorCode )
{
	string strError;
#ifdef _WIN32
	LPVOID lpMsgBuf;
	FormatMessage( 
		FORMAT_MESSAGE_ALLOCATE_BUFFER | 
		FORMAT_MESSAGE_FROM_SYSTEM | 
		FORMAT_MESSAGE_IGNORE_INSERTS,
		NULL,
		nErrorCode,
		MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT), // Default language
		(LPTSTR) &lpMsgBuf,
		0,
		NULL 
	);
	strError = (char*)lpMsgBuf;
	// Free the buffer.
	LocalFree( lpMsgBuf );
#else
	strError = strerror( nErrorCode );
#endif
	return strError;
}

class CSockException : public exception
{
public:
	int m_nErrorCode;				//the error code that caused the excepton
	string m_strError;
public:
	CSockException(){
#ifdef _WIN32
		m_nErrorCode = WSAGetLastError();
#else
		m_nErrorCode = errno;
#endif
		m_strError = GetErrorString( m_nErrorCode );
	}
	CSockException( int nErrorCode ):m_nErrorCode(nErrorCode){
		m_strError = GetErrorString( m_nErrorCode );
	}
	CSockException( int nErrorCode, const char* pszErrMsg ):m_nErrorCode(nErrorCode){
		m_strError = pszErrMsg;
	}
	virtual ~CSockException() throw(){}

	int GetErrorCode(){ return m_nErrorCode; }
	virtual const char* what(){
		return m_strError.c_str();
	}
	void Delete(){ 
//		cdump<<"delete sock exception: "<<(void*)this<<endl;
		delete this; 
	}
};

/*inline void PrintNetError()
{
	int nEwsa = WSAGetLastError();
	int nElst = GetLastError();
	printf( "WSA Error is %d, Last Error is %d\n", nEwsa, nElst );
}*/

/*class CSocket
{
public:
	SOCKET m_hSocket;
public:
	CSocket();
	CSocket( int af, int type, int protocol );
	void Create( int af, int type, int protocol );
	void Bind( const SOCKADDR* name, int namelen );
	void Bind( const char* lpszAddress, UINT nPort );
};*/

inline SOCKET Socket( int af, int type, int protocol )
{
	SOCKET sock_ret = socket( af, type, protocol );
	if( sock_ret < 0 )throw new CSockException();

	return sock_ret;
}

inline int Bind( SOCKET s, const struct sockaddr FAR *name, int namelen )
{
	int nRet = bind( s, name, namelen );
	if( nRet != 0 )throw new CSockException();

	return nRet;
};

inline int Listen( SOCKET s, int backlog )
{
	int nRet = listen( s, backlog );
	if( nRet != 0 )throw new CSockException();

	return nRet;
}

inline SOCKET Accept( SOCKET s, struct sockaddr FAR *addr, socklen_t FAR *addrlen )
{
	SOCKET sockclt=accept( s, addr, addrlen );
	if( sockclt < 0 ){
		cdump<<lock<<"accepting exception\n"<<endl<<unlock;
		throw new CSockException();
	}

	return sockclt;
}

inline int Select( int nfds, fd_set FAR *readfds, fd_set FAR *writefds, fd_set FAR *exceptfds, struct timeval FAR *timeout )
{
	int nRet = select( nfds, readfds, writefds, exceptfds, timeout );
	if( nRet<0 )throw new CSockException();

	return nRet;
}

inline int Connect( SOCKET s, const struct sockaddr FAR *name, int namelen )
{
	int nRet = connect( s, name, namelen );
	if( nRet != 0 ){
		SOCKADDR_IN* name_in = (SOCKADDR_IN*)name;
		struct hostent * phent = gethostbyaddr( (char*)&name_in->sin_addr, sizeof(IN_ADDR), AF_INET );
		cdump<<lock<<(phent?phent->h_name:"unknown host")<<" connecting exception\n"<<endl<<unlock;
		throw new CSockException();
	}

	return nRet;
}

inline int Send ( SOCKET s, const char FAR * buf, int len, int flags )
{
	int nRet = send( s, buf, len, flags );
	if( nRet==SOCKET_ERROR ){
		cdump<<lock<<"sending exception\n"<<endl<<unlock;
		throw new CSockException();
	}

	return nRet;
}

inline int Recv( SOCKET s, char FAR* buf, int len, int flags )
{
	int nRet = recv( s, buf, len, flags );
	if( nRet==SOCKET_ERROR ){
		cdump<<lock<<"recving exception\n"<<endl<<unlock;
		throw new CSockException();
	}

	return nRet;
}

inline int GetSockName( SOCKET s, struct sockaddr FAR *name, socklen_t FAR *namelen )
{
	int nRet = getsockname( s, name, namelen );
	if( nRet != 0 )throw new CSockException();

	return nRet;
}

inline int GetPeerName( SOCKET s, struct sockaddr FAR* name, socklen_t FAR *namelen )
{
	int nRet = getpeername( s, name, namelen );
	if( nRet != 0 )throw new CSockException();

	return nRet;
}

inline IN_ADDR GetPeerInAddr( SOCKET s )
{
	SOCKADDR_IN addr;
	socklen_t len = sizeof(SOCKADDR_IN);
	GetPeerName( s, (SOCKADDR*)&addr, &len );
	return addr.sin_addr;
}

inline void SetSockNonBlock( SOCKET s, bool bNonBlock )
{
#ifdef _WIN32
	//set the connection back to block mode.
	u_long nonblock = 0;
	if( bNonBlock )nonblock = 1;
	ioctlsocket( s, FIONBIO, &nonblock );
#else
	int flags = fcntl( s, F_GETFL );
	if( bNonBlock )setbit(flags, O_NONBLOCK);
	else clrbit(flags, O_NONBLOCK);

	fcntl(s, F_SETFL, flags);
#endif
}

#ifdef _WIN32
// MS Transport Provider IOCTL to control
// reporting PORT_UNREACHABLE messages
// on UDP sockets via recv/WSARecv/etc.
// Path TRUE in input buffer to enable (default if supported),
// FALSE to disable.
#define SIO_UDP_CONNRESET _WSAIOW(IOC_VENDOR,12)

inline BOOL SetUdpBlock( SOCKET sd )
{
	DWORD dwBytesReturned = 0;
	BOOL bNewBehavior = FALSE;
	DWORD status;

	// disable  new behavior using
	// IOCTL: SIO_UDP_CONNRESET
	status = WSAIoctl(sd, SIO_UDP_CONNRESET,
					&bNewBehavior, sizeof(bNewBehavior),
					NULL, 0, &dwBytesReturned,
					NULL, NULL);

	if (SOCKET_ERROR == status)
	{
		DWORD dwErr = WSAGetLastError();
		if (WSAEWOULDBLOCK == dwErr)
		{
			// nothing to do
			return(FALSE);
		}
		else
		{
			printf("WSAIoctl(SIO_UDP_CONNRESET) Error: %d\n", dwErr);
			return(FALSE);
		}
	}
	return TRUE;
}

#endif


/* 
The function except to send nLen bytes from buf to the socket.
Exception: the function throws CSockException if error happens.
*/
inline void SendBuffer( SOCKET sock, const char* buf, int nLen )
{
	int nSentBytes = 0;
	int nRet = 0;

	while( nSentBytes<nLen ){
		nRet = send( sock, buf+nSentBytes, nLen-nSentBytes, 0 );
		if( nRet==SOCKET_ERROR )break;

		nSentBytes += nRet;
	}

	if( nRet==SOCKET_ERROR )throw new CSockException();
	ASSERT( nSentBytes==nLen );
}

/* 
The function except to send nLen bytes from buf through the socket to the address addr.
Exception: the function throws CSockException if error happens.
*/
inline void SendBufferTo( SOCKET sock, const char* buf, int nLen, const SOCKADDR* addr, socklen_t tolen )
{
	int nSentBytes = 0;
	int nRet = 0;

	while( nSentBytes<nLen ){
		nRet = sendto( sock, buf+nSentBytes, nLen-nSentBytes, 0, addr, tolen );
		if( nRet==SOCKET_ERROR )break;

		nSentBytes += nRet;
	}

	if( nRet==SOCKET_ERROR )throw new CSockException();
	ASSERT( nSentBytes==nLen );
}

/*
The function except to receive exact nLen bytes into buf unless the connection is closed.
Return: the number of bytes acctually received before connection is closed. If the socket is closed, the returned value should be less than nLen.
Exception: the function throws CSockException if error happens on the socket.
Note: if the sock connection is gracefully closed before the receiving is completed, 
		then the function returns the number of bytes acutall received, which should be less than nLen.
*/
inline int RecvBuffer( SOCKET sock, char* buf, int nLen )
{
	int nRecvBytes = 0;
	int nRet = 0;

	while( nRecvBytes<nLen ){
		nRet = recv( sock, buf+nRecvBytes, nLen-nRecvBytes, 0 );
		if( nRet<=0 )break;

		nRecvBytes += nRet;
	}

	if( nRet<0 )throw new CSockException();
	return nRecvBytes;
}

/*
The function except to receive exact nLen bytes into buf unless the connection is closed.
Return: the number of bytes acctually received before connection is closed. If the socket is closed, the returned value should be less than nLen.
Exception: the function throws CSockException if error happens on the socket.
Note: if the sock connection is gracefully closed before the receiving is completed, 
		then the function returns the number of bytes acutall received, which should be less than nLen.
*/
inline int RecvBufferFrom( SOCKET sock, char* buf, int nLen, SOCKADDR* addr, socklen_t* addrlen )
{
	int nRecvBytes = 0;
	int nRet = 0;

	while( nRecvBytes<nLen ){
		nRet = recvfrom( sock, buf+nRecvBytes, nLen-nRecvBytes, 0, addr, addrlen );
		if( nRet<=0 )break;

		nRecvBytes += nRet;
	}

	if( nRet<0 )throw new CSockException();
	return nRecvBytes;
}

/*
SendLenBuffer sends a stream with length information self-encoded.
The length information is stored within the buffer at the offset of nLenOffset.
The (buf+nLenOffset) must be int field stored in network byte order indicating the length of the sending stream
The nLenOffset must less than MAXOFFSET (which is a minimum package size on internet)
Exception: The functon throws exception if socket error happens during sending process.
*/
inline void SendLenBuffer( SOCKET sock, const char* buf, int nLenOffset=0 )
{
	ASSERT( nLenOffset<MAXOFFSET );

	int nLen = ntohl( *((int*)(buf+nLenOffset)) );
	SendBuffer( sock, buf, nLen );
}

/*
RecvLenBuffer receives a stream with length information self-encoded
The length information is stored within the buffer at the offset of nLenOffset.
The (buf+nLenOffset) must be int field stored in network byte order indicating the length of the sending stream
The nLenOffset must less than MAXOFFSET (which is a minimum package size on internet)
Return: true if the stream is completely received. false if the socket is closed during transmission.
Exception: the function throws CSockExcpetion if error happens
*/
inline bool RecvLenBuffer( SOCKET sock, char* buf, int nBufSize, int nLenOffset=0 )
{
	ASSERT( nLenOffset<=MAXOFFSET );

	//compute the head bytes
	int nHeadBytes = nLenOffset+sizeof(int);
	ASSERT( nHeadBytes<=nBufSize );

	//peek the head bytes from receive buffer but don't remove them
	int nRet = recv( sock, buf, nHeadBytes, MSG_PEEK );
	if( nRet<0 )throw new CSockException();
	if( nRet==0 )return false;
	ASSERT( nRet>=nHeadBytes );

	//now receive the stream indicated by nLen
	int nLen = ntohl( *((int*)(buf+nLenOffset)) );
	ASSERT( nBufSize>=nLen );
	return RecvBuffer( sock, buf, nLen )==nLen;
}