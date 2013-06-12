#pragma once

#include <stdlib.h>
#include <Winsock2.h>
#include <memory.h>
#include <io.h>
#include <strstream>

using namespace std;


#ifndef ELEMENTS
	#define ELEMENTS(array)		(sizeof(array)/sizeof((array)[0]))	//elements number of an arrray
#endif

#define bzero( addr, size ) memset( addr, 0, size )
#define bcopy( addrsrc, addrdst, size ) memcpy(addrdst, addrsrc, size)

#define setbit( a, i ) ( a |= i )
#define clrbit( a, i ) ( a &= ~(i) )
#define isset( a, i ) ( a & i )
#define isclr( a, i ) ( !isset( a, i ) )

#define WSVERS	MAKEWORD(2,0)

//typedef struct sockaddr_in	SOCKADDR_IN;
//typedef struct sockaddr		SOCKADDR;
typedef struct in_addr IN_ADDR;

typedef struct net_file_header
{
	int nSize;
	unsigned nAttrib;
	_fsize_t lFileSize;
}NET_FILE_HEADER;

inline void PrintNetError()
{
	int nEwsa = WSAGetLastError();
	int nElst = GetLastError();
	printf( "WSA Error is %d, Last Error is %d\n", nEwsa, nElst );
}

inline SOCKET Socket( int af, int type, int protocol )
{
	SOCKET sock_ret = socket( af, type, protocol );
	if( sock_ret < 0 ){
		perror( "socket failed\n" );
		exit(-1);
	}

	return sock_ret;
}

inline int Bind( SOCKET s, const struct sockaddr FAR *name, int namelen )
{
	int ret = bind( s, name, namelen );
	if( ret!=0 ){
		perror( "bind failed\n" );
		exit( -1 );
	}

	return ret;
};

inline int Listen( SOCKET s, int backlog )
{
	int ret = listen( s, backlog );
	if( ret!=0 ){
		perror( "listen failed\n" );
		exit( -1 );
	}
	return ret;
}

inline SOCKET Accept( SOCKET s, struct sockaddr FAR *addr, int FAR *addrlen )
{
	SOCKET sockclt=accept( s, addr, addrlen );
	if( sockclt<0 ){
		perror( "accept failed\n" );
		exit( -1 );
	}

	return sockclt;
}

inline int Select( int nfds, fd_set FAR *readfds, fd_set FAR *writefds, fd_set FAR *exceptfds, const struct timeval FAR *timeout )
{
	int nRet = select( nfds, readfds, writefds, exceptfds, timeout );
	if( nRet<0 ){
		perror( "select failed\n" );
		exit(-1);
	}
	return nRet;
}

inline int Connect( SOCKET s, const struct sockaddr FAR *name, int namelen )
{
	int nRet = connect( s, name, namelen );
	if( nRet!=0 ){
		perror( "connect failed\n" );
		exit(-1);
	}
	return nRet;
}

inline int GetSockName( SOCKET s, struct sockaddr FAR *name, int FAR *namelen )
{
	int nRet = getsockname( s, name, namelen );
	if( nRet!=0 ){
		perror( "getsockname failed\n" );
		exit(-1);
	}
	return nRet;
}

//send the whole buffer. If succeess, return is equals to nLen, otherwise return actual bytes sent out.
inline int SendBuffer( SOCKET sock, const char* buf, int nLen, int* pnBytesDone=NULL )
{
	int nSentBytes = 0;
	int nRet = 0;

	while( nSentBytes<nLen ){
		nRet = send( sock, buf+nSentBytes, nLen-nSentBytes, 0 );
		if( nRet==SOCKET_ERROR )break;

		nSentBytes += nRet;
	}

	if( pnBytesDone )*pnBytesDone = nSentBytes;
	return nRet==SOCKET_ERROR ? nRet : nSentBytes;
}

//send the whole buffer. If succeess, return is equals to nLen, otherwise return actual bytes sent out.
/*inline int SendBuffer( SOCKET sock, const char* buf, int nLen, int* pnRet=NULL )
{
	int nSentBytes = 0;
	int nRet = 0;

	while( nSentBytes<nLen ){
		nRet = send( sock, buf+nSentBytes, nLen-nSentBytes, 0 );
		if( nRet==SOCK_ERROR )break;

		nSentBytes += nRet;
	}
	if( pnRet ){
		if( nLen==nSentBytes )*pnRet=nSentBytes;
		else *pnRet = nRet;
	}
	return nSentBytes;
}


//send the whole buffer. If succeess, return is equals to nLen, otherwise return actual bytes sent out.
inline int SendBuffer( SOCKET sock, const char* buf, int nLen )
{
	int nSentBytes = 0;

	while( nSentBytes<nLen ){
		int nRet = send( sock, buf+nSentBytes, nLen-nSentBytes, 0 );
		if( nRet==SOCK_ERROR )break;

		nSentBytes += nRet;
	}
	return nSentBytes;
}

inline int RecvBuffer( SOCKET sock, char* buf, int nLen, bool* pbShutDown=NULL )
{
	int nRecvBytes = 0;
	int nRet = -1;

	while( nRecvBytes<nLen ){
		nRet = recv( sock, buf, nLen-nRecvBytes, 0 );
		if( nRet<=0 )break;

		nRecvBytes += nRet;
	}
	if( pbShutDown ){
		*pbShutDown = nRet==0;
	}
	return nRecvBytes;
}

inline int RecvBuffer( SOCKET sock, char* buf, int nLen, int* pnRet=NULL )
{
	int nRecvBytes = 0;
	int nRet = 0;

	while( nRecvBytes<nLen ){
		int nRet = recv( sock, buf, nLen-nRecvBytes, 0 );
		if( nRet<=0 )break;

		nRecvBytes += nRet;
	}
	if( pnRet ){
		if( nRecvBytes==nLen )*pnRet=nRecvBytes;
		else *pnRet = nRet;
	}
	return nRecvBytes;
}*/

inline int RecvBuffer( SOCKET sock, char* buf, int nLen, int* pnBytesDone=NULL )
{
	int nRecvBytes = 0;
	int nRet = -1;

	while( nRecvBytes<nLen ){
		nRet = recv( sock, buf+nRecvBytes, nLen-nRecvBytes, 0 );
		if( nRet<=0 )break;

		nRecvBytes += nRet;
	}
	if( pnBytesDone ) *pnBytesDone=nRecvBytes;
	return nRet<=0 ? nRet : nRecvBytes;
}

//the first integer in the buffer must be the length in network order.
inline int SendLenBuffer( SOCKET sock, const char* buf, int nLenOffset=0, int*pnBytesDone=NULL )
{
	int nLen = ntohl( *((int*)(buf+nLenOffset)) );
	return SendBuffer( sock, buf, nLen, pnBytesDone );
}

//the first integer received in the buffer must be the length in network order
inline int RecvLenBuffer( SOCKET sock, char* buf, int nBufSize, int nLenOffset=0, int* pnBytesDone=NULL )
{
	if( nLenOffset+(int)sizeof(int)>nBufSize )return -1;

	int nRet = RecvBuffer( sock, buf, nLenOffset+sizeof(int), pnBytesDone );
	if( nRet<=0 )return nRet;

	int nHeadBytes = nRet;
	int nLen = ntohl( *((int*)(buf+nLenOffset)) );
	if( nBufSize<nLen )return -1;

	nRet = RecvBuffer( sock, buf+nRet, nLen-nHeadBytes, pnBytesDone );

	if( pnBytesDone )*pnBytesDone += nHeadBytes;
	return nRet<=0 ? nRet : nRet+nHeadBytes;
}

inline bool SendFile( SOCKET sock, char* strFileName, int nBufSize=0x1000 )
{
	//find the file size and attribute
	struct _finddata_t fileinfo;
	intptr_t hfind = _findfirst( strFileName, &fileinfo );
	if( hfind==-1 )return false;

	bool bSuccess = false;
	do{
		if( !(fileinfo.attrib & FILE_ATTRIBUTE_DIRECTORY) ){
			bSuccess = true;
			break;
		}
	}while( _findnext( hfind, &fileinfo )==0 );
	_findclose( hfind );
	if( !bSuccess )return false;

	//put the file basic infomation into the header and send it out.
	NET_FILE_HEADER flHeader;
	flHeader.nSize = htonl( sizeof(NET_FILE_HEADER) );
	flHeader.nAttrib = htonl( fileinfo.attrib );
	flHeader.lFileSize = htonl( fileinfo.size );

	if( SendLenBuffer( sock, (char*)&flHeader )<0 )return false;

	//send the file content.
	FILE* pf = fopen( strFileName, "rb" );
	if( pf==NULL )return false;

	char* pBuf = new char[nBufSize];
	while( !feof(pf) ){
		int nLen = (int)fread( pBuf, 1, nBufSize, pf );
		if( SendBuffer( sock, pBuf, nLen, NULL )==SOCKET_ERROR ){
			bSuccess = false;
			break;
		}
	}

	delete[] pBuf;
	fclose( pf );
	return bSuccess;
}

inline bool RecvFile( SOCKET sock, char* strFileName, int nBufSize=0x1000 )
{
	bool bSuccess = true;

	//receive the file header
	NET_FILE_HEADER flHeader;

	if( RecvLenBuffer( sock, (char*)&flHeader, sizeof(flHeader) )<=0 )return false;
	flHeader.nSize = ntohl( flHeader.nSize );
	flHeader.nAttrib = ntohl( flHeader.nAttrib );
	flHeader.lFileSize = ntohl( flHeader.lFileSize );

	//receive the file content.
	unsigned long lAvailBytes = flHeader.lFileSize;
	FILE* pf = fopen( strFileName, "wb" );
	if( pf==NULL )return false;

	char* pBuf = new char[nBufSize];
	while( lAvailBytes>0 ){
		int nAvailBuf = min( (unsigned long)nBufSize, lAvailBytes );
		if( RecvBuffer( sock, pBuf, nAvailBuf )<=0 ||
			fwrite( pBuf, 1, nAvailBuf, pf )<(size_t)nAvailBuf ){
			bSuccess = false;
			break;
		}

		lAvailBytes -= nAvailBuf;
	}

	delete[] pBuf;
	fclose( pf );
	return bSuccess;
}

inline bool SendStream( SOCKET sock, istrstream& istr, int nBufSize=0x1000 )
{
	//get the stream size
	int nPos = istr.tellg();
	istr.seekg( 0, ios_base::end );
	int nSize = istr.tellg();
	nSize = nSize - nPos;
	istr.seekg( 0, nPos );

	//put the file basic infomation into the header and send it out.
	NET_FILE_HEADER flHeader;
	flHeader.nSize = htonl( sizeof(NET_FILE_HEADER) );
	flHeader.nAttrib = htonl( 0 );
	flHeader.lFileSize = htonl( nSize );

	if( SendLenBuffer( sock, (char*)&flHeader )<0 )return false;

	bool bSuccess = true;
	//send the file content.
	char* pBuf = new char[nBufSize];
	while( true ){
		istr.read( pBuf, nBufSize );
		int nLen = istr.gcount();

		if( SendBuffer( sock, pBuf, nLen, NULL )==SOCKET_ERROR ){
			bSuccess = false;
			break;
		}
		if( istr.fail() )break;
	}

	delete[] pBuf;
	return bSuccess;
}

inline bool RecvStream( SOCKET sock, ostrstream& ostr, int nBufSize=0x1000 )
{
	bool bSuccess = true;

	//receive the file header
	NET_FILE_HEADER flHeader;

	if( RecvLenBuffer( sock, (char*)&flHeader, sizeof(flHeader) )<=0 )return false;
	flHeader.nSize = ntohl( sizeof(NET_FILE_HEADER) );
	flHeader.nAttrib = ntohl( flHeader.nAttrib );
	flHeader.lFileSize = ntohl( flHeader.lFileSize );

	//receive the file content.
	unsigned long lAvailBytes = flHeader.lFileSize;

	char* pBuf = new char[nBufSize];
	while( lAvailBytes>0 ){
		int nAvailBuf = min( (unsigned long)nBufSize, lAvailBytes );
		if( RecvBuffer( sock, pBuf, nAvailBuf )<=0 ){
			bSuccess = false;
			break;
		}

		ostr.write( pBuf, nAvailBuf );
		if( ostr.fail() ){
			bSuccess = false;
			break;
		}

		lAvailBytes -= nAvailBuf;
	}

	delete[] pBuf;
	return bSuccess;
}