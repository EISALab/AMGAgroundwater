#include "netsock.h"

#ifdef _DEBUG
#define VERIFY(f)          ASSERT(f)
#else
#define VERIFY(f)          (f)
#endif

int CSocket::GetLastError()
{
#ifdef _WIN32
	return WSAGetLastError();
#else
	return errno;
#endif
}

void CSocket::SetLastError( int nError )
{
#ifdef _WIN32
	WSASetLastError( nError );
#else
	errno = nError;
#endif
}

/////////////////////////////////////////////////////////////////////////////
// CSocket Construction

CSocket::CSocket()
{
	m_hSocket = INVALID_SOCKET;
}

CSocket::~CSocket()
{
	if (m_hSocket != INVALID_SOCKET)
		Close();
}


bool CSocket::Create(UINT nSocketPort, int nSocketType, LPCSTR lpszSocketAddress)
{
	if( Socket(nSocketType) ){
		if( Bind(nSocketPort,lpszSocketAddress) )
			return true;

		int nResult = GetLastError();
		Close();
		SetLastError( nResult );
	}
	return false;
}

bool CSocket::Open(LPCSTR lpszHostAddress, UINT nHostPort)
{
	if( INVALID_SOCKET==m_hSocket ){
		if( !Socket( SOCK_STREAM ) )
			return false;
	}

	return Connect( lpszHostAddress, nHostPort );
}

bool CSocket::Open(const SOCKADDR_IN* lpSockInAddr)
{
	if( INVALID_SOCKET==m_hSocket ){
		if( !Socket( SOCK_STREAM ) )
			return false;
	}

	return Connect( lpSockInAddr );
}

bool CSocket::Socket(int nSocketType, int nProtocolType, int nAddressFormat)
{
	ASSERT(m_hSocket == INVALID_SOCKET);

	m_hSocket = socket(nAddressFormat,nSocketType,nProtocolType);
	return (m_hSocket != INVALID_SOCKET);
}

/////////////////////////////////////////////////////////////////////////////
// CSocket Attributes

bool CSocket::Attach(SOCKET hSocket)
{
	ASSERT(hSocket != INVALID_SOCKET);

	m_hSocket = hSocket;
	return true;
}

SOCKET CSocket::Detach()
{
	SOCKET hSocket = m_hSocket;
	m_hSocket = INVALID_SOCKET;
	return hSocket;
}

bool CSocket::GetPeerName(SOCKADDR_IN* lpSockInAddr)
{
	memset(lpSockInAddr, 0, sizeof(SOCKADDR_IN));

	socklen_t nSockAddrLen = sizeof(SOCKADDR_IN);
	return GetPeerName((SOCKADDR*)lpSockInAddr, &nSockAddrLen);
}

bool CSocket::GetSockName(SOCKADDR_IN* lpSockInAddr)
{
	memset(lpSockInAddr, 0, sizeof(SOCKADDR_IN));

	socklen_t nSockAddrLen = sizeof(SOCKADDR_IN);
	return GetSockName((SOCKADDR*)lpSockInAddr, &nSockAddrLen);
}

bool CSocket::GetPeerName(string& rPeerAddress, UINT& rPeerPort)
{
	SOCKADDR_IN sockAddr;

	bool bResult = GetPeerName(&sockAddr);
	if (bResult)
	{
		rPeerPort = ntohs(sockAddr.sin_port);
		rPeerAddress = inet_ntoa(sockAddr.sin_addr);
	}
	return bResult;
}

bool CSocket::GetSockName(string& rSocketAddress, UINT& rSocketPort)
{
	SOCKADDR_IN sockAddr;

	bool bResult = GetSockName(&sockAddr);
	if (bResult)
	{
		rSocketPort = ntohs(sockAddr.sin_port);
		rSocketAddress = inet_ntoa(sockAddr.sin_addr);
	}
	return bResult;
}

/////////////////////////////////////////////////////////////////////////////
// CAscynSocket Operations

bool CSocket::Accept(CSocket& rConnectedSocket, SOCKADDR* lpSockAddr, socklen_t* lpSockAddrLen)
{
	ASSERT(rConnectedSocket.m_hSocket == INVALID_SOCKET);

	rConnectedSocket.m_hSocket = INVALID_SOCKET;

	SOCKET hTemp = accept(m_hSocket, lpSockAddr, lpSockAddrLen);

	if (hTemp == INVALID_SOCKET)
	{
		DWORD dwProblem = GetLastError();
		rConnectedSocket.m_hSocket = INVALID_SOCKET;
		SetLastError(dwProblem);
	}
	else
	{
		rConnectedSocket.m_hSocket = hTemp;
	}

	return (hTemp != INVALID_SOCKET);
}

bool CSocket::Bind(const SOCKADDR_IN* lpSockInAddr )
{
	return Bind( (SOCKADDR*)lpSockInAddr, sizeof(SOCKADDR_IN) );
}

bool CSocket::Bind(UINT nSocketPort, LPCSTR lpszSocketAddress)
{
	SOCKADDR_IN sockAddr;
	memset(&sockAddr,0,sizeof(sockAddr));

	sockAddr.sin_family = AF_INET;

	if (lpszSocketAddress == NULL)
		sockAddr.sin_addr.s_addr = htonl(INADDR_ANY);
	else
	{
		IN_ADDR in_addr;
		if( Inet_InAddr( lpszSocketAddress, in_addr ) )
			sockAddr.sin_addr = in_addr;
		else
			return false;
	}

	sockAddr.sin_port = htons((u_short)nSocketPort);

	return Bind( &sockAddr );
}

void CSocket::Close()
{
	if (m_hSocket != INVALID_SOCKET)
	{
		VERIFY(SOCKET_ERROR != closesocket(m_hSocket));
		m_hSocket = INVALID_SOCKET;
	}
}

bool CSocket::Connect(const SOCKADDR_IN* lpSockInAddr )
{
	return Connect( (SOCKADDR*)lpSockInAddr, sizeof(SOCKADDR_IN) );
}

bool CSocket::Connect(LPCSTR lpszHostAddress, UINT nHostPort)
{
	ASSERT(lpszHostAddress != NULL);

	SOCKADDR_IN sockAddr;
	memset(&sockAddr,0,sizeof(sockAddr));

	sockAddr.sin_family = AF_INET;

	if( !Inet_InAddr(lpszHostAddress, sockAddr.sin_addr) )
		return false;

	sockAddr.sin_port = htons((u_short)nHostPort);

	return Connect((SOCKADDR*)&sockAddr, sizeof(sockAddr));
}

int CSocket::Receive(void* lpBuf, int nBufLen, int nFlags)
{
	return recv(m_hSocket, (LPSTR)lpBuf, nBufLen, nFlags);
}

int CSocket::ReceiveFrom(void* lpBuf, int nBufLen, SOCKADDR_IN* lpSockInAddr, int nFlags)
{
	socklen_t nSockAddrLen = sizeof(SOCKADDR_IN);
	return ReceiveFrom( lpBuf, nBufLen, (SOCKADDR*)lpSockInAddr, &nSockAddrLen, nFlags );
}

int CSocket::ReceiveFrom(void* lpBuf, int nBufLen, string& rSocketAddress, UINT& rSocketPort, int nFlags)
{
	SOCKADDR_IN sockAddr;
	memset(&sockAddr, 0, sizeof(sockAddr));

	int nResult = ReceiveFrom(lpBuf, nBufLen, &sockAddr, nFlags);
	if(nResult != SOCKET_ERROR)
	{
		rSocketPort = ntohs(sockAddr.sin_port);
		rSocketAddress = inet_ntoa(sockAddr.sin_addr);
	}
	return nResult;
}

int CSocket::Send(const void* lpBuf, int nBufLen, int nFlags)
{
	return send(m_hSocket, (LPSTR)lpBuf, nBufLen, nFlags);
}

int CSocket::SendTo(const void* lpBuf, int nBufLen, const SOCKADDR_IN* lpSockInAddr, int nFlags)
{
	return SendTo( lpBuf, nBufLen, (SOCKADDR*)lpSockInAddr, sizeof(SOCKADDR_IN), nFlags );
}

int CSocket::SendTo(const void* lpBuf, int nBufLen, UINT nHostPort, LPCSTR lpszHostAddress, int nFlags)
{
	SOCKADDR_IN sockAddr;
	memset(&sockAddr,0,sizeof(sockAddr));

	sockAddr.sin_family = AF_INET;

	if (lpszHostAddress == NULL)
		sockAddr.sin_addr.s_addr = htonl(INADDR_BROADCAST);
	else
	{
		if( !Inet_InAddr(lpszHostAddress, sockAddr.sin_addr) )
			return false;
	}

	sockAddr.sin_port = htons((u_short)nHostPort);

	return SendTo(lpBuf, nBufLen, &sockAddr, nFlags);
}

int CSocket::Write( const void* lpBuf, int nBufLen )
{
	return Send( lpBuf, nBufLen );
}

int CSocket::Read( void* lpBuf, int nBufLen )
{
	return Receive( lpBuf, nBufLen );
}


