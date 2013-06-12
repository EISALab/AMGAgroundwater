#ifndef _NETSOCK_H_
#define _NETSOCK_H_

#include "netbase.h"

#include <string>
#include <errno.h>

#ifndef _WIN32
#endif

#define _SOCK_INLINE_	inline

class CSockAddrIn
{
public:
	SOCKADDR_IN m_sockAddr;
public:
	CSockAddrIn(){
		memset( &m_sockAddr, 0, sizeof(m_sockAddr) );
		m_sockAddr.sin_family = AF_INET;
	}
	CSockAddrIn( IN_ADDR in_addr, UINT nSocketPort=0 ){
		memset( &m_sockAddr, 0, sizeof(m_sockAddr) );
		m_sockAddr.sin_family = AF_INET;
		m_sockAddr.sin_addr = in_addr;
		m_sockAddr.sin_port = htons((u_short)nSocketPort);
	}
	CSockAddrIn( const SOCKADDR_IN& addr ){
		m_sockAddr = addr;
	}
	bool Create( LPCSTR lpszHostAddress, UINT nSocketPort=0 ){
		memset( &m_sockAddr, 0, sizeof(m_sockAddr) );
		m_sockAddr.sin_family = AF_INET;
		if( !Inet_InAddr(lpszHostAddress, m_sockAddr.sin_addr) )
			return false;
		m_sockAddr.sin_port = htons((u_short)nSocketPort);
		return true;
	}

//properties.
	socklen_t size()const { return sizeof(m_sockAddr); }

	IN_ADDR GetInAddr(){ return m_sockAddr.sin_addr; }
	void SetInAddr( IN_ADDR in_addr ){ m_sockAddr.sin_addr = in_addr; }

	UINT GetPort(){ return (ntohs)(m_sockAddr.sin_port); }
	void SetPort( UINT nPort ){ m_sockAddr.sin_port = htons((u_short)nPort); }

//operators
	operator SOCKADDR_IN&(){
		return m_sockAddr;
	}
	operator LPSOCKADDR_IN(){
		return &m_sockAddr;
	}
	operator LPSOCKADDR(){
		return (SOCKADDR*)&m_sockAddr;
	}
};

class CSocket
{
public:
	SOCKET m_hSocket;

private:
	CSocket(const CSocket& rSrc);    // no implementation
	void operator=(const CSocket& rSrc);  // no implementation

public:
	CSocket();
	virtual ~CSocket();

	operator SOCKET() const;

	bool Create(UINT nSocketPort = 0, int nSocketType=SOCK_STREAM, LPCSTR lpszSocketAddress = NULL);

	bool Socket(int nSocketType = SOCK_STREAM, int nProtocolType = 0, int nAddressFormat = PF_INET);
	bool Bind(const SOCKADDR* lpSockAddr, int nSockAddrLen);
	bool Bind(const SOCKADDR_IN* lpSockInAddr );
	bool Bind(UINT nSocketPort=0, LPCSTR lpszSocketAddress = NULL);

	bool Open(LPCSTR lpszHostAddress, UINT nHostPort);
	bool Open(const SOCKADDR_IN* lpSockInAddr);
	int Write( const void* lpBuf, int nBufLen );
	int Read( void* lpBuf, int nBufLen );
	void Close();

	bool Connect(LPCSTR lpszHostAddress, UINT nHostPort);
	bool Connect(const SOCKADDR* lpSockAddr, int nSockAddrLen);
	bool Connect(const SOCKADDR_IN* lpSockInAddr );
	bool Listen(int nConnectionBacklog=5);
	bool Accept(CSocket& rConnectedSocket, SOCKADDR* lpSockAddr = NULL, socklen_t* lpSockAddrLen = NULL);

	enum { receives = 0, sends = 1, both = 2 };
	bool ShutDown(int nHow = sends);

	int Send(const void* lpBuf, int nBufLen, int nFlags = 0);
	int SendTo(const void* lpBuf, int nBufLen, UINT nHostPort, LPCSTR lpszHostAddress = NULL, int nFlags = 0);
	int SendTo(const void* lpBuf, int nBufLen, const SOCKADDR* lpSockAddr, int nSockAddrLen, int nFlags = 0);
	int SendTo(const void* lpBuf, int nBufLen, const SOCKADDR_IN* lpSockInAddr, int nFlags = 0);

	int Receive(void* lpBuf, int nBufLen, int nFlags = 0);
	int ReceiveFrom(void* lpBuf, int nBufLen, std::string& rSocketAddress, UINT& rSocketPort, int nFlags = 0);
	int ReceiveFrom(void* lpBuf, int nBufLen, SOCKADDR* lpSockAddr, socklen_t* lpSockAddrLen, int nFlags = 0);
	int ReceiveFrom(void* lpBuf, int nBufLen, SOCKADDR_IN* lpSockInAddr, int nFlags = 0);

	bool Attach(SOCKET hSocket);
	SOCKET Detach();

	bool GetPeerName(std::string& rPeerAddress, UINT& rPeerPort);
	bool GetPeerName(SOCKADDR* lpSockAddr, socklen_t* lpSockAddrLen);
	bool GetPeerName(SOCKADDR_IN* lpSockInAddr);

	bool GetSockName(std::string& rSocketAddress, UINT& rSocketPort);
	bool GetSockName(SOCKADDR* lpSockAddr, socklen_t* lpSockAddrLen);
	bool GetSockName(SOCKADDR_IN* lpSockInAddr);

	bool SetSockOpt(int nOptionName, const void* lpOptionValue, socklen_t nOptionLen, int nLevel = SOL_SOCKET);
	bool GetSockOpt(int nOptionName, void* lpOptionValue, socklen_t* lpOptionLen, int nLevel = SOL_SOCKET);

	static int GetLastError();
	static void SetLastError( int nError );
};

_SOCK_INLINE_ bool CSocket::GetPeerName(SOCKADDR* lpSockAddr, socklen_t* lpSockAddrLen)
	{ ASSERT(INVALID_SOCKET!=m_hSocket); return (SOCKET_ERROR != getpeername(m_hSocket, lpSockAddr, lpSockAddrLen)); }
_SOCK_INLINE_ bool CSocket::GetSockName(SOCKADDR* lpSockAddr, socklen_t* lpSockAddrLen)
	{ ASSERT(INVALID_SOCKET!=m_hSocket); return (SOCKET_ERROR != getsockname(m_hSocket, lpSockAddr, lpSockAddrLen)); }
_SOCK_INLINE_ bool CSocket::Bind(const SOCKADDR* lpSockAddr, int nSockAddrLen)
	{ ASSERT(INVALID_SOCKET!=m_hSocket); return (SOCKET_ERROR != bind(m_hSocket, lpSockAddr, nSockAddrLen)); }
_SOCK_INLINE_ bool CSocket::Connect(const SOCKADDR* lpSockAddr, int nSockAddrLen)
	{ ASSERT(INVALID_SOCKET!=m_hSocket); return (SOCKET_ERROR != connect(m_hSocket, lpSockAddr, nSockAddrLen)); }
_SOCK_INLINE_ int CSocket::ReceiveFrom(void* lpBuf, int nBufLen, SOCKADDR* lpSockAddr, socklen_t* lpSockAddrLen, int nFlags)
	{ ASSERT(INVALID_SOCKET!=m_hSocket); return recvfrom(m_hSocket, (LPSTR)lpBuf, nBufLen, nFlags, lpSockAddr, lpSockAddrLen); }
_SOCK_INLINE_ bool CSocket::Listen(int nConnectionBacklog)
	{ ASSERT(INVALID_SOCKET!=m_hSocket); return (SOCKET_ERROR != listen(m_hSocket, nConnectionBacklog)); }
_SOCK_INLINE_ bool CSocket::ShutDown(int nHow)
	{ ASSERT(INVALID_SOCKET!=m_hSocket); return (SOCKET_ERROR != shutdown(m_hSocket,nHow)); }
_SOCK_INLINE_ int CSocket::SendTo(const void* lpBuf, int nBufLen, const SOCKADDR* lpSockAddr, int nSockAddrLen, int nFlags)
	{ ASSERT(INVALID_SOCKET!=m_hSocket); return sendto(m_hSocket, (LPSTR)lpBuf, nBufLen, nFlags, lpSockAddr, nSockAddrLen); }
_SOCK_INLINE_ CSocket::operator SOCKET() const
	{ return m_hSocket; }
_SOCK_INLINE_ bool CSocket::SetSockOpt(int nOptionName, const void* lpOptionValue, socklen_t nOptionLen, int nLevel)
	{ return (SOCKET_ERROR != setsockopt(m_hSocket, nLevel, nOptionName, (LPCSTR)lpOptionValue, nOptionLen)); }
_SOCK_INLINE_ bool CSocket::GetSockOpt(int nOptionName, void* lpOptionValue, socklen_t* lpOptionLen, int nLevel)
	{ return (SOCKET_ERROR != getsockopt(m_hSocket, nLevel, nOptionName, (LPSTR)lpOptionValue, lpOptionLen)); }

#endif
