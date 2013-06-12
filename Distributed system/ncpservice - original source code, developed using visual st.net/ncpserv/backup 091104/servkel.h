#include "uwkelobj.h"
#include <list>
#include <set>
#include <map>
#include "msgqueue.h"
#include "cpuinfo.h"

class CMessageRouter;
class CServerSession;

class CServerApp;
class CServerProxy;
class CServerSession;

class CServerProxy
{
private:
	HANDLE	m_hMutexSock;
	HANDLE	m_hMutex;
	CServerApp* m_pServApp;

	SOCKET	m_sock;
	CSessionAddr m_addrProxy;

	list< CMessageHead* >	m_WaitingMessages;
	std::map< CSessionAddr, CServerSession* > m_ServSessions;
	std::map<IN_ADDR, CProxySocket*> m_PeerSocks;

	HANDLE m_hProxyThread;
protected:
	bool MessageFromClient();
	bool MessageFromServer( SOCKET sock );
	void PumpWaitingMessages( SOCKET sock, bool bOk );
public:
	void LockSock(){ LockMutex( m_hMutexSock );	}
	void UnlockSock(){ UnlockMutex( m_hMutexSock );	}
	void Lock(){ LockMutex(m_hMutex); }
	void Unlock(){ UnlockMutex(m_hMutex); }
public:
	CServerProxy( CServerApp* pServApp, SOCKET sock, const CSessionAddr& addrProxy ){
		m_addrProxy = addrProxy;
		m_sock = sock;
		m_pServApp = pServApp;
		m_hMutexSock = CreateMutex();
		m_hMutex = CreateMutex();
		m_hProxyThread = NULL;
	}
	~CServerProxy(){
		CloseHandle( m_hMutexSock );
		CloseHandle( m_hMutex );
	}

	const CSessionAddr& GetAddr(){
		return m_addrProxy;
	}

	CProxySocket* GetPeerSocket( IN_ADDR addr ){
		std::map<IN_ADDR, CProxySocket*>::iterator pos = m_PeerSocks.find( addr );
		if( pos!=m_PeerSocks.end() )return pos->second;
		return NULL;
	}
	void InsertPeerSocket( IN_ADDR addr, CProxySocket* pProxySock ){
		m_PeerSocks[addr] = pProxySock;
	}
	void RemovePeerSocket( IN_ADDR addr ){
		std::map<IN_ADDR, CProxySocket*>::iterator pos = m_PeerSocks.find( addr );
		if( pos!=m_PeerSocks.end() )m_PeerSocks.erase( pos );
	}

	CServerSession* GetServerSession( const CSessionAddr& addr );
	void InsertServerSession( CServerSession* pServSession );
	void RemoveServerSession( const CSessionAddr& addr );

	bool SendMessage( CMessageHead* pMsg );

	//Message loop.
	void RouteMessage();
	CMessageHead* GetMessage();

	//SendMessage may be blocked
	void DispatchMessage( CMessageHead* pMsg );
	void ProcessMessage( CMessageHead* pMsg );

	//process functin call
	void OpenServerSession( const CSessionAddr& addrClnt );
	void CloseServerSession( CServerSession* pServSession );

	//thread
	static int ServerProxyThread( void* arg );
	bool StartServerProxy();
	void WaitForObject();

	//enforce the channel to shutdown
	void Shutdown();
};


#define SS_CLOSED		-1
#define SS_OPEN_WAIT	1
#define SS_ESTABLISHED	0

class CServerSession
{
protected:
	CSessionAddr	m_addrClnt;			//client address
	CSessionAddr	m_addrServ;			//server address

	CServerProxy*	m_pServProxy;		//the control server proxy
	int				m_nState;			//state: SS_CLOSED, SS_OPEN_WAIT, SS_ESTABLISHED.
public:
	//constructor
	CServerSession() : m_pServProxy(NULL), m_nState(SS_CLOSED){}
	CServerSession( const CSessionAddr& addr ){
		m_addrServ		= addr;
		m_pServProxy	= NULL;
		m_nState		= SS_CLOSED;
	}
	//property.
	const CSessionAddr& GetAddr(){ return m_addrServ; }
	const CSessionAddr& GetClntAddr(){ return m_addrClnt; }
	CServerProxy* GetServerProxy(){ return m_pServProxy; }
	
	int GetState(){ return m_nState; }
	bool IsClosed(){ return m_nState==SS_CLOSED; }
	bool IsEstablished(){ return m_nState==SS_ESTABLISHED; }
	bool IsOpenWait(){ return m_nState==SS_OPEN_WAIT; }

	//virtual functions
	virtual void PostMessage( CMessageHead* pMsg ) = 0;		//note PostMessage declare the ownership of pMsg. So pMsg must be dynamically allocated.

	//open and close.
	virtual bool Open( CServerProxy* pServProxy, const CSessionAddr& addrClnt ) = 0;
	virtual bool Close() = 0;

	virtual void WaitForObject(){ return; }
};

class CServerSessionStub : public CServerSession
{
protected:
	CSessionAddr		m_addrPeer;			//the peer server session address at remote side.
	CProxySocket*		m_pProxySock;		//the proxy socket for peer connection
public:
	//constructor
	CServerSessionStub( const CSessionAddr& addr, IN_ADDR addrPeer ) :  CServerSession(addr), m_addrPeer(addrPeer,0){
		m_pProxySock = NULL;
	}
	//property
	const CSessionAddr& GetPeerAddr(){ return m_addrPeer; }
	void SetPeerAddr( const CSessionAddr& addr ){ m_addrPeer=addr; }
	CProxySocket* GetProxySocket(){ return m_pProxySock; }

	//helper function for sending message
	void SendPeerMessage( CMessageHead* pMsg ){
		pMsg->Send( *m_pProxySock );
	}

//	void ProcessMessage( CMessageHead* pMsg );

	//reflect the messages returned from server to the client.
	void PostReturnMessage( CMessageHead* pMsg );

	//virtual functions
	virtual void PostMessage( CMessageHead* pMsg );		//note PostMessage declare the ownership of pMsg. So pMsg must be dynamically allocated.

	//open and close.
	void Open2();
	virtual bool Open( CServerProxy* pServProxy, const CSessionAddr& addrClnt );
	virtual bool Close();
};


class CServerSessionLocal : public CServerSession
{
private:
	CMessageQueue	m_MsgQueue;
	HANDLE			m_hSessionThread;
public:
	CServerSessionLocal( const CSessionAddr& addr ) : CServerSession(addr){
		m_hSessionThread = NULL;
	}
	~CServerSessionLocal(){
		WaitForObject();
	}
	//properties
	const CSessionAddr& GetAddr(){ return m_addrServ; }
	const CSessionAddr& GetClntAddr(){ return m_addrClnt; }

	//message loop.
	void PostQuitMessage();
	void PostMessage( CMessageHead* pMsg );
	CMessageHead* GetMessage();
	CMessageHead* GetMessage( int nCmd );
	void MessageLoop();
	//message processing.
	void ProcessMessage( CMessageHead* pMsg );
	void DefaultMessage( CMessageHead* pMsg );

	static int ServerSessionThread( void* arg );
	bool StartServerSession();

	virtual void WaitForObject();

	//command processing.
	virtual bool Open( CServerProxy* pServProxy, const CSessionAddr& addrClnt );
	virtual bool Close();

	//message handler
	void AckOpen();
};

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

class CServerApp
{
public:
	std::set< IN_ADDR >				m_HostAddrs;
	map<CSessionAddr, CProxyToken*> m_PeerTokens;
	map<CSessionAddr, CProxyToken*> m_ServTokens;

	std::list<CServerSession*>		m_SessionTockens;
	std::list<CServerProxy*>		m_ServerProxies;
	std::map<SOCKET, CServerProxy*> m_SockMap;

	HANDLE		m_hMutex;
	int			m_nStateFlags;
	CPU_INFO	m_CpuInfo;

	int			m_nNextSessionId;
	CSessionAddr m_addr;

	string m_strHosts;
public:
	void Lock(){ LockMutex( m_hMutex );	}
	void Unlock(){ UnlockMutex( m_hMutex );	}
public:
	CServerApp(){
		m_hMutex = CreateMutex();
		m_addr.m_addrHost = GetLocalHostAddr();
		m_addr.m_nServId = 0;
		m_nNextSessionId = rand();
	}
	~CServerApp();

	CServerSession* RetrieveServerSession();
	void ReturnServerSession( CServerSession* pServSession );

	CServerProxy* CreateServerProxy( SOCKET sock );
	void DestroyServerProxy( CServerProxy* pServProxy );

	const CSessionAddr& GetAddr(){ return m_addr; }
	int GetNextSessionId(){ return m_nNextSessionId++; }

	bool IsProxy(){ return true; }
	bool IsServer(){ return true; }

	void Broadcast( SOCKET sock, CMessageHead* pMsg );

	//poll all the registered hosts
	void DoPoll( SOCKET sock ){
		CObjLocker<CServerApp> locker(this);
		CMessageHead msg(NCM_POLLSERVER);
		msg.hton();
		SOCKADDR_IN dstaddr;
		dstaddr.sin_family = AF_INET;
		dstaddr.sin_port = htons( NCP_SERVER_PING_PORT );

		std::set<IN_ADDR>::iterator iter;
		for( iter=m_HostAddrs.begin(); iter!=m_HostAddrs.end(); ++iter ){
			dstaddr.sin_addr = *iter;
			sendto( sock, (char*)&msg, msg.GetLength(), 0, (SOCKADDR*)&dstaddr, sizeof(SOCKADDR_IN) );
		}
	}
	//acknowledge the poll message from addr
	void AckPoll( SOCKET sock, SOCKADDR_IN& addr ){
		CObjLocker<CServerApp> locker(this);
		int nLength = sizeof(CMessageHead)+m_ServTokens.size()*sizeof(TOKEN_INFO);
		CMessageHead* pMsg = (CMessageHead*)new char[nLength];
		TOKEN_INFO* pinfo = (TOKEN_INFO*)(pMsg+1);
		CMessageTrash trash(pMsg);

		pMsg->Init( NCF_ACK+NCM_POLLSERVER, GetAddr(), CSessionAddr(addr.sin_addr, 0), m_ServTokens.size() );
		map<CSessionAddr, CProxyToken*>::iterator iter;
		for( iter=m_ServTokens.begin(); iter!=m_ServTokens.end(); ++iter ){
			pinfo->nServId = iter->first.m_nServId;
			pinfo->nCpuType = iter->second->GetCpuType();
			pinfo->nCpuMhz = iter->second->GetCpuMhz();
			pinfo->hton();
			pinfo++;
		}
		pMsg->SetLength( nLength );
		pMsg->hton();
		sendto( sock, (char*)pMsg, pMsg->GetLength(), 0, (SOCKADDR*)&addr, sizeof(SOCKADDR_IN) );
	}

	void OnAckPoll( CMessageHead* pMsg ){
		CObjLocker<CServerApp> locker(this);
		TOKEN_INFO* pinfo = (TOKEN_INFO*)(pMsg+1);

		IN_ADDR addrHost = pMsg->GetSrcAddr().m_addrHost;
		//update the host set
		if( m_HostAddrs.find( addrHost )==m_HostAddrs.end() )m_HostAddrs.insert( addrHost );

		//mark all the existing idle tokens from the host as dead
		map<CSessionAddr, CProxyToken*>::iterator pos = m_PeerTokens.lower_bound( CSessionAddr(addrHost,0) );
		while( pos!=m_PeerTokens.end() && ( pos->second->GetPeerAddr().m_addrHost==addrHost) ){
			if( pos->second->IsIdle() )pos->second->SetState(CTS_DEAD);
			++pos;
		}

		//reset the tokens to idle
		int nInfos = pMsg->GetResult();
		for( int i=0; i<nInfos; i++ ){
			pinfo->ntoh();
			CSessionAddr addrPeer( pMsg->GetSrcAddr().m_addrHost, pinfo->nServId );
			pos = m_PeerTokens.find( addrPeer );
			if( pos==m_PeerTokens.end() ){
				CSessionAddr addrServ( GetAddr().m_addrHost, GetNextSessionId() );
				CProxyToken* pToken = new CProxyToken( addrServ, addrPeer, pinfo->nCpuType, pinfo->nCpuMhz );
				m_PeerTokens[pToken->GetPeerAddr()] = pToken;
				m_ServTokens[pToken->GetServAddr()] = pToken;
			}else{
				pos->second->SetState( CTS_IDLE );
			}
		}

		//remove all the dead tokens
		pos = m_PeerTokens.lower_bound( CSessionAddr(addrHost,0) );
		while( pos!=m_PeerTokens.end() && ( pos->second->GetPeerAddr().m_addrHost==addrHost) ){
			CProxyToken* pToken = pos->second;
			++pos;

			if( pToken->IsDead() ){
				m_PeerTokens.erase( pToken->GetPeerAddr() );
				m_ServTokens.erase( pToken->GetServAddr() );
			}
		}
	}

	void RunServer();
	static int PollThread( void* arg );
};

/*class CServerApp
{
private:
	map<SOCKET, CServerRouter*> m_SockMap;
//	list< CServerRouter* >


	CAvlSessionLocks m_avlSessionLocks;
	list<HANDLE> m_lstProcHandles;
	HANDLE m_hMutex;
	int m_nStateFlags;
	int m_nNextSessionId;		//the next allocatable session id
protected:
	void Lock(){
		LockMutex( m_hMutex );
	}
	void Unlock(){
		UnlockMutex( m_hMutex );
	}
	void SetPriority( int nStateFlags );			//set the running processes priority
	int State2Priority(int nStateFlags );			//compute priorit class by stateflags			
public:
	CServerApp(){
		m_hMutex = CreateMutex();
		m_nStateFlags = SYSTEM_IDLE;				//sys free.
	}
	~CServerApp(){
		//DestroyMutex( m_hMutex );
		CloseHandle( m_hMutex );
	}

	CSessionLocker* GetSessionLocker( const char* strUser );
	void ReleaseSessionLocker( CSessionLocker* pLocker );

	int AllocNextSessionId();

	void RegistProc( HANDLE hProc );
	void UnregistProc( HANDLE hProc );

	void SetServState( int nStateFlags );	//set the server state flags.
	int GetServState();						//get the server state flags.
	int GetPriority();						//get the default process running priority

	void KillProcs();			//kill all the running processes

	int GetSessionCount();
	int GetProcCount();

	void RunServer();
	static int SessionThread( void* arg );
	static int PingServThread( void* arg );

#ifndef _WIN32 
#ifdef _SHARED_DISK
	static int CServerApp::MonitorThread( void* arg );
#endif 
#endif

};*/