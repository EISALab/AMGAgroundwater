#include "uwkelobj.h"
#include <list>
#include <set>
#include <map>
#include <vector>
#include "netstd.h"
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
	u_short		m_nPasPort;	//the port number for passive server.
protected:
	bool MessageFromStream( SOCKET sock );
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

		cdump<<"proxy was created now"<<endl;
	}
	~CServerProxy(){
		CloseHandle( m_hMutexSock );
		CloseHandle( m_hMutex );
		cdump<<"proxy was closed"<<endl;
	}

	const CSessionAddr& GetAddr(){
		return m_addrProxy;
	}
	u_short GetPassivePort(){ return m_nPasPort; }

	CProxySocket* GetPeerSocket( IN_ADDR addr ){
		std::map<IN_ADDR, CProxySocket*>::iterator pos = m_PeerSocks.find( addr );
		if( pos!=m_PeerSocks.end() )return pos->second;
		return NULL;
	}
	CProxySocket* FindPeerSocket( SOCKET sock ){
		std::map<IN_ADDR, CProxySocket*>::iterator pos;
		for( pos=m_PeerSocks.begin(); pos!=m_PeerSocks.end(); ++pos ){
			if( sock==*(pos->second) )return pos->second;
		}
		return NULL;
	}
	void InsertPeerSocket( IN_ADDR addr, CProxySocket* pProxySock ){
		m_PeerSocks[addr] = pProxySock;
	}
	void RemovePeerSocket( IN_ADDR addr ){
		std::map<IN_ADDR, CProxySocket*>::iterator pos = m_PeerSocks.find( addr );
		if( pos!=m_PeerSocks.end() )m_PeerSocks.erase( pos );
	}

	CServerSession* GetSession( const CSessionAddr& addr );
	void InsertSession( CServerSession* pServSession );
	void RemoveSession( const CSessionAddr& addr );

	bool SendMessage( CMessageHead* pMsg );

	//Message loop.
	void RouteMessage();
	CMessageHead* GetMessage();

	//SendMessage may be blocked
	void DispatchMessage( CMessageHead* pMsg );
	void ProcessMessage( CMessageHead* pMsg );

	//process functin call
//	void OpenServerSession( const CSessionAddr& addrClnt, const CSessionAddr& addrServ );
//	void CloseServerSession( CServerSession* pServSession );

	//thread
	static int ServerProxyThread( void* arg );
	bool StartServerProxy();
	void WaitForObject();

	//enforce the channel to shutdown
	void Shutdown();

	//shutdown all the active sessions and stubs.
	void ShutdownSessions();
	//shutdown the stubs associated with the stub socket.
	void ShutdownStubs( SOCKET sock );

	//process functin call
	CServerSession* CreateSession( const CSessionAddr& addrServ );
	void DestroySession( CServerSession* pServSession );

	//message processing handlers.
	void OnOpenSession( const CSessionAddr& addrClnt, const CSessionAddr& addrServ );
	void OnUnreachable( CMessageHead* pMsg );
};


#define SS_CLOSED		-1
#define SS_OPEN_WAIT	1
#define SS_ESTABLISHED	0

class CServerSession
{
protected:
	CProxyToken*	m_pProxyToken;		//the token of the session
	CSessionAddr	m_addrClnt;			//client address
	CSessionAddr	m_addrServ;			//server address

	CServerProxy*	m_pServProxy;		//the control server proxy
	int				m_nState;			//state: SS_CLOSED, SS_OPEN_WAIT, SS_ESTABLISHED.
public:
	//constructor
	CServerSession() : m_pProxyToken(NULL), m_pServProxy(NULL), m_nState(SS_CLOSED){}
	CServerSession( CProxyToken* pProxyToken ){
		m_pProxyToken	= pProxyToken;
		m_addrServ		= pProxyToken->GetServAddr();
		m_pServProxy	= NULL;
		m_nState		= SS_CLOSED;
	}
	virtual ~CServerSession(){}
	//property.
	const CSessionAddr& GetAddr(){ return m_addrServ; }
	const CSessionAddr& GetClntAddr(){ return m_addrClnt; }
	CServerProxy* GetServerProxy(){ return m_pServProxy; }
	CProxyToken* GetProxyToken(){ return m_pProxyToken; }
	
	int GetState(){ return m_nState; }
	bool IsClosed(){ return m_nState==SS_CLOSED; }
	bool IsEstablished(){ return m_nState==SS_ESTABLISHED; }
	bool IsOpenWait(){ return m_nState==SS_OPEN_WAIT; }

	//virtual functions
	virtual void PostMessage( CMessageHead* pMsg ) = 0;		//note PostMessage declare the ownership of pMsg. So pMsg must be dynamically allocated.

	//open and close.
	virtual bool Open( CServerProxy* pServProxy, const CSessionAddr& addrClnt ) = 0;
	virtual bool Close() = 0;
	virtual void OpenStream( SOCKET sock ) = 0;

//	virtual void WaitForObject(){ return; }
};

class CServerSessionStub : public CServerSession
{
protected:
	CSessionAddr		m_addrPeer;			//the peer server session address at remote side.
	CProxySocket*		m_pProxySock;		//the proxy socket for peer connection
	u_short				m_nPeerPasPort;		//the passive port number at peer side
public:
	//constructor
	CServerSessionStub( CProxyToken* pProxyToken ):CServerSession(pProxyToken){
		m_addrPeer = pProxyToken->GetPeerAddr();
		m_addrPeer.m_nServId = 0;
		m_pProxySock = NULL;
	}
	virtual ~CServerSessionStub(){}
	//property
	const CSessionAddr& GetPeerAddr(){ return m_addrPeer; }
	void SetPeerAddr( const CSessionAddr& addr ){ m_addrPeer=addr; }
	CProxySocket* GetProxySocket(){ return m_pProxySock; }

	//helper function for sending message
	void SendPeerMessage( CMessageHead* pMsg ){
		pMsg->Send( *m_pProxySock );
	}

	void OpenStream( SOCKET sock );
	static int StreamProxyThread( void* arg );

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

	//home path and current path.
	string			m_strHomePath;
	string			m_strCurPath;
	//upload and download file mode (binary or text)
	int				m_nFileMode;
	//image process handle
	HANDLE			m_hRunningProcess;
public:
	CServerSessionLocal( CProxyToken* pProxyToken ) : CServerSession(pProxyToken){
		m_hSessionThread = NULL;
		m_nFileMode = O_BINARY;
		m_hRunningProcess = 0;
	}
	virtual ~CServerSessionLocal(){
		m_hSessionThread = 0;
//		ASSERT( m_hSessionThread==NULL );
//		WaitForObject();
	}
	//properties
	const CSessionAddr& GetAddr(){ return m_addrServ; }
	const CSessionAddr& GetClntAddr(){ return m_addrClnt; }

	//message loop.
	void PostQuitMessage();
	void PostMessage( CMessageHead* pMsg );
	CMessageHead* GetMessage();
//	CMessageHead* GetMessage( int nCmd );
	CMessageHead* WaitForMessage( int nMessage );
	void MessageLoop();
	//message processing.
	void ProcessMessage( CMessageHead* pMsg );
	void DefaultMessage( CMessageHead* pMsg );

	static int ServerSessionThread( void* arg );
	bool StartServerSession();

//	virtual void WaitForObject();

	//command processing.
	virtual bool Open( CServerProxy* pServProxy, const CSessionAddr& addrClnt );
	virtual bool Close();
	virtual void OpenStream( SOCKET sock );

	//message handler
	void AckOpen();
	void AckClose();

	//command handler
	void OnLogon( CMessageHead* pMsg );
	void OnSoftLink( CMessageHead* pMsg );
	void AckLogon( string& strUser, string& strPassword );

	//passive and file handling.
	void AckPassive();
	void AckSoftLink( const string& strSrc, const string& strLnk );
	void AckPutFile( const string& strFileName, bool bOverWrite );
	void AckGetFile( const string& strFileName );
	void AckRunImage( const string& strCommand, bool bSync );
	void AckFileMode( int nMode );
	void AckFileInfo( const string& strFileName );

	//directory operations
	void AckMkDir( const string& strPath );
	void AckChDir( const string& strPath );
	void AckRmDir( const string& strPath, bool bRecursive );
	void AckListDir( const string& strPath );

	//helper function.
	string ParsePathMessage( CMessageHead* pMsg );
	string MakePath( const char* pPathName );
	void AckMessage( int nCmd, int nRet=S_OK, SOCKET sock=INVALID_SOCKET );

friend int MonitorProcessThread( void* arg );
};

#define CAF_AS_PROXY		0x00000001
#define CAF_AS_SERVER		0x00000002

class CServerApp
{
public:
	std::set< IN_ADDR >				m_HostAddrs;
	std::map<CSessionAddr, CProxyToken*> m_PeerTokens;
	std::map<CSessionAddr, CProxyToken*> m_ServTokens;

//	std::list< CServerSession* >	m_SessionRecycle;
//	std::list<CServerSession*>		m_SessionTockens;
	std::list<CServerProxy*>		m_ServerProxies;
//	std::map<SOCKET, CServerProxy*> m_SockMap;

	HANDLE		m_hMutex;
	int			m_nFlags;
	int			m_nNextSessionId;
	CSessionAddr m_addr;

	HANDLE		m_hPollThread;
	string		m_strHostFile;
public:
	void Lock(){ LockMutex( m_hMutex );	}
	void Unlock(){ UnlockMutex( m_hMutex );	}
public:
	CServerApp(){
		m_hMutex = CreateMutex();
		m_nFlags = CAF_AS_SERVER;
		m_nNextSessionId = rand();
		m_strHostFile = "";
	}
	~CServerApp();

	CProxyToken* RetrieveToken( const CSessionAddr& addrServ );
	void ReturnToken( CProxyToken* pToken );

//	CServerSession* RetrieveServerSession( const CSessionAddr& addrServ );
//	void ReturnServerSession( CServerSession* pServSession );

	CServerProxy* CreateServerProxy( SOCKET sock );
	void DestroyServerProxy( CServerProxy* pServProxy );

	const CSessionAddr& GetAddr(){ return m_addr; }
	int GetNextSessionId(){ return m_nNextSessionId++; }

	void SetFlags( int nFlags ){ m_nFlags = nFlags; }
	bool IsProxy(){ return isset(m_nFlags, CAF_AS_PROXY); }
	bool IsServer(){ return isset(m_nFlags, CAF_AS_SERVER); }

	//poll all the registered hosts
	void DoPoll( SOCKET sock );

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
		pMsg->SendTo( sock, addr );
		//pMsg->hton();
		//sendto( sock, (char*)pMsg, pMsg->GetLength(), 0, (SOCKADDR*)&addr, sizeof(SOCKADDR_IN) );
	}

	void OnAckPoll( CMessageHead* pMsg );

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