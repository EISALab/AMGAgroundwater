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

#define CAF_IDLE				0x00
#define CAF_USER_LOGON			0x01
#define CAF_USER_LOCK			0x02
#define CAF_SCREEN_SAVER		0x04
#define CAF_SUSPEND_SERVICE		0x08

#define CAF_AS_PROXY			0x80000000
#define CAF_AS_SERVER			0x40000000

inline bool IsSystemIdle( int nSysStatFlags )
{
	return isclr(nSysStatFlags, CAF_SUSPEND_SERVICE) && (
				isclr(nSysStatFlags, CAF_USER_LOGON) || isset(nSysStatFlags, CAF_USER_LOCK) ||
				( isset(nSysStatFlags, CAF_USER_LOGON) && isset(nSysStatFlags, CAF_SCREEN_SAVER) )
			);
}

inline DWORD MapProcessPriority( int nStateFlags )
{
    DWORD dwPriorityClass;
	if( isset(nStateFlags, CAF_SUSPEND_SERVICE) ){
		dwPriorityClass=-1;
	}else{
		bool bSystemFree = IsSystemIdle( nStateFlags );
		dwPriorityClass = bSystemFree ? NORMAL_PRIORITY_CLASS : IDLE_PRIORITY_CLASS;
	}

	return dwPriorityClass;
}

class CServerApp
{
public:
	std::set< IN_ADDR >				m_HostAddrs;
	std::map<CSessionAddr, CProxyToken*> m_PeerTokens;
	std::map<CSessionAddr, CProxyToken*> m_ServTokens;
	std::list<CServerProxy*>		m_ServerProxies;

	HANDLE		m_hMutex;
//	int			m_nFlags;
	int			m_nNextSessionId;
	int			m_nStateFlags;
	CSessionAddr m_addr;

	HANDLE		m_hPollThread;
	string		m_strHostFile;
public:
	void Lock(){ LockMutex( m_hMutex );	}
	void Unlock(){ UnlockMutex( m_hMutex );	}
public:
	CServerApp(){
		m_hMutex = CreateMutex();
//		m_nFlags = CAF_AS_SERVER;
		m_nNextSessionId = rand();
		m_strHostFile = "";
		m_nStateFlags = CAF_IDLE  | CAF_AS_SERVER;
	}
	~CServerApp();

	void LoadHostFile( const char* strNodeFile );

	CProxyToken* RetrieveToken( const CSessionAddr& addrServ );
	void ReturnToken( CProxyToken* pToken );

//	CServerSession* RetrieveServerSession( const CSessionAddr& addrServ );
//	void ReturnServerSession( CServerSession* pServSession );

	CServerProxy* CreateServerProxy( SOCKET sock );
	void DestroyServerProxy( CServerProxy* pServProxy );

	const CSessionAddr& GetAddr(){ return m_addr; }
	IN_ADDR GetLocalInAddr(){ return m_addr.m_addrHost; }
	int GetNextSessionId(){ return m_nNextSessionId++; }

	void SetStateFlags( int nStateFlags ){ m_nStateFlags = nStateFlags; }
	int GetStateFlags(){ return m_nStateFlags; }
	void SetAsProxy( bool bAsProxy )
	{
		if( bAsProxy)setbit(m_nStateFlags, CAF_AS_PROXY);
		else clrbit(m_nStateFlags, CAF_AS_PROXY);
	}
	void SetAsServer( bool bAsServer )
	{
		if( bAsServer )setbit(m_nStateFlags, CAF_AS_SERVER);
		else clrbit(m_nStateFlags, CAF_AS_SERVER);
	}
	bool IsAsProxy(){ return isset(m_nStateFlags, CAF_AS_PROXY); }
	bool IsAsServer(){ return isset(m_nStateFlags, CAF_AS_SERVER); }

	//helper function to query the machine system states
	bool IsSuspended(){ return isset(m_nStateFlags, CAF_SUSPEND_SERVICE); }
	DWORD GetSchedulePriority(){ return MapProcessPriority(m_nStateFlags); }

	void SetSysLockBit( bool bSet ){
		DWORD dwOldPriority = GetSchedulePriority();
		if( bSet )setbit(m_nStateFlags, CAF_USER_LOCK);
		else clrbit(m_nStateFlags, CAF_USER_LOCK);

		if( dwOldPriority!=GetSchedulePriority() )AdjustSchedulePriority();
	}
	void SetSysLogonBit( bool bSet ){
		DWORD dwOldPriority = GetSchedulePriority();
		if( bSet )setbit(m_nStateFlags, CAF_USER_LOGON);
		else clrbit(m_nStateFlags, CAF_USER_LOGON);

		if( dwOldPriority!=GetSchedulePriority() )AdjustSchedulePriority();
	}
	void SetSysScreenSaverBit( bool bSet ){
		DWORD dwOldPriority = GetSchedulePriority();
		if( bSet )setbit(m_nStateFlags, CAF_SCREEN_SAVER);
		else clrbit(m_nStateFlags, CAF_SCREEN_SAVER);

		if( dwOldPriority!=GetSchedulePriority() )AdjustSchedulePriority();
	}
	void SetSysSuspendBit( bool bSet ){
		DWORD dwOldPriority = GetSchedulePriority();
		if( bSet )setbit(m_nStateFlags, CAF_SUSPEND_SERVICE);
		else clrbit(m_nStateFlags, CAF_SUSPEND_SERVICE);

		if( dwOldPriority!=GetSchedulePriority() )AdjustSchedulePriority();
	}
	void AdjustSchedulePriority();

//	int GetState(){ return m_nState; }
//	void SetState( int nState );

	//poll all the registered hosts
	void DoPoll( SOCKET sock );

	//acknowledge the poll message from addr
	void AckPoll( SOCKET sock, SOCKADDR_IN& addr );

	void OnAckPoll( CMessageBase* pMsg );

	void RunServer();
	static int PollThread( void* arg );

	CMessageBase* GetMessage( SOCKET sock, SOCKADDR_IN& srcaddr );
	void ProcessMessage( CMessageBase* pMsg, SOCKET socksrv, const SOCKADDR_IN& srcaddr );

#ifndef _WIN32
	static int LoginMonitorThread( void* arg );
	void StartLoginMonitorThread();
#endif

	int GetRunningProcesses();
	int GetRunningSessions(){ return m_PeerTokens.size() - m_ServTokens.size(); }

};


class CServerProxy
{
private:
	HANDLE	m_hMutexSock;
	HANDLE	m_hMutex;
	CServerApp* m_pServApp;

	SOCKET	m_sock;
	CSessionAddr m_addrProxy;

	list< CMessageBase* >	m_WaitingMessages;
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
	CServerApp* GetServerApp(){ return m_pServApp; }

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

	template< typename MSG_TYPE >
	bool SendMessage( MSG_TYPE* pMsg ){
		CSockLocker<CServerProxy> locker(this);
		if( m_sock==0 )return false;

		::SendMessage( m_sock, pMsg );
		return true;
	}

	//Message loop.
	void RouteMessage();
	CMessageBase* GetMessage();

	//SendMessage may be blocked
	void DispatchMessage( CMessageBase* pMsg );
	void ProcessMessage( CMessageBase* pMsg );

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
	void OnUnreachable( CMessageBase* pMsg );
	void OnKeepAlive();

	//change the priority of the running classes
	void SetProcessPriorityClass(DWORD dwPriority);
	int GetRunningProcesses();
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
	bool IsLocal(){ return m_pProxyToken->m_addrPeer.m_addrHost==m_pServProxy->GetAddr().m_addrHost; }
	
	int GetState(){ return m_nState; }
	bool IsClosed(){ return m_nState==SS_CLOSED; }
	bool IsEstablished(){ return m_nState==SS_ESTABLISHED; }
	bool IsOpenWait(){ return m_nState==SS_OPEN_WAIT; }

	//virtual functions
	virtual void PostMessage( CMessageBase* pMsg ) = 0;		//note PostMessage declare the ownership of pMsg. So pMsg must be dynamically allocated.

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
	template< typename MSG_TYPE >
	void SendPeerMessage( MSG_TYPE* pMsg ){
		::SendMessage( (*m_pProxySock), pMsg );
	}

	void OpenStream( SOCKET sock );
	static int StreamProxyThread( void* arg );

//	void ProcessMessage( CMessageHead* pMsg );

	//reflect the messages returned from server to the client.
	void PostReturnMessage( CMessageBase* pMsg );

	//virtual functions
	virtual void PostMessage( CMessageBase* pMsg );		//note PostMessage declare the ownership of pMsg. So pMsg must be dynamically allocated.

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

	DWORD GetSchedulePriority(){
		return m_pServProxy->GetServerApp()->GetSchedulePriority();
	}

	//message loop.
	void PostQuitMessage();
	void PostMessage( CMessageBase* pMsg );
	CMessageBase* GetMessage();
	CMessageBase* WaitForMessage( int nMessage );
	void MessageLoop();

	//message processing.
	void ProcessMessage( CMessageBase* pMsg );
	void DefaultMessage( CMessageBase* pMsg );

	static int ServerSessionThread( void* arg );
	bool StartServerSession();

	//command processing.
	virtual bool Open( CServerProxy* pServProxy, const CSessionAddr& addrClnt );
	virtual bool Close();
	virtual void OpenStream( SOCKET sock );

	//message handler
	void AckOpen();
	void AckClose();

	//command handler
	void OnLogon( CMessageBase* pMsg );
	void OnSoftLink( CMessageBase* pMsg );
	void AckLogon( string& strUser, string& strPassword );
	void AckServerInfo();

//passive prepare a TCP stream.
	void AckPassive();
//file handlings
	//create a soft link, this works only for unix or linux system. 
	void AckSoftLink( const string& strSrc, const string& strLnk );
	//change file upload or download mode, ASCII or BINARY
	void AckFileMode( int nMode );
	//upload a file to the server's current directory
	void AckPutFile( const string& strFileName );
	//download a file from the server's current directory
	void AckGetFile( const string& strFileName );
	//chmod of a file access priviledge.
	void AckChMod( const string& strFileName, int nMode );
	//remove a file from the server
	void AckRemove( const string& strFileName );
	//run an image file on the file server
	void AckRunImage( const string& strCommand, bool bSync );
	//return a file's information. a NETF_STAT will be returned
	void AckFileInfo( const string& strFileName );

//directory operations
	//mkdir on the file server
	void AckMkDir( const string& strPath );
	//chdir, change the current directory
	void AckChDir( const string& strPath );
	//remove a directory on the file server. if bRecursive is true, the sub-folders and files will be erased
	void AckRmDir( const string& strPath );
	//remove but not remove a directory.
	void AckEmptyDir( const string& strPath );
	//list files or sub-folders of a directory
	void AckListDir( const string& strPath );

	SOCKET WaitStreamSocket();

	//helper function.
	string ParsePathMessage( CMessageBase* pMsg );
	string MakePath( const char* pPathName );
	void AckMessage( int nCmd, int nRet=S_OK, SOCKET sock=INVALID_SOCKET );

	HANDLE GetRunningProcess(){ return m_hRunningProcess; }
	friend int MonitorProcessThread( void* arg );
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