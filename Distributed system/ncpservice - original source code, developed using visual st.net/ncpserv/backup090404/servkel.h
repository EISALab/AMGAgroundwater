#include "../uwkelobj.h"
#include <list>
#include <map>

class CMessageQueue;
class CMessageRouter;
class CServerSession;

class CMessageQueue
{
private:
	HANDLE m_hSema;
	HANDLE m_hMutex;
	std::list< CMessageHead* > m_MsgQueue;
public:
	class CMessageQueueLocker
	{
	private:
		CMessageQueue* m_pMsgQueue;
	public:
		CMessageQueueLocker( CMessageQueue* pMsgQueue ):m_pMsgQueue(pMsgQueue){
			m_pMsgQueue->Lock();
		}
		~CMessageQueueLocker(){
			m_pMsgQueue->Unlock();
		}
	};
public:
	CMessageQueue(){
		m_hMutex = CreateMutex();
		m_hSema = CreateSemaphore();
	};
	~CMessageQueue(){
		CloseHandle( m_hMutex );
		CloseHandle( m_hSema );
	}
	void Lock(){
		LockMutex( m_hMutex );
	}
	void Unlock(){
		UnlockMutex( m_hMutex );
	}
	void Enqueue( CMessageHead* pMsg ){
		CMessageQueueLocker locker(this);
		m_MsgQueue.push_back( pMsg );
	}
	CMessageHead* Dequeue(){
		CMessageQueueLocker locker(this);
		CMessageHead* pMsg = m_MsgQueue.front();
		m_MsgQueue.pop_front();
		return pMsg;
	}
public:
	CMessageHead* GetMessage(){
		WaitForSemaphore( m_hSema );
		return Dequeue();
	}
	void PutMessage( CMessageHead* pMsg ){
		Enqueue( pMsg );
		ReleaseSemaphore( m_hSema );
	}
	bool IsEmpty(){
		CMessageQueueLocker locker(this);
		return m_MsgQueue.empty();
	}
	int GetSize(){
		CMessageQueueLocker locker(this);
		return (int)m_MsgQueue.size();
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

class CServerApp;
class CServerProxy;
class CServerSession;

class CServerProxy
{
private:
	HANDLE	m_hMutexSock;
	HANDLE	m_hMutex;
	CServerApp* m_pServApp;

	std::list< CServerSession* > m_ServSessions;
	SOCKET	m_sock;
	CSessionAddr m_addrProxy;

	HANDLE m_hProxyThread;
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

	CServerSession* GetServerSession( const CSessionAddr& addr );

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

class CServerSession
{
private:
	CSessionAddr	m_addrClnt;
	CSessionAddr	m_addrServ;

	CServerProxy*	m_pServProxy;
	CMessageQueue	m_MsgQueue;

	HANDLE			m_hSessionThread;
public:
	CServerSession( const CSessionAddr& addr ){
		m_addrServ = addr;
		m_pServProxy = NULL;
		m_hSessionThread = NULL;
	}
//	CMessageQueue* GetMessageQueue(){
//		return &m_MsgQueue;
//	}
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
	void WaitForObject();

	//command processing.
	bool Open( CServerProxy* pServProxy, const CSessionAddr& addrClnt );
	bool Close();
};

class CServerApp
{
public:
	std::list<CServerSession*>		m_SessionTockens;
	std::list<CServerProxy*>		m_ServerProxies;
	std::map<SOCKET, CServerProxy*> m_SockMap;

	HANDLE m_hMutex;
	int m_nStateFlags;
public:
	void Lock(){ LockMutex( m_hMutex );	}
	void Unlock(){ UnlockMutex( m_hMutex );	}
public:
	CServerApp(){
		m_hMutex = CreateMutex();
	}
	~CServerApp();

	CServerSession* RetrieveServerSession();
	void ReturnServerSession( CServerSession* pServSession );

	CServerProxy* CreateServerProxy( SOCKET sock );
	void DestroyServerProxy( CServerProxy* pServProxy );

	void RunServer();
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