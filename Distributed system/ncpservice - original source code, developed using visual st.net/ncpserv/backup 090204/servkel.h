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

class CServerRouter
{
private:
	HANDLE	m_hMutex;
	int		m_nNextServId;

	std::list< CServerSession* > m_ServSessions;
	CMessageQueue m_MsgQueue;
	SOCKET	m_skMsg;
	HANDLE	m_hRouterThread;
public:
	void LockSock(){
		LockMutex( m_hMutex );
	}
	void UnlockSock(){
		UnlockMutex( m_hMutex );
	}
public:
	CServerRouter( SOCKET skMsg ){
		m_nNextServId = rand();
		m_skMsg = skMsg;
		m_hRouterThread = NULL;
	}
	int GetNextServId(){
		return m_nNextServId++;
	}
	CMessageQueue* GetMessageQueue(){
		return &m_MsgQueue;
	}
	CServerSession* GetServerSession( int nServId );
	int GetServId(){ return -1; }

	//Message loop.
	void RouteMessage();
	CMessageHead* GetMessage();
	bool DispatchMessage( CMessageHead* pMsg );


	//SendMessage may be blocked
	void SendMessage( CMessageHead* pMsg );
	void PostMessage( CMessageHead* pMsg );
	bool ProcessMessage( CMessageHead* pMsg );


	//process functin call
	bool OpenServSession( int nClntId );
	bool CloseServSession( int nServId );

	//thread
	static int RouterThread( void* arg );
	bool StartRouter();
	void WaitRouter();
};

class CServerSession
{
private:
	int				m_nServId;
	int				m_nClntId;
	CServerRouter*	m_pServRouter;
	CMessageQueue	m_MsgQueue;

	HANDLE			m_hSessionThread;
public:
	CServerSession( CServerRouter* pServRouter, int nServId, int nClntId ){
		m_pServRouter = pServRouter;
		m_nServId = nServId;
		m_nClntId = nClntId;
		m_hSessionThread = NULL;
	}
	CMessageQueue* GetMessageQueue(){
		return &m_MsgQueue;
	}
	int GetServId(){ return m_nServId; }
	int GetClntId(){ return m_nClntId; }

	void SendMessage( CMessageHead* pMsg );
	void PostMessage( CMessageHead* pMsg );

	void MessageLoop();
	CMessageHead* GetMessage();
	bool ProcessMessage( CMessageHead* pMsg );

	bool StartSession();
	void WaitSession();
	static int SessionThread( void* arg );

	//command processing.
	bool Close();
};

class CServerApp
{
private:
	std::map<SOCKET, CServerRouter*> m_SockMap;

	HANDLE m_hMutex;
	int m_nStateFlags;
protected:
	void Lock(){
		LockMutex( m_hMutex );
	}
	void Unlock(){
		UnlockMutex( m_hMutex );
	}
public:
	CServerApp(){
		m_hMutex = CreateMutex();
	}
	~CServerApp(){
		//DestroyMutex( m_hMutex );
		CloseHandle( m_hMutex );
	}

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