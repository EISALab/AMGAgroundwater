#include "netbase.h"
#include "ncphead.h"
#include "uwkelobj.h"
#include "msgqueue.h"
#include <map>
#include <set>
#include <vector>
#include "netstd.h"

#pragma once

class CClientSession;
class CClientProxy;
class CClientManager;

#define E_SOCK_CONNECT		0x8000001

class CClientProxy;

#define CSS_CLOSED			1
#define CSS_ESTABLISHED		2

typedef void (*EnumNetfEntryProc)( NETF_ENTRY* );

#define REMOTE_RET_CHECK( hRet )	if( FAILED(hRet) )throw new CRemoteException(hRet)

#define COPY_DONTCARE		0
#define COPY_NO_OVERWRITE	1
#define COPY_OLD_OVERWRITE	2

class CClientSession
{
private:
	CServerToken*	m_pServToken;		//the server token of the client session
	CClientProxy*	m_pClntProxy;		//the client proxy
	CSessionAddr	m_addr;				//the address of the client session
	CSessionAddr	m_addrServ;			//the server session address

	CMessageQueue	m_MsgQueue;			//the message queue
	int				m_nState;			//the state of the session CSS_CLOSED or CSS_ESTABLISHED
	int				m_nRetCode;			//the error code caused closing.
public:
	CClientSession( const CSessionAddr& addr, CServerToken* pServToken ): m_addr(addr), m_pServToken(pServToken){
		m_addrServ = pServToken->GetServAddr();
		m_addrServ.m_nServId = 0;
		m_nState = CSS_CLOSED;
		m_nRetCode = S_OK;
	}
	~CClientSession(){};

	const CSessionAddr& GetAddr(){ return m_addr; }
	void SetAddrId( int nId ){
		m_addr = CSessionAddr( GetLocalInAddr(), nId );
	}
	const CSessionAddr& GetServAddr(){ return m_addrServ; }
	void SetProxy( CClientProxy* pClntProxy ){ m_pClntProxy=pClntProxy; }
	CClientProxy* GetProxy(){ return m_pClntProxy; }
	int GetRetCode(){ return m_nRetCode; }
	CServerToken* GetToken(){ return m_pServToken; }
	void SetState( int nState ){ m_nState = nState; }
	bool IsEstablished(){ return m_nState==CSS_ESTABLISHED; }

	//message process.
	CMessageBase* GetMessage(){
		CMessageBase* pMsg = m_MsgQueue.GetMessage();
		if( pMsg->IsAcking() && pMsg->GetResult()==E_BROKENPIPE ){
			CMessageTrash trash(pMsg);
			throw new CRemoteException(pMsg->GetResult());
		}
		if( pMsg->GetMessage()==NCM_PROXYSHUTDOWN ){
			CMessageTrash trash(pMsg);
			throw new CRemoteException( E_BROKENPIPE );
		}

		return pMsg;
	}
	CMessageBase* WaitForMessage( int nMessage );
/*	CMessageBase* GetMessage( int nMessage ){
		CMessageBase* pMsg = m_MsgQueue.GetMessage();
		if( pMsg->GetMessage()!=nMessage ){
			CMessageTrash trash(pMsg);
			throw new CRemoteException(-1);
		}
		return pMsg;
	}*/
	void PostMessage( CMessageBase* pMsg ){
		m_MsgQueue.PutMessage( pMsg );
	}

	//open stream.
	SOCKET OpenStream( u_int nPort );

	//connect and disconnect.
	bool Open();
	bool Close();

	//logon the remote server
	HRESULT DoLogon( const string& strUser, const string& strPassword );
//directory operations
	//make a directory
	HRESULT DoMkDir( const string& strPath );
	//change the current directory
	HRESULT DoChDir( const string& strPath );
	//remove a directory.
	//bRecursive: if it is true, the server will recursively erase the sub-folders and files. 
	//Otherwise it just tries to remove the current directory, so the current directory should be empty
	HRESULT DoRmDir( const string& strPath );
	HRESULT DoEmptyDir( const string& strPath );
	//list a directory on the server. pEnumProc will be called on each NETF_ENTRY.
	HRESULT DoListDir( const string& strPath, EnumNetfEntryProc pEnumProc );
//return a passive stream channel
	HRESULT DoPassive( u_int& nPasPort );
//file opeartions
	//upload a file to the server
	//strSrcFile: Local file name on the computer
	//strDstFile: The file name that will be saved on the server side.
	HRESULT DoPutFile( const string& strSrcFile, const string& strDstFile );
	//download a file from the server
	//strSrcFile: The file name on the server side that will be downloaded.
	//strDstFile: the file name that will be saved on the local computer
	HRESULT DoGetFile( const string& strSrcFile, const string& strDstFile );
	//remove a file from the server
	HRESULT DoRemove( const string& strFileName );
	//change access privilege of a file
	HRESULT DoChMod( const string& strFileName, int nMode );
	//change the file upload or download mode (TEXT or BINARY)
	HRESULT DoFileMode( int nFileMode );
	//get information of a file.
	HRESULT DoFileInfo( const string& strFileInfo, NETF_STAT& fstat );
	//run a command (process) on the server, if bSync, the funtion will wait until the process returnes, and dwExitCode has the exit code of the process
	HRESULT DoCommand( const string& strCommand, bool bSync, DWORD& dwExitCode );

	HRESULT DoSoftLink( const string& strSrc, const string& strLnk );
//kill the running process at server
	HRESULT DoKillProcess();

	HRESULT DoServerInfo( SERVERINFO& info );

//help functions.
	bool CopyTo( const string& strSrc, const string& strDst, int how=COPY_DONTCARE );
	bool CopyFrom( const string& strSrc, const string& strDst, int how=COPY_DONTCARE );
	bool Remove( const string& strFileName ){ return SUCCEEDED(DoRemove(strFileName)); }
	bool EmptyDir( const string& strPath );
	bool Logon( const string& strUser, const string& strPassword );
	bool RunCommand( const string& strCommand, bool bSync, DWORD& dwExitCodt );
	bool ChDir( const string& strPath ){ return SUCCEEDED(DoChDir(strPath)); }
	bool MkDir( const string& strPath ){ return SUCCEEDED(DoMkDir(strPath)); }
	bool ListDir( const string& strPath, EnumNetfEntryProc pEnumProc ){ return SUCCEEDED(DoListDir(strPath, pEnumProc)); }
	bool RemoveDir( const string& strPath, bool bRecursive );
	bool ChMod( const string& strFileName, int nMode ){ return SUCCEEDED(DoChMod(strFileName, nMode)); }
	bool SoftLink( const string& strSrc, const string& strLnk ){ return SUCCEEDED(DoSoftLink(strSrc, strLnk)); }
	bool ServerInfo( SERVERINFO& info ){ return SUCCEEDED(DoServerInfo(info)); }
	bool FileMode( int nFileMode ){ return SUCCEEDED(DoFileMode(nFileMode)); }
};

class CClientProxy
{
private:
	HANDLE			m_hMutex;
	HANDLE			m_hMutexSock;
	IN_ADDR			m_addrServ;
	CSessionAddr	m_addr;
	CProxySocket	m_sock;
	HANDLE			m_hProxyThread;
	map<CSessionAddr, CClientSession*> m_ClntSessions;
	CClientManager*	m_pManager;
public:
	void Lock(){ LockMutex(m_hMutex); }
	void Unlock(){ UnlockMutex(m_hMutex); }
	void LockSock(){ LockMutex(m_hMutexSock); }
	void UnlockSock(){ UnlockMutex(m_hMutexSock); }
public:
	CClientProxy( CClientManager* pManager, const CSessionAddr& addr, IN_ADDR addrServ ):m_pManager(pManager), m_addr(addr), m_addrServ(addrServ){
		m_hMutex = CreateMutex();
		m_hMutexSock = CreateMutex();
		m_hProxyThread = NULL;
	}
	~CClientProxy(){
		CloseHandle( m_hMutex );
		CloseHandle( m_hMutexSock );
	}

	const CSessionAddr& GetAddr(){ return m_addr; }
	CClientManager* GetManager(){ return m_pManager; }

	void InsertSession( CClientSession* pClntSession ){
		CObjLocker<CClientProxy> locker(this);
		m_ClntSessions[pClntSession->GetAddr()] = pClntSession;
	}
	void RemoveSession( CClientSession* pClntSession ){
		CObjLocker<CClientProxy> locker(this);
		m_ClntSessions.erase( pClntSession->GetAddr() );
	}
	CClientSession* GetSession( const CSessionAddr& addr ){
		CObjLocker<CClientProxy> locker(this);
		map<CSessionAddr, CClientSession*>::iterator pos = m_ClntSessions.find( addr );
		if( pos!=m_ClntSessions.end() )return pos->second;
		return NULL;
	}
	bool IsEmpty(){ return m_ClntSessions.empty(); }
	bool IsSockError(){
		CSockLocker<CClientProxy> locker(this);
		return m_sock.IsError();
	}

	template< typename MSG_TYPE >
	bool SendMessage( MSG_TYPE* pMsg )
	{
		CSockLocker<CClientProxy> locker(this);
		::SendMessage( m_sock, pMsg );
		return true;
	}


	bool OpenChannel( CClientSession* pSession );
	bool CloseChannel( CClientSession* pSession );
	void Shutdown();
	void ShutdownSessions(){
		CMessageBase msg(NCM_PROXYSHUTDOWN);
		Broadcast( &msg );
	}
	void Broadcast( CMessageBase* pMsg )
	{
		CObjLocker<CClientProxy> locker(this);
		map<CSessionAddr, CClientSession*>::iterator pos = m_ClntSessions.begin();
		while( pos!=m_ClntSessions.end() ){
			pos->second->PostMessage( pMsg->Clone() );
			++pos;
		}
	}

	//kill a running process at the server session. The message is out of band message and no ACK returns.
	void KillProcess( const CSessionAddr& addrServ )
	{
		CMessageBase msg;
		msg.Init(NCM_KILLPROCESS, GetAddr(), addrServ );
		SendMessage( &msg );
	}

	void KeepAlive();
	void RouteMessage();

	static int ClientProxyThread( void* arg );
	bool StartClientProxy();
	void WaitForObject();
};

#define	CMRS_FASTEST	100
#define CMRS_SLOWEST	101
#define CMRS_RANDOM		102


class CClientManager
{
private:
	int		m_nNextSessionId;
	HANDLE	m_hMutex;
	HANDLE	m_hSema;
	HANDLE	m_hPollThread;
	multiset<CServerToken*, TokenCpuCmp> m_ServTokens;
	map<CSessionAddr, CServerToken*> m_PeerTokens;
	map<IN_ADDR, CClientProxy*> m_ClntProxies;
	set<IN_ADDR>				m_HostAddrs;
public:
	void Lock(){ LockMutex(m_hMutex); }
	void Unlock(){ UnlockMutex(m_hMutex); }
protected:
	CClientProxy* FindClntProxy( IN_ADDR addr ){
		CObjLocker<CClientManager> locker(this);
		map<IN_ADDR, CClientProxy*>::iterator pos = m_ClntProxies.find( addr );
		if( pos!=m_ClntProxies.end() )return pos->second;
		return NULL;
	}
	void InsertClntProxy( IN_ADDR addr, CClientProxy* pClntProxy ){
		CObjLocker<CClientManager> locker(this);
		m_ClntProxies[addr] = pClntProxy;
	}
	void RemoveClntProxy( IN_ADDR addr ){
		CObjLocker<CClientManager> locker(this);
		m_ClntProxies.erase( addr );
	}

public:
	CClientManager(){
		m_hMutex = CreateMutex();
		m_hSema  = CreateSemaphore();
		m_nNextSessionId = rand();
	}
	~CClientManager(){
		if( m_hPollThread )TerminateThread( m_hPollThread );
		CloseHandle( m_hMutex );
		CloseHandle( m_hSema );
	}
	int GetNextSessionId(){ return m_nNextSessionId++; }

	void AddToken( const CSessionAddr& addr, int nCpuType, int nCpuMhz );

	void AddHost( IN_ADDR addr );
	void AddHost( const char* strHost );
	void LoadHostFile( const char* strNodeFile );


	CClientSession* RetrieveSession( int nAlgorithm );
	void ReturnSession( CClientSession* pSession );

	//poll all the registered hosts
	void DoPoll( SOCKET sock, unsigned short sPort );

	void OnAckPoll( CMessageBase* pMsg );

	static int PollThread( void* arg );
	bool StartPollThread(){
		m_hPollThread = CreateThread( (THREAD_ROUTINE)PollThread, this, 0 );
		return true;
	}

	bool KillProcess( const CSessionAddr& addrServ )
	{
		CClientProxy* pProxy = FindClntProxy( addrServ.m_addrHost );
		if( pProxy ){
			pProxy->KillProcess( addrServ );
			return true;
		}
		return false;
	}
	void ShutdownServer( IN_ADDR addrHost, bool bForward=false );
};


/*class CClientSession
{
private:
	SOCKET			m_sock;
	CSessionAddr	m_addr;
	CSessionAddr	m_addrServ;
	int			m_nFileMode;		//file open mode
protected:
	int DoLogOn( const char* strUser, const char* strPass );
	int DoLogOff();
	//file operations
	int DoPassive( short* pport );
	int DoPutFile( const char* strSrc, const char* strDst, bool bOverwrite );
	int DoGetFile( const char* strSrc, const char* strDst );
	int DoRemFile( const char* strName );
//	int DoList();
	int DoChMod( const char* strName, int nMode );
	int DoSoftLink( const char* strSrc, const char* strLink ); 
	int DoFileMode( int nFileMode );

	//directory operations
	int DoMkDir( const char* strDir );
	int DoChDir( const char* strDir );
	int DoRmDir( const char* strDir );

	//other operations
	int DoRunImage( const char* strImage, bool bSync=true );
	int DoGetSysInfo( SYSINFO* pSysInfo );
	int DoKillProcs( );
	int DoSetIdle( BOOL bIdle );

public:
	CClientSession( );
	~CClientSession();

	const CSessionAddr& GetAddr(){ return m_addr; }
	void SetAddrId( int nId ){
		m_addr = CSessionAddr( GetLocalHostAddr(), nId );
	}
	const CSessionAddr& GetServAddr(){ return m_addrServ; }

	//connect and disconnect.
	int Connect( SOCKADDR_IN* srvAddr, const char* strUser, const char* strPass );
	int Disconnect();
	void KillConnect();

	//file operation
	int PutFile( const char* strSrc, const char* strDst, bool bOverwrite=true );
	int GetFile( const char* strSrc, const char* strDst );
	int Remove( const char* strName );
	int ChMod( const char* strName, int nMode );
	int SoftLink( const char* strSrc, const char* strLink );	//create a link linking to strSrc.
	int FileMode( int nFileMode );

	//directory operation
	int ChDir( const char* strDir );
	int MkDir( const char* strDir );
	int RmDir( const char* strDir );

	//run executable.
	int RunImage( const char* strImage, bool bSync=true );
	//system information
	int GetSysInfo( SYSINFO* pSysInfo );
	//kill all waiting processes
	int KillProcs( );
	int SetIdle( BOOL bIdle );
};*/

