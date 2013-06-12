#include "ncphead.h"
#include "netstd.h"
#include "uwkelobj.h"
#include "ncpjob.h"
#include "ofdump.h"

///////////////////////////////////////////////////////////////////////////////////////////////////////////
//											CJobObject
///////////////////////////////////////////////////////////////////////////////////////////////////////////

CJobObject::CJobObject( int nId, FUNCJOB pfnExc, void* pParam, double rCpuHint, CJobSet* pJobSet )
{
	m_nId = nId;
	m_pfnExc = pfnExc;
	m_pParam = pParam;
	m_rCpuHint = rCpuHint;

	m_nFailCount = 0;
	m_tmStart = m_tmStop = 0;
	bzero( &m_addrHost, sizeof(IN_ADDR) );
	m_pJobSet = pJobSet;
}

bool CJobObject::Launch( CClientSession* pSession )
{
	bool bOk = true;
	//create the session if it not exists.
	try{
		char buf[_MAX_LINE];
		sprintf( buf, "opening %d for job %d", pSession->GetToken()->GetServAddr().m_nServId, m_nId );
		cdump<<lock<<buf<<endl<<unlock;

		bOk = pSession->Open();
		sprintf( buf, "%s(%d) for job %d", GetHostByAddr(pSession->GetServAddr().m_addrHost).c_str(), pSession->GetToken()->GetServAddr().m_nServId, m_nId );
		cdump<<lock<<buf<<" opened "<<bOk<<endl<<unlock;
		if( !bOk ){
//			cdump<<lock<<pSession->GetAddr().m_nServId<<"open failed"<<endl<<unlock;
		}

		if( bOk ){
			m_addrHost = pSession->GetServAddr().m_addrHost;

			SERVERINFO sysinfo;
			pSession->ServerInfo( sysinfo );
	//		if( isset(sysinfo.nSysStat, SYSTEM_SUSPEND_SERVICE) ){
	//			return false;
	//		}

			bOk = (*m_pfnExc)( m_nId, pSession, m_pParam );
		}

		pSession->Close();

		sprintf( buf, "%s(%d)", GetHostByAddr(pSession->GetServAddr().m_addrHost).c_str(), pSession->GetServAddr().m_nServId );
		cdump<<lock<<buf<<" closed"<<endl<<unlock;
	}catch( CSockException* e){
		char buf[_MAX_LINE];
		sprintf( buf, "%s(%d)", GetHostByAddr(pSession->GetServAddr().m_addrHost).c_str(), pSession->GetServAddr().m_nServId );
		cdump<<lock<<buf<<" Sock Exception:"<<e->what()<<endl<<unlock;

		e->Delete();
		bOk = false;
	}catch( CRemoteException* e ){
		char buf[_MAX_LINE];
		sprintf( buf, "%s(%d)", GetHostByAddr(pSession->GetServAddr().m_addrHost).c_str(), pSession->GetServAddr().m_nServId );
		cdump<<lock<<buf<<" Remote Exception:"<<e->what()<<endl<<unlock;

		e->Delete();
		bOk = false;
	}

	return bOk;
}


///////////////////////////////////////////////////////////////////////////////////////////////////////////
//											CJobSet
///////////////////////////////////////////////////////////////////////////////////////////////////////////

CJobSet::CJobSet()
{
	m_mxLock = CreateMutex();
	m_evMoreJob = CreateEvent( FALSE, FALSE );
	m_evLastJob = CreateEvent( FALSE, FALSE );
	//m_evWakeRouter = CreateEvent( TRUE, FALSE );
}

CJobSet::~CJobSet()
{
	Clear();
	CloseHandle( m_mxLock );
	CloseHandle( m_evMoreJob );
	CloseHandle( m_evLastJob );
	//CloseHandle( m_evWakeRouter );
}

void CJobSet::Lock()
{
	LockMutex( m_mxLock );
}

void CJobSet::Unlock()
{
	UnlockMutex( m_mxLock );
}

void CJobSet::Add( int nId, FUNCJOB pfnExc, void* pParam, double rCpuHint )
{
	CJobObject* pJobObj = new CJobObject( nId, pfnExc, pParam, rCpuHint, this );

	Lock();
	m_PendingSet.Push( pJobObj );
	Unlock();
}

void CJobSet::Clear()
{
	Lock();
	while( !m_PendingSet.IsEmpty() ){
		delete m_PendingSet.Pop();
	}
	while( !m_ZombieSet.IsEmpty() ){
		delete m_ZombieSet.Pop();
	}
	Unlock();
}

int timeout = 6*60*1000;

void CJobSet::RouteJobs( CClientManager* pManager )
{
	timeout = 70*1000;
//	while( !m_PendingSet.IsEmpty() ){
	while( true ){
		while( !m_PendingSet.IsEmpty() ){

			//wait for a host tocken to run a job
			CClientSession* pSession = pManager->RetrieveSession( CMRS_FASTEST );
			ASSERT( pSession!=NULL );

			//get the job object from the job set.
			Lock();
			CJobObject* pJobObj = m_PendingSet.Pop();
			Unlock();
			ASSERT( pJobObj!=NULL );

//			IN_ADDR addr = pHostObj->GetAddr();
//			char* pname = GetHostByAddr( addr );
//			cdump/*<<"("<<pHostObj->m_rSpeedHint<<")"*/<<lock<<pname<<"("<<pHostObj->GetSpeedHint()<<")"<<"\trunning job:"<<pJobObj->GetId()<<endl<<unlock;
			//cout<<inet_ntoa( addr )<<"\t"<<phent->h_name<<endl;

			//set the parameters
			WORKTHREADPARAM* pParam = new WORKTHREADPARAM();
			pParam->m_pSession = pSession;
			pParam->m_pManager = pManager;
			pParam->m_pJobObj  = pJobObj;

			//increate the running thread counter.
			m_ThreadPool.LockedInc();

			//create a worker thread
			HANDLE hThread = 0;
			hThread=CreateThread( (THREAD_ROUTINE)WorkThread, (void*)pParam, 0 );

			//may need to buffer the thread into the thread pool in the future.
			m_ThreadPool.AddThread( hThread );
			CloseHandle( hThread );
		}

		//if the thread is waken up, there may be failed jobs back to the pending set.
		cdump<<lock<<"waiting for hosts... "<<timeout<<endl<<unlock;
//		pHostSet->DumpAllHosts();
		HANDLE arrEvents[2];
		arrEvents[0] = m_evMoreJob;
		arrEvents[1] = m_evLastJob;
		DWORD status = WaitForMultipleObjects( 2, arrEvents, FALSE, timeout );
		if( status==WAIT_TIMEOUT ){
		//if( WaitForSingleObject( m_evWakeRouter, timeout )==WAIT_TIMEOUT ){
			timeout = 1.8*timeout;
			timeout = min( 30*60*1000, timeout );

//			pHostSet->DumpRunningHosts();
			cdump<<lock<<"canceling running hosts..."<<endl<<unlock;

//			int nAvailHosts = pHostSet->GetAliveHostCount();
//			pHostSet->CancelRunningHosts( nAvailHosts );
			status = WaitForMultipleObjects( 2, arrEvents, FALSE, INFINITE );
			//WaitForSingleObject( m_evWakeRouter, INFINITE );
			MilliSleep( 1000 );
//			pHostSet->DumpAllHosts();
		}
		//if the last job is done, then exit the loop.
		if( status==WAIT_OBJECT_0+1 )break;

		cdump<<lock<<"waiting wake up..."<<endl<<unlock;
		//ClearEvent( m_evWakeRouter );

/*		int maxwait = 2;
		for( int i=0; i<maxwait; i++ ){
//			if( WaitForSingleObject( m_evWakeRouter, 15*60*1000 )==WAIT_TIMEOUT ){
			if( WaitForSingleObject( m_evWakeRouter, 30 )==WAIT_TIMEOUT ){
				pHostSet->DumpRunningHosts();
			}else{
				//WaitEvent( m_evWakeRouter );
				ClearEvent( m_evWakeRouter );
				break;
			}
			if( i==maxwait-2
		}
		if( i>=maxwait ){
			cdump<<lock<<"canceling running hosts..."<<endl<<unlock;
			pHostSet->CancelRunningHosts();
			Sleep( 5000 );
		}else{
			cdump<<lock<<"waiting wake up..."<<endl<<unlock;
		}*/

//		DumpPendingJobs(); 
	}
	time_t rawtime;
	time( &rawtime );
	struct tm* timeinfo;
	timeinfo = localtime( &rawtime );
	cdump<<lock<<endl<<asctime(timeinfo)<<"  all done!"<<endl<<unlock;
}

void* CJobSet::WorkThread( void* arg )
{
	//copy parameters.
	WORKTHREADPARAM* pParam = (WORKTHREADPARAM*)arg;
	CJobObject* pJobObj = pParam->m_pJobObj;
	CClientSession* pSession = pParam->m_pSession;
	CClientManager* pManager = pParam->m_pManager;
	CJobSet* pJobSet = pJobObj->GetJobSet();

	//execute the job on the host
	pJobObj->m_tmStart = GetMilliTime();
	bool bOk = pJobObj->Launch( pSession );
	pJobObj->m_tmStop = GetMilliTime();

	//update the speed hint of the host object.
	if( bOk ){
		double rSpeedHint = ( pJobObj->m_tmStop - pJobObj->m_tmStart ) / pJobObj->m_rCpuHint;
//		pHostObj->UpdateSpeedHint( rSpeedHint );
	}else{
		pJobObj->m_nFailCount++;
//		if( pHostObj->m_pSession!=NULL ){
//			if( pHostObj->GetSpeedHint()!=-1 ){
//				pHostObj->SetSpeedHint( 1.5*pHostObj->GetSpeedHint() );
//			}
//		}else{
//			pHostObj->SetSpeedHint( -1 );
//		}
	}

	char buf[512];
	string strHost = GetHostByAddr( pSession->GetServAddr().m_addrHost );
	sprintf( buf, "%s(%d)\tjob %d done:%d", strHost.c_str(), pSession->GetServAddr().m_nServId, pJobObj->GetId(), bOk );

	//return the host tocken back to the host set.
	pManager->ReturnSession( pSession );

	bool bSignalMoreJob = false;
	bool bSignalLastJob = false;
	pJobSet->Lock();
	if( bOk ){	//the job is successful
		pJobSet->m_ZombieSet.Push( pJobObj );
	}else{		//the job is failed, return the Object to pending set to try it again.
		if( pJobSet->m_PendingSet.IsEmpty() ){
			bSignalMoreJob = true;
		}
		pJobSet->m_PendingSet.Push( pJobObj );
	}
	pJobSet->Unlock();

	int nRunningCounter = pJobSet->m_ThreadPool.LockedDec();

	bool bPendingJobs = !pJobSet->m_PendingSet.IsEmpty();

//	if( nRunningCounter==0 && pJobSet->m_PendingSet.IsEmpty() )bSignal = true;
	if( nRunningCounter==0 && !bPendingJobs )bSignalLastJob = true;

	sprintf( buf, "%s,\tremaining threads:%d,\tMoreJobs:%d, signal_more:%d, signal_last:%d", buf, nRunningCounter, bPendingJobs, bSignalMoreJob, bSignalLastJob );
	cdump<<lock<<buf<<endl<<unlock;

	ASSERT( !(bSignalMoreJob && bSignalLastJob) );
	if( bSignalMoreJob ){
		SignalEvent( pJobSet->m_evMoreJob );
	}else if( bSignalLastJob ){
		SignalEvent( pJobSet->m_evLastJob );
	}
	delete pParam;

	return NULL;
}

void CJobSet::DumpPendingJobs()
{
	Lock();

	CFifoSet<CJobObject*>::iterator iter = m_PendingSet.begin();

	while( iter!=m_PendingSet.end() ){
		CJobObject* pJobObj = *iter++;
		IN_ADDR addr = pJobObj->m_addrHost;
		string pname = GetHostByAddr( addr );
		cdump<<lock<<"pending job :"<<pJobObj->GetId()<<"\t on "<<pname.c_str()<<endl<<unlock;
	}

	Unlock();
}