//the file is defined to provide a uniform interface for windows, unix and linux process and thread support.
/*---------------------------------interfaces provided------------------------------------------------
HANDLE CreateMutex();					//Create a mutex. NULL if failed
BOOL LockMutex( HANDLE hMutex );		//Lock and Unlock the mutex
BOOL UnlockMutex( HANDLE hMutex );

HANDLE CreateSemaphore( int initCount=0 );	//Create a semaphore with initCount as initial count.
BOOL WaitForSemaphore( HANDLE hSema );		//Wait on a semaphore object
BOOL ReleaseSemaphore( HANDLE hSema );		//Insease the smeaphore count by 1.

HANDLE CreateEvent( BOOL bManualReset, BOOL bInitState );	//Create a Event object. The event could be manual reset, or auto reset. bInitState is the initial state of the event.
BOOL WaitForEvent( HANDLE hEvent );							//Wait for the event.
BOOL SignalEvent( HANDLE hEvent );							//Set the event to signal state
BOOL ClearEvent( HANDLE hEvent );							//Clear the event to non-signal state

HANDLE CreateThread( THREAD_ROUTINE threadStart, void* threadParam, unsigned initFlags );	//Create a thread from threadStart. initFlags is reserved, should always be 0.
BOOL WaitForThread( HANDLE hThread, LPDWORD lpExitCode=NULL );								//Wait for a thread and retrieve the exit code of thread.
void ExitThread( DWORD dwExitCode );														//normal exit of a thread.
BOOL TerminateThread( HANDLE hThread );														//request termination of a thread. Note: thread may not terminate or immediately terminate in Unix version.
void ThreadTestCancel();																	//Thread should call the function to check the terminating signal so that it can terminate.
DWORD SuspendThread( HANDLE hThread );
DWORD ResumeThread( HANDLE hThread );

BOOL CreateProcess( const char* lpCmdLine, const char* lpCurDir, HANDLE* phProc );			//Create a process
BOOL WaitForProcess( HANDLE hProcess, LPDWORD lpExitCode=NULL );														//wait for a process to terminate.
BOOL SetProcessPriority( HANDLE hProcess, DWORD dwPriorityClass );							//set the priority of a process.
DWORD GetProcessPriority( HANDLE hProcess );
BOOL KillProcess( HANDLE hProcess );														//terminate a process

BOOL WaitForObject( HANDLE hObject );	//Wait for a single kernel object. Could be mutex, semaphore, event, thread or process
BOOL CloseHandle( HANDLE hObject );		//every kernel object should call it to release the kernel object resource.													

										By Shengquan Yan  EMAIL:smyan@uiuc.edu, sq_yan@hotmail.com
-------------------------------------------------------------------------------------------------------------*/

/*---------------------------------bug records, starts 1/14/05------------------------------------------------
1. In the CloseHandle, pthread_detach is called only if pthread_join was not called before. In POSIX standards, 
	pthread_join and pthread_detach both reclaim the thread resources. If CloseHandle is called after WaitForThread, 
	some linux systems report segmentation fault.

-------------------------------------------------------------------------------------------------------------*/

#ifndef _UWKEL_H_
#define _UWKEL_H_

//included head files
#include "uwstd.h"

#ifdef _WIN32
#else
	#include <sys/wait.h>
	#include <sys/types.h>
	#include <sys/sem.h>
	#include <sys/time.h> 
	#include <sys/resource.h> 
	#include <sys/stat.h>
	#include <string.h>
	#include <pthread.h>
	#include <semaphore.h>

typedef unsigned long	DWORD;
typedef DWORD*			LPDWORD;

#define BOOL	int
#define TRUE	1
#define FALSE	0

#endif

/************************** Kernel Object Definition ***********************************/
#ifdef _WIN32
#define THREAD_ROUTINE	LPTHREAD_START_ROUTINE
#else

#define KERNEL_EMPTY		0
#define	KERNEL_MUTEX		1
#define KERNEL_SEMAPHORE	2
#define KERNEL_EVENT		3
#define KERNEL_THREAD		4
#define KERNEL_PROCESS		5
#define KERNEL_SIGNALED		0x80000000
#define KERNEL_TYPE(type)	(type&~KERNEL_SIGNALED)
#define KERNEL_DESTROYED	(int)(~0)			//diagonsing operation on destroied kernel object

#define REALTIME_PRIORITY_CLASS			-20		//Specify this class for a process that has the highest possible priority. The threads of the process preempt the threads of all other processes, including operating system processes performing important tasks. For example, a real-time process that executes for more than a very brief interval can cause disk caches not to flush or cause the mouse to be unresponsive.
#define HIGH_PRIORITY_CLASS				-15		//Specify this class for a process that performs time-critical tasks that must be executed immediately. The threads of the process preempt the threads of normal or idle priority class processes. An example is the Task List, which must respond quickly when called by the user, regardless of the load on the operating system. Use extreme care when using the high-priority class, because a high-priority class application can use nearly all available CPU time.
#define ABOVE_NORMAL_PRIORITY_CLASS		-10		//Windows 2000/XP: Indicates a process that has priority above NORMAL_PRIORITY_CLASS but below HIGH_PRIORITY_CLASS. 
#define	NORMAL_PRIORITY_CLASS			0		//Specify this class for a process with no special scheduling needs. 
#define BELOW_NORMAL_PRIORITY_CLASS		10		//Windows 2000/XP: Indicates a process that has priority above IDLE_PRIORITY_CLASS but below NORMAL_PRIORITY_CLASS. 
#define IDLE_PRIORITY_CLASS				20		//Specify this class for a process whose threads run only when the system is idle. The threads of the process are preempted by the threads of any process running in a higher priority class. An example is a screen saver. The idle-priority class is inherited by child processes. 

typedef void* (*THREAD_ROUTINE)(void *);

typedef struct _event_t
{
	bool				manual;
	sem_t				sema;
	pthread_mutex_t		mutex;
}event_t;

typedef struct _kernel_t
{
	int type;
	union {
		pthread_mutex_t *	mutex;
		sem_t *				sema;
		event_t *			event;
		pthread_t			thread;
		pid_t				pid;
	}k_un;
	DWORD exit_code;		//the exitcode of a thread or process
}kernel_t;

typedef kernel_t*	HANDLE;

inline int event_init( event_t* event, bool manual_reset, bool init_state  )
{
	pthread_mutex_init( &(event->mutex), NULL );
	sem_init( &(event->sema), 0/*shared by threads*/, 0 );
	event->manual = manual_reset;
	if( init_state ){
		sem_post( &(event->sema) );
	}
	return 0;
}

inline int event_destroy( event_t* event )
{
	pthread_mutex_destroy( &(event->mutex) );
	sem_destroy( &(event->sema) );
	return 0;
}

inline int event_set( event_t* event )
{
	pthread_mutex_lock( &(event->mutex) );
	int value;
	sem_getvalue( &(event->sema), &value );
	if( value==0 ){
		sem_post( &(event->sema) );
	}
	pthread_mutex_unlock( &(event->mutex) );

	return 0;
}

inline int event_reset( event_t* event )
{
	pthread_mutex_lock( &(event->mutex) );
	int status;
	while( (status=sem_trywait( &(event->sema) ))==0 );
	status = errno;			//save the errno.
	pthread_mutex_unlock( &(event->mutex) );
	return status==EAGAIN;
}

inline int event_wait( event_t* event )
{
	sem_wait( &(event->sema) );
	//a manual reset event should keep the state to signal status.
	if( event->manual ){
		event_set( event );
	}
	return 0;
}

#ifndef SEM_R
#define SEM_R	0400
#endif

#ifndef SEM_A
#define SEM_A	0200
#endif

union semun {
    int val;
    struct semid_ds *buf;
    u_short *array;
};

#define HANDLE_TO_THREADID( handle )		(((kernel_t*)handle)->k_un.thread)

#endif //_WIN32

/*************************** functions for mutex, semaphore and event *****************************/

//return:	the handle of the mutex. NULL if failed
inline HANDLE CreateMutex()
{
#ifdef _WIN32
	return CreateMutex( NULL, FALSE, NULL );
#else
	kernel_t * pKelObj = new kernel_t();
	pKelObj->type = KERNEL_MUTEX;
	pKelObj->k_un.mutex = new pthread_mutex_t();
	pthread_mutex_init( pKelObj->k_un.mutex, NULL );
	return pKelObj;
#endif
}

//hMutex(in): the mutex handle
//return:	true if successful
inline BOOL LockMutex( HANDLE hMutex )
{
#ifdef _WIN32
	DWORD status = WaitForSingleObject( hMutex, INFINITE );
	return status==WAIT_OBJECT_0;
#else
	kernel_t* pKelObj = hMutex;
	ASSERT( pKelObj!=NULL );
	ASSERT( KERNEL_TYPE(pKelObj->type)==KERNEL_MUTEX );
	int status = pthread_mutex_lock( pKelObj->k_un.mutex );
	return status==0;
#endif
}

inline BOOL UnlockMutex( HANDLE hMutex )
{
#ifdef _WIN32
	return ReleaseMutex( hMutex );
#else
	kernel_t* pKelObj = hMutex;
	ASSERT( pKelObj!=NULL );
	ASSERT( KERNEL_TYPE(pKelObj->type)==KERNEL_MUTEX );
	int status = pthread_mutex_unlock( pKelObj->k_un.mutex );
	return status==0;
#endif
}

inline HANDLE CreateSemaphore( int initCount=0 )
{
#ifdef _WIN32
	return CreateSemaphore( NULL, initCount, 0xffff, NULL );
#else
	kernel_t * pKelObj = new kernel_t();
	pKelObj->type = KERNEL_SEMAPHORE;
	pKelObj->k_un.sema = new sem_t();
	sem_init( pKelObj->k_un.sema, 0/*shared by threads*/, initCount );
	return pKelObj;
#endif
}

inline BOOL WaitForSemaphore( HANDLE hSema )
{
#ifdef _WIN32
	DWORD status = WaitForSingleObject( hSema, INFINITE );
	return status==WAIT_OBJECT_0;
#else
	kernel_t* pKelObj = hSema;
	ASSERT( pKelObj!=NULL );
	ASSERT( KERNEL_TYPE(pKelObj->type)==KERNEL_SEMAPHORE );
	int status = sem_wait( pKelObj->k_un.sema );
	return status==0;
#endif
}

inline BOOL ReleaseSemaphore( HANDLE hSema )
{
#ifdef _WIN32
	return ReleaseSemaphore( hSema, 1, NULL );
#else
	kernel_t* pKelObj = hSema;
	ASSERT( pKelObj!=NULL );
	ASSERT( KERNEL_TYPE(pKelObj->type)==KERNEL_SEMAPHORE );
	int status = sem_post( pKelObj->k_un.sema );
	return status==0;
#endif
}

inline HANDLE CreateEvent( BOOL bManualReset, BOOL bInitState )
{
#ifdef _WIN32
	return ::CreateEvent( NULL, bManualReset, bInitState, NULL );
#else
	kernel_t * pKelObj = new kernel_t();
	pKelObj->type = KERNEL_EVENT;
	pKelObj->k_un.event = new event_t();
	event_init( pKelObj->k_un.event, bManualReset, bInitState );
	return pKelObj;
#endif
}

inline BOOL WaitForEvent( HANDLE hEvent )
{
#ifdef _WIN32
	DWORD status = WaitForSingleObject( hEvent, INFINITE );
	return status==WAIT_OBJECT_0;
#else
	kernel_t* pKelObj = hEvent;
	ASSERT( pKelObj!=NULL );
	ASSERT( KERNEL_TYPE(pKelObj->type)==KERNEL_EVENT );
	int status = event_wait( pKelObj->k_un.event );
	ASSERT( KERNEL_TYPE(pKelObj->type)==KERNEL_EVENT );
	return status==0;
#endif
}

inline BOOL SignalEvent( HANDLE hEvent )
{
#ifdef _WIN32
	return ::SetEvent( hEvent );
#else
	kernel_t* pKelObj = hEvent;
	ASSERT( pKelObj!=NULL );
	ASSERT( KERNEL_TYPE(pKelObj->type)==KERNEL_EVENT );
	int status = event_set( pKelObj->k_un.event );
	return status==0;
#endif
}

inline BOOL ClearEvent( HANDLE hEvent )
{
#ifdef _WIN32
	return ::ResetEvent( hEvent );
#else
	kernel_t* pKelObj = hEvent;
	ASSERT( pKelObj!=NULL );
	ASSERT( KERNEL_TYPE(pKelObj->type)==KERNEL_EVENT );
	int status = event_reset( pKelObj->k_un.event );
	return status==0;
#endif
}

/*************************** functions for thread and process *****************************/

//Create a thread from threadStart. 
//threadStart(in): the thread starting function. 
//threadParam(in): the parameter passed to the thread function.
//initFlags is reserved, should always be 0.
inline HANDLE CreateThread( THREAD_ROUTINE threadStart, void* threadParam, unsigned initFlags )
{
#ifdef _WIN32
	return CreateThread( NULL, 0, (LPTHREAD_START_ROUTINE)threadStart, threadParam, initFlags, (LPDWORD)NULL );
#else
	pthread_attr_t attr;
	//initialize the thread attr.
	pthread_attr_init (&attr);

	pthread_t thread_id;
	int status = pthread_create( &thread_id, &attr, threadStart, threadParam );
	if( status!=0 )return NULL;

	kernel_t* pKelObj = new kernel_t();
	pKelObj->type = KERNEL_THREAD;
	pKelObj->k_un.thread = thread_id;
	return pKelObj;
#endif
}

#ifndef _WIN32
/*
	Suspend a thread by sending it a signal (SIGUSR1), which will block the thread until another signal (SIGUSR2) arrives.

	Multiple calls to the thd_suspend for a sigle thread have no additional effect on the thread --
	a single thd_continue call will cause it to resume execution.
*/
int thd_suspend( pthread_t target );
/*
	Resume a suspended thread by sending it SIGUSR2 to break it out of the sigsuspend() in which it is waitng.
	If the target thread isn't suspended, return with success.
*/
int thd_continue( pthread_t target );

inline DWORD SuspendThread( HANDLE hThread )
{
	kernel_t* pKelObj = hThread;
	ASSERT( pKelObj!=NULL );
	ASSERT( KERNEL_TYPE(pKelObj->type) == KERNEL_THREAD );
	if( pKelObj->type & KERNEL_SIGNALED ){
		return (DWORD)-1;
	}
	int status = thd_suspend( pKelObj->k_un.thread );
	ASSERT( KERNEL_TYPE(pKelObj->type) == KERNEL_THREAD );
	return status==0;
}

inline DWORD ResumeThread( HANDLE hThread )
{
	kernel_t* pKelObj = hThread;
	ASSERT( pKelObj!=NULL );
	ASSERT( KERNEL_TYPE(pKelObj->type) == KERNEL_THREAD );
	int status = thd_continue( pKelObj->k_un.thread );
	ASSERT( KERNEL_TYPE(pKelObj->type) == KERNEL_THREAD );
	return status==0;
}

#endif

//Thread can only be wait once in Unix and Linux. 
//After waiting the handle should not be used except for CloseHandle().
inline BOOL WaitForThread( HANDLE hThread, LPDWORD lpExitCode=NULL )
{
#ifdef _WIN32
	DWORD status = WaitForSingleObject( hThread, INFINITE );
	if( status!=WAIT_OBJECT_0 )return FALSE;
	if( lpExitCode!=NULL ){
		return GetExitCodeThread( hThread, lpExitCode );
	}
	return TRUE;
#else
	kernel_t* pKelObj = hThread;
	ASSERT( pKelObj!=NULL );
	ASSERT( KERNEL_TYPE(pKelObj->type) == KERNEL_THREAD );
	if( pKelObj->type & KERNEL_SIGNALED ){
		if( lpExitCode )*lpExitCode = pKelObj->exit_code;
		return TRUE;
	}
	int status = pthread_join( pKelObj->k_un.thread, (void**)&pKelObj->exit_code );
	if( status==0 ){
		ASSERT( KERNEL_TYPE(pKelObj->type)==KERNEL_THREAD );
		//POSIX stardard can only wait thread once, the thread is immediately recycled after the waiting.
		pKelObj->type |= KERNEL_SIGNALED;
		if( lpExitCode )*lpExitCode = pKelObj->exit_code;
	}
	return status==0;
#endif
}

#ifndef _WIN32
inline void ExitThread( DWORD dwExitCode )
{
	pthread_exit( (void*)dwExitCode );
}
#endif

inline BOOL TerminateThread( HANDLE hThread )
{
#ifdef _WIN32
	return TerminateThread( hThread, 0 );
#else
	kernel_t* pKelObj = hThread;
	ASSERT( pKelObj!=NULL );
	ASSERT( KERNEL_TYPE(pKelObj->type) == KERNEL_THREAD );
	int status = pthread_cancel( pKelObj->k_un.thread );

	//POSIX stardard can only wait thread once, the thread is immediately recycled after the waiting.
	//pKelObj->type = KERNEL_EMPTY;
	return status==0;
#endif
}

inline void ThreadTestCancel()
{
#ifndef _WIN32
	pthread_testcancel();
#endif
}

BOOL CreateProcess( const char* lpCmdLine, const char* lpCurDir, HANDLE* phProc );

//Process can only be wait once in Unix and Linux. 
//After waiting the handle should not be used except for CloseHandle().
inline BOOL WaitForProcess( HANDLE hProcess, LPDWORD lpExitCode=NULL )
{
#ifdef _WIN32
	DWORD status = WaitForSingleObject( hProcess, INFINITE );
	if( (status==WAIT_OBJECT_0) && (lpExitCode!=NULL) )GetExitCodeProcess( hProcess, lpExitCode );
	return status==WAIT_OBJECT_0;
#else
	kernel_t* pKelObj = hProcess;
	ASSERT( pKelObj!=NULL );
	ASSERT( KERNEL_TYPE(pKelObj->type) == KERNEL_PROCESS );
	if( pKelObj->type & KERNEL_SIGNALED ){					//the process has exited
		if( lpExitCode )*lpExitCode = pKelObj->exit_code;
		return TRUE;
	}

	int status;
	BOOL suc =( pKelObj->k_un.pid == waitpid( pKelObj->k_un.pid, &status, 0 ) );
	if( suc ){
		ASSERT( KERNEL_TYPE(pKelObj->type)==KERNEL_PROCESS );
		//unix stardard can only wait process once, the process is immediately recycled after the waiting.
		pKelObj->type |= KERNEL_SIGNALED;
		pKelObj->exit_code = (DWORD)status;
		if(lpExitCode)*lpExitCode = status;
	}

	return suc;
#endif
}

inline BOOL SetProcessPriority( HANDLE hProcess, DWORD dwPriorityClass )
{
#ifdef _WIN32
	return SetPriorityClass( hProcess, dwPriorityClass );
#else
	kernel_t* pKelObj = hProcess;
	ASSERT( pKelObj!=NULL );
	ASSERT( KERNEL_TYPE(pKelObj->type) == KERNEL_PROCESS );
	int status = setpriority( PRIO_PROCESS, pKelObj->k_un.pid, dwPriorityClass );
	return status==0;
#endif
}

inline DWORD GetProcessPriority( HANDLE hProcess )
{
#ifdef _WIN32
	return GetPriorityClass( hProcess );
#else
	kernel_t* pKelObj = hProcess;
	ASSERT( pKelObj!=NULL );
	ASSERT( KERNEL_TYPE(pKelObj->type) == KERNEL_PROCESS );
	return getpriority( PRIO_PROCESS, pKelObj->k_un.pid );
#endif
}

inline BOOL KillProcess( HANDLE hProcess )
{
#ifdef _WIN32
	return TerminateProcess( hProcess, 0 );
#else
	kernel_t* pKelObj = hProcess;
	ASSERT( pKelObj!=NULL );
	ASSERT( KERNEL_TYPE(pKelObj->type) == KERNEL_PROCESS );
	int status = kill( pKelObj->k_un.pid, SIGABRT );
	return status==0;
#endif
}

inline BOOL WaitForObject( HANDLE hObject )
{
#ifdef _WIN32
	DWORD status = WaitForSingleObject( hObject, INFINITE );
	return status==WAIT_OBJECT_0;
#else
	kernel_t* pKelObj = hObject;
	ASSERT( pKelObj!=NULL );
	switch( pKelObj->type ){
	case KERNEL_MUTEX:
		return LockMutex( hObject );
		break;
	case KERNEL_SEMAPHORE:
		return WaitForSemaphore( hObject );
		break;
	case KERNEL_EVENT:
		return WaitForEvent( hObject );
		break;
	case KERNEL_THREAD:
		return WaitForThread( hObject, NULL );
		break;
	case KERNEL_PROCESS:
		return WaitForProcess( hObject, NULL );
		break;
	default:
		ASSERT( FALSE );
		return FALSE;
	}
	return FALSE;
#endif
}


#ifndef _WIN32
inline BOOL CloseHandle( HANDLE hObject )
{
	kernel_t* pKelObj = hObject;
	ASSERT( pKelObj!=NULL );
	int status = 0;
	switch( KERNEL_TYPE(pKelObj->type) ){
	case KERNEL_MUTEX:
		status = pthread_mutex_destroy( pKelObj->k_un.mutex );
		delete pKelObj->k_un.mutex;
		break;
	case KERNEL_SEMAPHORE:
		status = sem_destroy( pKelObj->k_un.sema );
		delete pKelObj->k_un.sema;
		break;
	case KERNEL_EVENT:
		status = event_destroy( pKelObj->k_un.event );
		delete pKelObj->k_un.event;
		break;
	case KERNEL_THREAD:
		if( !(pKelObj->type&KERNEL_SIGNALED) )
			status = pthread_detach( pKelObj->k_un.thread );
		break;
	case KERNEL_PROCESS:
		//WaitForProcess( hObject ); can't really remove the zombie process without waiting...
		break;
	case KERNEL_EMPTY:
		break;
	default:
		ASSERT( FALSE );
		return FALSE;
	}
	pKelObj->type = KERNEL_DESTROYED;
	delete pKelObj;
	return status==0;
}
#endif //_WIN32

#endif	//_UWKEL_H
