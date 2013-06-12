#include "uwkelobj.h"

#ifndef _WIN32

#include <vector>
#include <algorithm>
using namespace std;

#define err_abort( code, text )	do{ \
	fprintf( stderr, "%s at \"%s\":%d: %s\n", \
		(text), __FILE__, __LINE__, strerror(code)); \
	abort(); \
	}while(0)

#define errno_abort( text )	do{ \
	fprintf( stderr, "%s at \"%s\":%d: %s\n", \
		(text), __FILE__, __LINE__, strerror(errno)); \
	abort(); \
	}while(0)

pthread_mutex_t		the_mutex = PTHREAD_MUTEX_INITIALIZER;
pthread_once_t		the_once = PTHREAD_ONCE_INIT;
vector<pthread_t>	the_threads;
int					the_inited = FALSE;
sem_t				the_sema;

/*
	Handle SIGUSR1 in the target thread, so suspend it unitl receiving SIGUSR2(resume)
*/
void suspend_signal_handler( int sig )
{
	sigset_t signal_set;
	//Block all signals except SIGUSR2 while suspended.
	sigfillset( &signal_set );
	sigdelset( &signal_set, SIGUSR2 );

	//Post the semaphore to let the thd_suspend return.
	if( sem_post( &the_sema )==-1 )errno_abort( "Post semaphore" );

	//Now wait for SIGUSR2 (the resume signal)
	sigsuspend( &signal_set );

	//Once I'm here, I've been resumed, and the resume signal handler has been run to the completion.
	return;
}

/*
	Handle SIGUSR2 in the target thread, to resume it, Note that the signal handler does nothing. 
	It exists only because we need to cause sigsuspend() to reutrn.
*/
void resume_signal_handler( int sig )
{
	return;
}

/*
	Dynamically initialize the "suspend package" when first used.
	(called by pthread_once).
*/
void suspend_init_routine()
{
	int status;
	struct sigaction sigusr1, sigusr2;

	//Initialize the semaphore
	status = sem_init( &the_sema, 0, 0 );
	if( status==-1 )errno_abort( "Initialize semaphore" );

	//Install the signal handlers for suspend/resume.
	sigusr1.sa_flags = 0;
	sigusr1.sa_handler = suspend_signal_handler;
	sigemptyset( &sigusr1.sa_mask );
	status = sigaction( SIGUSR1, &sigusr1, NULL );
	if( status==-1 )errno_abort("Installing suspend handler");

	sigusr2.sa_flags = 0;
	sigusr2.sa_handler = resume_signal_handler;
	sigusr2.sa_mask = sigusr1.sa_mask;
	status = sigaction( SIGUSR2, &sigusr2, NULL );
	if( status==-1 )errno_abort("Installing resume handler");

	the_inited = TRUE;
}

/*
	Suspend a thread by sending it a signal (SIGUSR1), which will block the thread until another signal (SIGUSR2) arrives.

	Multiple calls to the thd_suspend for a sigle thread have no additional effect on the thread --
	a single thd_continue call will cause it to resume execution.
*/
int thd_suspend( pthread_t target )
{
	int status;

	//The first call to thd_supend will initialize the package.
	status = pthread_once( &the_once, suspend_init_routine );
	if( status!=0 )return status;

	//Serialize access to suspend.
	status = pthread_mutex_lock( &the_mutex );
	if( status!=0 )return status;

	//Thread that are suspended are added to the the_thread vector; a request to suspend a thread already listed in the 
	//the vector is ignroed. Sending a second SIGUSR1 would cause the thread to resuspend itself as soon as it is resumed.
	vector<pthread_t>::iterator pos = find( the_threads.begin(), the_threads.end(), target );
	if( pos!=the_threads.end() )return pthread_mutex_unlock( &the_mutex );

	//Signal the thread to suspend
	status = pthread_kill( target, SIGUSR1 );
	if( status!=0 ){
		pthread_mutex_unlock( &the_mutex );
		return status;
	}

	//Wait for the semaphore, to make sure the thread is suspened.
	do{
		status = sem_wait( &the_sema );
	}while( status==EINTR );
	if( status!=0 )errno_abort( "Wait for semaphore" );


	//Insert the thread into the thread vector
	pos = find( the_threads.begin(), the_threads.end(), (pthread_t)0 );
	if( pos==the_threads.end() ){
		the_threads.push_back( target );
	}else{
		*pos = target;
	}

	return pthread_mutex_unlock( &the_mutex );
}

/*
	Resume a suspended thread by sending it SIGUSR2 to break it out of the sigsuspend() in which it is waitng.
	If the target thread isn't suspended, return with success.
*/
int thd_continue( pthread_t target )
{
	int status;
	
	//Serialize access to suspend
	status = pthread_mutex_lock( &the_mutex );
	if( status!=0 )return status;

	//If we haven't been initialized, then the thread must be "resumed"; it couldn't have been suspended!
	if( !the_inited )return pthread_mutex_unlock( &the_mutex );

	//Make sure the thread is in the suspend vector. If not, it hasn't been suspended (or it has already been
	//resumed) and we can just carry on.
	vector<pthread_t>::iterator pos = find( the_threads.begin(), the_threads.end(), target );
	if( pos==the_threads.end() ){
		pthread_mutex_unlock( &the_mutex );
		return 0;
	}

	//Signal the thread to continue, and remove the thread from the suspended vector.
	status = pthread_kill( target, SIGUSR2 );
	if( status!=0 ){
		pthread_mutex_unlock( &the_mutex );
		return status;
	}

	*pos = 0;
	return pthread_mutex_unlock( &the_mutex );
}

#endif
	
BOOL CreateProcess( const char* lpCmdLine, const char* lpCurDir, HANDLE* phProc )
{
#ifdef _WIN32
	STARTUPINFO si;
	memset( &si, 0, sizeof(si) );
	si.cb = sizeof( si );

	PROCESS_INFORMATION pi;
	memset( &pi, 0, sizeof(pi) );
	BOOL suc = CreateProcess( NULL, (char*)lpCmdLine, NULL, NULL, FALSE, 0, NULL, lpCurDir, &si, &pi );
	if( suc ){
		CloseHandle( pi.hThread );
		if( phProc!=NULL ){
			*phProc = pi.hProcess;
		}else{
			CloseHandle( pi.hProcess );
		}
	}
	return suc;
#else
	pid_t pid;

	//create a semaphore to avoid zombie if no sync.
	int semid=0;
	if( phProc==NULL ){
		semget( IPC_PRIVATE, 1, SEM_R|SEM_A );

		union semun semarg;
		semarg.val = 0;
		//set the semaphore to 0. the 0 is for the union semun.val.
		semctl( semid, 0, SETVAL, semarg );
	}

	char buf[_MAX_PATH], filepath[_MAX_PATH];
	char* argv[_MAX_ARG];
	if( lpCurDir==NULL )lpCurDir = ".";

	//fullpath will modify cmdline, first copy it
	strcpy( buf, lpCmdLine );
	if( parse_cmdline( buf, filepath, _MAX_PATH, argv, _MAX_ARG )==-1 )return FALSE;

	//get the full path of the executable file to buf
	if( fullpath( filepath, filepath, _MAX_PATH )==NULL )return FALSE;

	struct stat st;
	if( stat(filepath, &st)!=0 )return FALSE;
	if( stat(lpCurDir, &st)!=0 || !S_ISDIR(st.st_mode) )return FALSE;

	if( (pid=fork())<0 ){
		return FALSE;
	}else if( pid==0 ){		//this is the child
		if( phProc==NULL ){	//no sync is needed, fork again to avoid the defunct process.
			//using IPC semaphore to synchronize the second and the parent.
            pid = fork();
			if( pid>0 ){
				//the first child exit immediately so that the second child's parent be init.
				exit(0);
			}else{			//the second child
				//the second child wait the running signal of the parent
				struct sembuf opbuf;
				opbuf.sem_num = 0; opbuf.sem_op = -1; opbuf.sem_flg = 0;
				semop( semid, &opbuf, 1 );

				//now delete the semaphore, it's a system resource.
				semctl( semid, 0, IPC_RMID, 0 );

				//run it now.
				chdir( lpCurDir );	//set the current directory and run the child process
				execv( filepath, argv );
				//execl( "/bin/sh", "sh", "-c", lpCmdLine, (char*)0 );
				_exit( -1 );		//shouldn't do it because execl doesn't return
			}
		}else{	//need sync, just do it on the first child
			chdir( lpCurDir );		//set the current directory and run the child process
			execv( filepath, argv );
			//execl( "/bin/sh", "sh", "-c", lpCmdLine, (char*)0 );
			_exit( -1 );		//shouldn't do it because execl doesn't return
		}
	}else{
		if( phProc!=NULL ){
			//create a kernel object and send it back through phProc
			kernel_t* pKelObj = new kernel_t();
			pKelObj->type = KERNEL_PROCESS;
			pKelObj->k_un.pid = pid;
			*phProc = pKelObj;
		}else{
			//no sync is needed, to avoid a zombie defunc process, wait the pid of the first child
			waitpid( pid, NULL, 0 );

			//signal the second child to run.
			struct sembuf opbuf;
			opbuf.sem_num = 0; opbuf.sem_op = 1; opbuf.sem_flg = 0;
			semop( semid, &opbuf, 1 );
		}
		return TRUE;
	}
#endif
}
