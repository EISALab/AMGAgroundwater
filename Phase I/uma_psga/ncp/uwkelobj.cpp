#include "uwkelobj.h"

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
