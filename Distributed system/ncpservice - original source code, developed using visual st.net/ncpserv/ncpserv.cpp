// ncpserv.cpp : Defines the entry point for the console application.
//

//#include "stdafx.h"
#include "ncphead.h"
#include "servkel.h"

CServerApp servApp;

int RunService(void*)
{
	servApp.RunServer();
	servApp.Destroy();
	return 0;
}

#ifndef _WIN32
void catch_sig_pipe(int sig_num)
{
    /* re-set the signal handler again to catch_int, for next time */
    signal(SIGPIPE, catch_sig_pipe);
    /* and print the message */
    printf("PIPE signal catched\n");
    fflush(stdout);
	exit(0);
}
void catch_sig_usr1(int sig_num)
{
    /* re-set the signal handler again to catch_int, for next time */
    signal(SIGUSR1, catch_sig_usr1);
    /* and print the message */
    printf("user1 signal catched\n");
    fflush(stdout);
}
#endif

int main(int argc, char* argv[])
//int _tmain(int argc, _TCHAR* argv[])
{
	GetMilliTime();
#ifdef _WIN32
	WSADATA wsadata;
	if( WSAStartup(WSVERS, &wsadata)!=0 ){
		exit(1);
	}
#endif

	if( argc>=2 ){
		if( strcmp(argv[1], "-p")==0 ){
			servApp.SetAsProxy( true );
			servApp.SetAsServer(false);
		}else if( strcmp(argv[1], "-s")==0 ){
			servApp.SetAsServer( true );;
		}else if( strcmp(argv[1], "-sp")==0 ){
			servApp.SetAsServer( true );
			servApp.SetAsProxy( true );
		}
		//servApp.m_strHosts = argv[1];
	}
	if( argc>=3 ){
		servApp.m_strHostFile = argv[2];
	}

/*#ifdef _WIN32
	if( argc>=2 ){
		if( strcmp( argv[1], "-i" )==0 ){
			InstallService();
		}else if( strcmp( argv[1], "-u" )==0 ){
			RemoveService();
		}else if( strcmp( argv[1], "-r" )==0 ){
			//run as a norm windows application. always set it default at lower priority
			pfout = stdout;
			servApp.SetServState( SYSTEM_USER_LOGON );
			RunService(NULL);
		}
	}else{
		char path[MAX_PATH];
		GetModuleFileName( NULL, path, MAX_PATH );
		char* p = strrchr( path, '\\' );
		*p = '\0';
		strcat( path, "\\ncp.log" );
		pfout = fopen( path, "wt" );

		RunWinService();

		fclose( pfout );
		pfout = NULL;
	}
#else*/

#ifndef _WIN32
	signal(SIGPIPE, catch_sig_pipe);
	signal(SIGUSR1, catch_sig_usr1);
#endif
	cout<<"CMessageBase:"<<sizeof(CMessageBase)<<", NETF_STAT:"<<sizeof(NETF_STAT)<<", time_t:"<<sizeof(time_t)<<endl;

	char buf[256];  
	gethostname( buf, ELEMENTS(buf) ); 
	hostent* phent = gethostbyname( buf );
//	printf( "server running on %s, machine int is %d, long is %d,ncpmsg is %d, netfstat is %d\n", phent->h_name, sizeof(int), sizeof(long), sizeof(CNcpMessage), sizeof(NETF_STAT) );         
	//for unix version. just run it.
	RunService(NULL);
//#endif

#ifdef _WIN32
	WSACleanup();
#endif
	return 0;
}

