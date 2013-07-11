// ncpserv.cpp : Defines the entry point for the console application.
//

#include "stdafx.h"
#include "../ncphead.h"
#include "servkel.h"

CServerApp servApp;

int RunService(void*)
{
	servApp.RunServer();
	return 0;
}

int main(int argc, char* argv[])
//int _tmain(int argc, _TCHAR* argv[])
{
#ifdef _WIN32
	WSADATA wsadata;
	if( WSAStartup(WSVERS, &wsadata)!=0 ){
		exit(1);
	}
#endif

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

