// ncptln.cpp : Defines the entry point for the console application.
//

#include "stdafx.h"
#include "netbase.h"
#include "ncphead.h"
#include "clntkel.h"
#include "netstd.h"
#include <strstream>
#include "ncpscript.h"

using namespace std;

#define MAXPATH		256
#define MAXLINE		256

void PrintUsage()
{
	cout<<"Usage:"<<endl;
	cout<<"hostname user [cmdscript]"<<endl;
}

typedef struct temparg
{
	CClientManager* pManager;
	CClientSession* pSession;
public:
	temparg( CClientManager* pMan, CClientSession* pSes ){
		pManager = pMan;
		pSession = pSes;
	}
}TEMPARG;

int ConsumeSessionThread( void* arg )
{
	TEMPARG* pArg = (TEMPARG*)arg;
	pArg->pSession->DoLogon( "smyan", "" );

	SERVERINFO info;
	pArg->pSession->ServerInfo( info );
	string strproc;
	if( info.nOpSystem==SI_WIN32 ){
		strproc = "testproc.exe";
	}else{
		strproc = "testproc.ln";
	}
	pArg->pSession->MkDir( "testproc" );
	pArg->pSession->ChDir( "testproc" );
	pArg->pSession->CopyTo( strproc, strproc, COPY_OLD_OVERWRITE );
	if( info.nOpSystem!=SI_WIN32 ){
		pArg->pSession->ChMod( strproc, 0x700 );
	}

	DWORD dwExitCode;
	pArg->pSession->RunCommand( strproc, true, dwExitCode );

	pArg->pSession->Close();
	pArg->pManager->ReturnSession( pArg->pSession );
	
	delete pArg;
	return 0;
}

void TestServers( CClientManager* pManager )
{
	vector<CSessionAddr> vcAddrs;

	for( int i=0; i<10; i++ ){
		CClientSession* pSession = pManager->RetrieveSession( CMRS_FASTEST );
		if( pSession->Open() ){
			vcAddrs.push_back( pSession->GetServAddr() );
			TEMPARG* pArg = new TEMPARG( pManager, pSession );
			HANDLE hThread = CreateThread( (THREAD_ROUTINE)ConsumeSessionThread, pArg, 0 );
			CloseHandle( hThread );
		}else{
			pSession->Close();
			pManager->ReturnSession( pSession );
		}
	}

	MilliSleep( (double)rand()/RAND_MAX*8*1000 );
	for( int i=0; i<vcAddrs.size(); i++ ){
		pManager->KillProcess( vcAddrs[i] );
	}
		/*
		int id = (double)rand()/RAND_MAX*vcAddrs.size();
		if( id>=vcAddrs.size() )id = vcAddrs.size()-1;
		pManager->KillProcess( vcAddrs[id] );*/

	MilliSleep( 15*1000 );
}

int main(int argc, char* argv[])
{
	if( argc<3 ){
		PrintUsage();
		return -1;
	}

	char* hostname = argv[1];
	char* struser = argv[2];
	char* cmdscript = NULL;
	ifstream ifscript;

	if( argc>=4 ){
		cmdscript = argv[3];
		ifscript.open( cmdscript );
		if( !ifscript ){
			printf( "script file %s open error\n", cmdscript );

			return -1;
		}
	}

#ifdef _WIN32
	WSADATA wsadata;
	if( WSAStartup(WSVERS, &wsadata)!=0 ){
		exit(1);
	}
#endif

    //get the net relay address from the package
	SOCKADDR_IN netaddr;
	netaddr.sin_family = AF_INET;
	netaddr.sin_port = htons( NCP_SERVER_PORT );

	/*********** parse host names and read the file into the sending buffer ***************/
	struct hostent* phenet = gethostbyname( hostname );
	if( phenet==NULL ){
		printf( "host name is invalid \n" );
		return false;
	}
	bcopy( phenet->h_addr, &netaddr.sin_addr, sizeof(IN_ADDR) );		//save the netrelay address into the package head

	{
		CClientManager manager;
		manager.AddHost( hostname );
	//	manager.AddHost( "tg-login.ncsa.uiuc.edu" );
	//	manager.AddHost( "cee-zzqr.cee.uiuc.edu" );
	//	manager.AddHost( "dcllnx5.ews.uiuc.edu" );
	//	manager.AddHost( "cee-zzvq.cee.uiuc.edu" );
		manager.StartPollThread();

//		MilliSleep( 180*1000 );

//		return 0;

//		TestServers( &manager );
//		manager.ShutdownServer( netaddr.sin_addr, true );
//		return 0;

		CClientSession* pSession = manager.RetrieveSession( CMRS_FASTEST );
		try{
			pSession->Open();
			pSession->DoLogon( struser, "" );

			if( !ifscript.is_open() ){
				DoScript( pSession, cin );
			}else{
				DoScript( pSession, ifscript );
			}
			pSession->Close();
		}catch( CRemoteException* e ){
			cdump<<lock<<"Error:"<<e->what()<<endl<<unlock;
			e->Delete();
		}catch( CSockException* e ){
			cdump<<lock<<"Error:"<<e->what()<<endl<<unlock;
			e->Delete();
		}

		manager.ReturnSession( pSession );
	}

	
#ifdef _WIN32
	WSACleanup();
#endif
	return 0;
}
