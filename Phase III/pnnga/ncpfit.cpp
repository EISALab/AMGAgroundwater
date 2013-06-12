#include "ncpfor_c.h"
#include "stdfor_c.h"

bool DoPdeFit( int nId, CClientSession* pSession, void* pParam )
{
	SERVERINFO sysinfo;

	char buf[MAXPATH], path[MAXPATH];

	if( !pSession->Logon( "smyan_new", "" ) )return false;
	strcpy( path, "hyper_sga" );
//	strcat( path, "_" );
//	strcat( path, GetLocalName().substr(0,8).c_str() );

	pSession->MkDir( path );
	pSession->ChDir( path );
	pSession->ServerInfo( sysinfo );

	sprintf( path, "slave%d", nId );

	char* cmd = NULL;
	if( sysinfo.nOpSystem==SI_WIN32 ){
		cmd = "pdefit.exe";
	}else if( sysinfo.nOpSystem==SI_LINUX ){
		cmd = "pdefit.ln";
	}else if( sysinfo.nOpSystem==SI_UNIX ){
		cmd = "pdefit.un";
	}

	//send all the static files in the static folder to the remote host
	pSession->FileMode( O_TEXT );
	bool bOk = SendDir( pSession, "static", false );

	pSession->FileMode( O_BINARY );
	pSession->CopyTo( cmd, cmd, COPY_OLD_OVERWRITE );
	pSession->ChMod( cmd, 0x700 );

	//for shared disk, make sub compuation folder and enter it.
	if( sysinfo.bShareDisk ){
		pSession->MkDir( path );
		pSession->ChDir( path );
	}

	//delete the surrbak.data file now
	pSession->Remove( "surrbak.dat" );

//	cdump<<lock<<nId<<"--check point 1"<<endl<<unlock;
	//send the files in the slave folder.
	pSession->FileMode( O_TEXT );
	SendDir( pSession, path, true );

	pSession->CopyTo( "mod2.bas", "mod2.bas" );
	pSession->CopyTo( "mod2.bcf", "mod2.bcf" );
	pSession->CopyTo( "mod2.wel", "mod2.wel" );
	pSession->CopyTo( "rt3d2.ssm", "rt3d2.ssm" );
	pSession->CopyTo( "rt3d2.btn", "rt3d2.btn" );


	//for shared disk, make soft link in the computation folder
	if( sysinfo.bShareDisk ){
		LinkDir( pSession, "static" );
		char buf[100];
//		pSession->Remove( cmd );
		sprintf( buf, "../%s", cmd );
		pSession->SoftLink( buf, cmd );
	}


	//run the pdefit
	DWORD dwExit = 0;
	bOk = pSession->RunCommand( cmd, true, dwExit );
//	bOk = pSession->CopyTo( "surrbak.dat", "surrbak.dat", COPY_NO_OVERWRITE );
	if( bOk && dwExit==0 ){
		//retrieve surrback.dat
		strcpy( buf, path );
		strcat( buf, "/surrbak.dat" );
		bOk = pSession->CopyFrom( "surrbak.dat", buf );
	}else{
		bOk = false;
	}

	if( sysinfo.bShareDisk ){
		pSession->ChDir( ".." );
		pSession->RemoveDir( path, true );
//		pSession->EmptyDir( "." );
		//remove the intemediate files to save disk space.
//		pSession->Remove( "optdemo_fine.ftl" );
//		pSession->Remove( "fort.30" );
//		pSession->Remove( "fort.35" );
	}

//	pSession->ChDir( "~" );

	return bOk;
}

