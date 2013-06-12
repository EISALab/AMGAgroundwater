#include "ncpfor_c.h"
#include "stdfor_c.h"

CClientManager* g_pClientManager;

//CHostSet* g_phostset;

bool DoPdeFit( int nId, CClientSession* pSession, void* pParam );

//the fortran interface
extern "C"{
	void _stdcall NCP_INITIALIZE()
	{ ncp_initialize();	}

	void _stdcall NCP_REFRESH_NODES()
	{ ncp_refresh_nodes(); }

	void _stdcall NCP_CLEANUP()
	{ ncp_cleanup(); }

	void _stdcall NCP_DISTRIBUTE_JOBS( int* pnJobIds, int* pnJobCount )
	{ ncp_distribute_jobs( pnJobIds, pnJobCount ); }
}

void ncp_initialize()
{
	WSADATA wsadata;
	if( WSAStartup(WSVERS, &wsadata)!=0 ){
		exit(1);
	}

	g_pClientManager = new CClientManager();
	ncp_refresh_nodes();

	g_pClientManager->StartPollThread();
	cdump.setcontext( "trace.txt", ios::out | ios::app );
}

void ncp_refresh_nodes()
{
	g_pClientManager->LoadHostFile( "host3.txt" );
}

void ncp_cleanup()
{
	delete g_pClientManager;
	WSACleanup();
}

void ncp_distribute_jobs( int* pnJobIds, int* pnJobCount )
{
	CJobSet jobset;

	for( int i=0; i<*pnJobCount; i++ ){
		jobset.Add( pnJobIds[i], DoPdeFit, NULL, 100 );
	}
	jobset.RouteJobs( g_pClientManager );
	jobset.Clear();
}

extern "C"{
	void _stdcall COPYSLAVEFILES( char* pStrFileList, int nChar )
	{
		char str[MAXPATH], buf[MAXPATH], strFile[MAXPATH];
		int nStaticFlag = 0;

		if( nChar>=MAXPATH ){
			return;
		}

		strftoc( pStrFileList, nChar, str );

		FILE* pFile = fopen( str, "rt" );
		while( !feof(pFile) ){
			fgets( buf, MAXPATH, pFile );
			if( feof(pFile) )break;
//			fscanf( pFile, "%s", buf );
			trim(buf);
			if( buf[0]=='\0' || buf[0]=='#' || strncmp(buf, "//", 2)==0 )continue;

			sscanf( buf, "%s%d", strFile, &nStaticFlag );

			//source file name
			strcpy( str, "../" );
			strcat( str, strFile );

			//destination file name
			if( nStaticFlag==1 ){
				strcpy( buf, "../static/" );
				strcat( buf, strFile );
				CopyFile1( str, buf, FALSE );
			}else{
				strcpy( buf, strFile );
				if( nStaticFlag==2 )CopyFile1( str, buf, FALSE ); //for local executation, don't do uncessary copying
				else CopyFile1( str, buf, TRUE );
			}
			//CopyFile( str, buf );
		}
		fclose(pFile);
	}
	
	void _stdcall INITSOCKET()
	{ ncp_initialize(); }

	void _stdcall REFRESHHOSTS()
	{ ncp_refresh_nodes(); }

	void _stdcall CLEARSOCKET()
	{ ncp_cleanup(); }

	void _stdcall ROUTESLAVES( int* pnJobIds, int* pnJobCount )
	{ ncp_distribute_jobs( pnJobIds, pnJobCount ); }

}

bool SendDir( CClientSession* pSession, const char* strDir, bool bOverwrite )
{
	char buf[MAXPATH];
	sprintf( buf, "%s/*.*", strDir );
	
	int nFlags = bOverwrite ? COPY_DONTCARE : COPY_OLD_OVERWRITE;

	struct _finddata_t fileinfo;
	intptr_t hfind = _findfirst( buf, &fileinfo );
	if( hfind==-1 )return false;
	bool bOk = true;
	do{
		if( !(fileinfo.attrib & FILE_ATTRIBUTE_DIRECTORY) ){
			//make the file name for each file
			sprintf( buf, "%s/%s", strDir, fileinfo.name );
			bOk = pSession->CopyTo( buf, fileinfo.name, nFlags );
			if( !bOk ){
				_findclose( hfind );
				return bOk;
			}
		}
	}while( _findnext( hfind, &fileinfo )==0 );
	_findclose( hfind );
	return bOk;
}

void LinkDir( CClientSession* pSession, const char* strDir )
{
	char buf[MAXPATH];
	sprintf( buf, "%s/*.*", strDir );

	struct _finddata_t fileinfo;
	intptr_t hfind = _findfirst( buf, &fileinfo );
	if( hfind==-1 )return;
	do{
		if( !(fileinfo.attrib & FILE_ATTRIBUTE_DIRECTORY) ){
//			pSession->Remove( fileinfo.name );
			//make the file name for each file
			sprintf( buf, "../%s", fileinfo.name );
			pSession->SoftLink( buf, fileinfo.name );
		}
	}while( _findnext( hfind, &fileinfo )==0 );
	_findclose( hfind );
}

