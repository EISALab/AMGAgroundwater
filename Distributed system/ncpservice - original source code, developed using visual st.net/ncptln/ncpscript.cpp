#include "stdafx.h"
#include "netbase.h"
#include "ncphead.h"
#include "clntkel.h"
#include "netstd.h"
#include <strstream>
#include "ncpscript.h"

void cmdPutFiles( CClientSession* pSession, char* pfilelist )
{
	char buf[_MAX_PATH];
	istrstream str(pfilelist);

	while(true){
		str>>buf;
		if( str.fail() )break;

#ifdef _WIN32
		char* p = strrchr( buf, '\\' );
#else
		char* p = strrchr( buf, '/' );
#endif
		if( p==NULL ){
			p = strchr( buf, ':' );
		}
		if( p!=NULL )p++;	//point to the true file name
		if( p==NULL )p = buf;

		if( !pSession->CopyTo(buf, p) ){
			cout<<"\tput file:("<<buf<<")failed!"<<endl;
		}
	}
}

void cmdRemove( CClientSession* pSession, char* pfile )
{
	if( !pSession->Remove( pfile ) ){
		cout<<"\tcmd failed!"<<endl;
	}
}

void cmdGetFiles( CClientSession* pSession, char* pfilelist )
{
	char buf[_MAX_PATH];
	istrstream str(pfilelist);

	while(true){
		str>>buf;
		if( str.fail() )break;
		if( !pSession->CopyFrom( buf, buf, COPY_DONTCARE ) ){
			cout<<"\tget file:("<<buf<<")failed!"<<endl;
		}
	}
}

void cmdRunImage( CClientSession* pSession, char* plist )
{
	char* pcmd = plist;
	char* pval = plist;
	while( *pval && !isspace(*pval) )pval++;

	trimleft( pcmd );
	strlwr( pcmd );

	bool bsync = true;

	if( pcmd[0]=='-' ){
		if( *pval!='\0' ){
			*pval++ = '\0';
		}

		if( strcmp( pcmd, "-w" )==0 ){
			bsync = true;
		}else if( strcmp( pcmd, "-n" )==0 ){
			bsync = false;
		}
	}else{
		pval = plist;
	}

	DWORD dwExitCode;
	if( !pSession->RunCommand(pval, bsync, dwExitCode) ){
		cout<<"\tcmd failed"<<endl;
	}else{
		cout<<"cmd finisned with exit code : "<<dwExitCode<<endl;
	}
}

void cmdMkDir( CClientSession* pSession, char* pdir )
{
	if( !pSession->MkDir(pdir) ){
		cout<<"\tcmd failed"<<endl;
	}
}

void cmdChDir( CClientSession* pSession, char* pdir )
{
	if( !pSession->ChDir(pdir) ){
		cout<<"\tcmd failed"<<endl;
	}
}

void cmdRmDir( CClientSession* pSession, char* pdir )
{
	char* pcmd = pdir;
	char* pval = pdir;
	while( *pval && !isspace(*pval) )pval++;

	trimleft( pcmd );
	strlwr( pcmd );

	bool bRecursive = false;

	if( pcmd[0]=='-' ){
		if( *pval!='\0' ){
			*pval++ = '\0';
		}

		if( strcmp( pcmd, "-r" )==0 ){
			bRecursive = true;
		}
	}else{
		pval = pdir;
	}

	if( !pSession->RemoveDir( pval, bRecursive ) ){
		cout<<"\tcmd failed"<<endl;
	}
}

void cmdFileMode( CClientSession* pSession, char* pval )
{
	int nFileMode = O_TEXT;
	if( strcmp( pval, "text" )==0 ){
		nFileMode = O_TEXT;
	}else if( strcmp( pval, "bin" )==0 ){
		nFileMode = O_BINARY;
	}else{
		cout<<"Unknown parameters!"<<endl;
		return;
	}

	int nRet = pSession->DoFileMode( nFileMode );
	if( FAILED(nRet) ){
		cout<<"\tcmd failed"<<endl;
	}
}

void PrintNetfEntry( NETF_ENTRY* pentry )
{
	time_t mtime = pentry->fstat.nfs_mtime;
	struct tm* newtime = localtime( &(mtime) );
	char buf[_MAX_LINE];
	strftime( buf, ELEMENTS(buf), "%m/%d/%y %I:%M %p", newtime );
	cout<<buf<<"\t"<<pentry->fname<<"\t"<<pentry->fstat.nfs_size<<endl;
}

void cmdListDir( CClientSession* pSession, char* pdir )
{
	if( !pSession->ListDir( pdir, PrintNetfEntry ) ){
		cout<<"\tcmd failed"<<endl;
	}
}


void cmdChMod( CClientSession* pSession, char* pval )
{
	char src[_MAX_PATH], mode[_MAX_PATH];
	istrstream str(pval);

	str>>mode>>src;
	if( mode[0]=='\0' ){
		cout<<"need mode, eg. 700 for rwx"<<endl;
	}
	int nMode;
	sscanf( mode, "%x", &nMode );

	if( !pSession->ChMod( src, nMode ) ){
		cout<<"\tcmd failed!"<<endl;
	}
}

void cmdSoftLink( CClientSession* pSession, char* pval )
{
	char src[_MAX_PATH], lnk[_MAX_PATH];
	istrstream str(pval);

	str>>src>>lnk;

	if( src[0]=='\0' || lnk[0]=='\0' )return;

	if( !pSession->SoftLink( src, lnk ) ){
		cout<<"\tcmd failed"<<endl;
	}
}

void cmdShutdown( CClientSession* pSession ,char* pval )
{
	IN_ADDR addrHost;
	addrHost.s_addr = INADDR_ANY;
	pSession->GetProxy()->GetManager()->ShutdownServer( addrHost, true );
	return;
}

void cmdSysInfo( CClientSession* pSession )
{
	SERVERINFO sysinfo;
	if( pSession->ServerInfo( sysinfo ) ){
		cout<<GetHostByAddr(sysinfo.ipAddr)<<endl<<"OS:"<<sysinfo.nOpSystem<<"\t"<<"DISK:"<<sysinfo.bShareDisk<<"\t"<<"STAT:"<<sysinfo.nSysStat<<endl;
		cout<<"SESS:"<<sysinfo.nSessions<<"\t"<<"PROC:"<<sysinfo.nChildPorcs<<"\tIDLE:"<<sysinfo.bSysIdle<<endl;
	}else{
		cout<<"\tcmd failed"<<endl;
	}
}

void cmdClearDir( CClientSession* pSession, char* pdir )
{
	if( !pSession->EmptyDir(pdir) ){
		cout<<"\tcmd failed"<<endl;
	}
}

bool DoScript( CClientSession* pSession, istream& ifscript )
{

	char buf[_MAX_LINE];

	while( !ifscript.eof() ){
		ifscript.getline( buf, ELEMENTS(buf) );
		if( ifscript.fail() )break;

		trimleft( buf );
		if( buf[0]=='\0' || strncmp(buf, "//", 2)==0 )continue;	//skip comments
		
		char* pcmd = buf;
		char* pval = buf;
		while( *pval && !isspace(*pval) )pval++;

		if( *pval!='\0' ){
			*pval++ = '\0';
		}
		trimright( pcmd );
		strlwr( pcmd );
		trimleft( pval );

		if( strcmp( pcmd, "put" )==0 ){
			cmdPutFiles( pSession, pval );
		}else if( strcmp( pcmd, "get" )==0 ){
			cmdGetFiles( pSession, pval );
		}else if( strcmp( pcmd, "rm" )==0 ){
			cmdRemove( pSession, pval );
		}else if( strcmp( pcmd, "chmod" )==0 ){
			cmdChMod( pSession, pval );
		}else if( strcmp( pcmd, "run" )==0 ){
			cmdRunImage( pSession, pval );
		}else if( strcmp( pcmd, "mkdir" )==0 ){
			cmdMkDir( pSession, pval );
		}else if( strcmp( pcmd, "cd" )==0 ){
			cmdChDir( pSession, pval );
		}else if( strcmp( pcmd, "rmdir" )==0 ){
			cmdRmDir( pSession, pval );
		}else if( strcmp( pcmd, "ls" )==0 ){
			cmdListDir( pSession, pval );
		}else if( strcmp( pcmd, "ln" )==0 ){
			cmdSoftLink( pSession, pval );
		}else if( strcmp( pcmd, "info" )==0 ){
			cmdSysInfo( pSession );
		}else if( strcmp( pcmd, "fmode" )==0 ){
			cmdFileMode( pSession, pval );
		}else if( strcmp( pcmd, "clrdir" )==0 ){
			cmdClearDir( pSession, pval );
		}else if( strcmp( pcmd, "sys" )==0 ){
//			cmdSystem( pval );
		}else if( strcmp( pcmd, "kill" )==0 ){
//			cmdKillProcs( pSession );
		}else if( strcmp( pcmd, "idle" )==0 ){
//			cmdSetIdle( pSession, pval );
		}else if( strcmp( pcmd, "end" )==0 ){
			break;
		}else if( strcmp( pcmd, "shutdown" )==0 ){
			cmdShutdown( pSession, pval );
		}else{
			cout<<"unknownd command."<<endl;
		}
	}
	return true;
}

