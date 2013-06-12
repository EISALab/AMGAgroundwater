#pragma once

#include "netbase.h"
#include <sys/stat.h>
#include <fcntl.h>
#include "uwkelobj.h"

//compitable head files for win32, unix and linux.
#ifdef _WIN32
#include <direct.h>
#else
#define O_TEXT		0x4000
#define O_BINARY	0x8000
#endif

string GetHostByAddr( IN_ADDR in_addr );

#define NETS_IWRITE		0x200
#define NETS_IREAD		0x400
#define NETS_IEXEC		0x100

inline string GetLocalName()
{
	char buf[_MAX_LINE];
	if( gethostname(buf, ELEMENTS(buf))!=0 )throw new CSockException();

	return buf;
}

inline IN_ADDR GetHostInAddr( const string& hostname )
{
	hostent* phent = NULL;
	if( hostname.empty() )phent = gethostbyname( GetLocalName().c_str() );
	else phent = gethostbyname( hostname.c_str() );

	if( phent==NULL )throw new CSockException();

	IN_ADDR addrloc;
	bcopy( phent->h_addr, &addrloc, sizeof(IN_ADDR) );
	return addrloc;
}

inline IN_ADDR GetLocalInAddr()
{
	return GetHostInAddr( GetLocalName() );
}

inline IN_ADDR GetHostInAddr( char* hostname )
{
	char buf[_MAX_LINE];

	if( hostname==NULL ){
		hostname = buf;
		//get local host address
		gethostname( buf, ELEMENTS(buf) );
	}
	hostent* phent = gethostbyname( hostname );
	IN_ADDR addrloc;
	bcopy( phent->h_addr, &addrloc, sizeof(IN_ADDR) );
	return addrloc;
}

inline int RedundantConnect( SOCKET sock, SOCKADDR_IN& addr, int nRetry=5 )
{
	int nRet = 0;
	for( int i=0; i<nRetry; i++ ){
        nRet = ::connect( sock, (SOCKADDR*)&addr, sizeof(SOCKADDR_IN) );
		if( nRet==0 )break;
	}
	return nRet;
}


typedef struct netf_stat
{
	short nfs_nlink;
	unsigned short nfs_mode;
	int nfs_size;
	time_t nfs_atime;
	time_t nfs_mtime;
	time_t nfs_ctime;
}NETF_STAT, *PNETF_STAT;

typedef struct netf_entry
{
	char fname[64];
	NETF_STAT fstat;
}NETF_ENTRY;

inline void hton( NETF_STAT& stat )
{
	stat.nfs_nlink = htons( stat.nfs_nlink );
	stat.nfs_mode = htons( stat.nfs_mode );
	stat.nfs_size = htonl( stat.nfs_size );
	stat.nfs_atime = htonl( stat.nfs_atime );
	stat.nfs_mtime = htonl( stat.nfs_mtime );
	stat.nfs_ctime = htonl( stat.nfs_ctime );
}

inline void ntoh( NETF_STAT& stat )
{
	stat.nfs_nlink = ntohs( stat.nfs_nlink );
	stat.nfs_mode = ntohs( stat.nfs_mode );
	stat.nfs_size = ntohl( stat.nfs_size );
	stat.nfs_atime = ntohl( stat.nfs_atime );
	stat.nfs_mtime = ntohl( stat.nfs_mtime );
	stat.nfs_ctime = ntohl( stat.nfs_ctime );
}

inline void hton( NETF_ENTRY& fentry) 
{
	hton( fentry.fstat );
}

inline void ntoh( NETF_ENTRY& fentry )
{
	ntoh( fentry.fstat );
}

inline bool IsSubDir( const char* strSub, const char* strDir )
{
	return true;
}

inline int MkDir( const char* strDir )
{
#ifdef _WIN32
		return mkdir( strDir );
#else
		return mkdir( strDir, S_IREAD|S_IWRITE|S_IEXEC );
#endif
}

template<class Function>
void ParseHostName( const char* strHostName, Function _Func )
{
	char buf[_MAX_LINE];
	strcpy( buf, strHostName );

	char* pl = strchr( buf, '[' );
	char* pm = NULL;
	if( pl )pm = strchr( pl, '-' );
	char* pr = NULL;
	if( pm )pr = strchr( buf, ']' );
	if( pl && pm && pr && pm>pl && pr>pm ){
		int nx = atoi( pl+1 );
		int ny = atoi( pm+1 );
		if( nx>ny )std::swap(nx,ny);

		*pl = '\0';
		pl = buf;	//pl left part
		pr = pr+1;	//pr right part

		//concatate the name.
		char bufname[_MAX_LINE];
		for( int i=nx; i<=ny; i++ ){
			sprintf( bufname, "%s%d%s", pl, i, pr );
			hostent* phent = gethostbyname(bufname);
			if( phent!=NULL ){
				IN_ADDR addr;
				bcopy( phent->h_addr, &addr, sizeof(IN_ADDR) );
				_Func( addr );
			}
		}
	}else{
		//can't parse the host name, add it directly
		hostent* phent = gethostbyname(strHostName);
		if( phent!=NULL ){
			IN_ADDR addr;
			bcopy( phent->h_addr, &addr, sizeof(IN_ADDR) );
			_Func( addr );
		}
	}
}

template<class Function>
void ParseHostFile( const char* strNodeFile, Function _Func )
{
	ifstream ifg(strNodeFile);
	if( ifg.fail() )return;

	char buf[_MAX_LINE];
	ifg.getline(buf, ELEMENTS(buf));
	while( !ifg.fail() ){
		trimleft( buf );
		if( strncmp(buf, "//", 2)!=0 ){
			ParseHostName( buf, _Func );
		}
		ifg.getline(buf, ELEMENTS(buf));
	}
}


int SendDirEntries( SOCKET sock, const char* path );

int GetNetfStat( const char* strFile, PNETF_STAT pStat );
void SendNetfStat( SOCKET sock, PNETF_STAT* pStat );
void RecvNetfStat( SOCKET sock, PNETF_STAT* pStat );

bool SendFileStream( SOCKET sock, FILE* pf, int nBufSize=0x1000 );
bool RecvFileStream( SOCKET sock, FILE* pf, unsigned long flsize, int nBufSize=0x1000 );

long SendFileStreamEx( SOCKET sock, FILE* pf, int nBufSize=0x1000 );
long RecvFileStreamEx( SOCKET sock, FILE* pf, long flsize=-1, int nBufSize=0x1000 );

//SendFile send a file through a connected stream.
//sock			the socket for the connected stream.
//strFileName	the filename of the file that will be sent
//nBufSize		the buffer size of each package.
long SendFileEx( SOCKET sock, const char* strFileName, int nFileMode, int nBufSize=0x1000 );
long RecvFileEx( SOCKET sock, const char* strFileName, int nFileMode, int nBufSize=0x1000 );

//SendFile send a file through a connected stream.
//sock			the socket for the connected stream.
//strFileName	the filename of the file that will be sent
//nBufSize		the buffer size of each package.
bool SendFile( SOCKET sock, const char* strFileName, int nBufSize=0x1000 );
bool RecvFile( SOCKET sock, const char* strFileName, int nBufSize=0x1000 );
