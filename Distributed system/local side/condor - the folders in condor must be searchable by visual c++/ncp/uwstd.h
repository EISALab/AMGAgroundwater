//the file is defined to a set of function interfaces for windows, unix and linux platform.
/*---------------------------------interfaces provided------------------------------------------------
void MilliSleep( int millisec );		//Sleep milliseconds
double GetMilliTime();					//Get high performance time count in unit of milli seconds.


int parse_cmdline( char* cmdline, char* path, int sz_path, char**argv, int sz_argv )		//parse command line into tokens argv[..].
char* fullpath( char* abspath, const char* relpath, int maxlen )							//expand relative path to fullpath relative to the current path.
char* expandpath( char* abspath, const char* workpath, const char* relpath, int maxlen )	//expand relative path to fullpath relative to a given work path.

										By Shengquan Yan  EMAIL:smyan@uiuc.edu, sq_yan@hotmail.com
-------------------------------------------------------------------------------------------------------------*/

#ifndef _UWSTD_H_
#define _UWSTD_H_

//included head files
#ifdef _WIN32
	#include <windows.h>
	#include <direct.h>
#else
	#include <unistd.h>
	#include <sys/time.h>
	#include <dirent.h>
#endif

#include <stdlib.h>
#include <sys/stat.h>
#include <fcntl.h>

//common headers.
#include <errno.h>
#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include "commdef.h"

//predefined macros
#ifdef _WIN32
	#define PATH_SEPTCHR	'\\'
	#define PATH_SEPTSTR	"\\"
#else
	#define PATH_SEPTCHR	'/'
	#define PATH_SEPTSTR	"/"
#endif

#ifndef ASSERT
	#define ASSERT assert
#endif

#ifndef _MAX_PATH
	#define _MAX_PATH	512
#endif

#ifndef _MAX_ARG
	#define _MAX_ARG	64
#endif

#ifndef _MAX_LINE
	#define _MAX_LINE	512
#endif


//Sleep a thread in unit of milli seconds
inline void MilliSleep( int millisec )
{
#ifdef _WIN32
	Sleep( millisec );
#else
	usleep( millisec * 1000 );
#endif
}

//Get high performance time count in unit of milli seconds.
double GetMilliTime();

inline bool IsFileExist( const char* strFileName )
{
	struct stat flstat;
	if( stat( strFileName, &flstat )==0 && 
		isset(flstat.st_mode, S_IFREG) )return true;
	
	return false;
}

inline bool IsDirExist( const char* strDir )
{
	struct stat flstat;
	if( stat( strDir, &flstat )==0 && 
		isset(flstat.st_mode, S_IFDIR) )return true;
	
	return false;
}

inline bool IsAbsDir( const char* strDir )
{
#ifdef _WIN32
	if( strDir[0]=='\\' || strchr(strDir, ':')!=NULL )return true;
#else
	if( strDir[0]=='/' )return true;
#endif

	return false;
}

//recursively remove a directory. all the files and subfolders will be erased.
//path(in)	: the directory that will be erased.
//rm_cur	: true, the current directory will be removed. false, the current directory will not be removed, which means the current directory is emptied. 
//return : 0 if successful, otherwise failed.
int erase_dir( const char* path, bool rm_cur=true );


/*
dir_norm ensure a path has a path separator at the end.
	for example, dir_norm("myfolder") return "myfolder/"
*/
//path(in)		: the path string to be normalized.
//ret_path(out)	: the returned normalized path.
//nsize(in)		: the capacity of ret_path
//return		: return pointer to the buffer of ret_path. If failed, return NULL
inline char* dir_norm( const char* path, char* ret_path, int nsize )
{
	if( (size_t)nsize<strlen(path)+2 ){
		errno = ERANGE;
		return NULL;
	}

	if( ret_path!=path )strcpy( ret_path, path );

	if( ret_path[strlen(ret_path)-1]!=PATH_SEPTCHR ){
		strcat( ret_path, PATH_SEPTSTR );
	}
	return ret_path;
}

/*
dir_cat concatate two path into one. the returned path should be pre_path+suc_path.
	for example, dir_cat("myfolder", "child", .. ) return "myfolder/child"
*/
//pre_path, suc_path(in)		: the path strings that will be concatenated
//ret_path(out)	: the returned normalized path.
//nsize(in)		: the capacity of ret_path
//return		: return pointer to the buffer of ret_path. If failed, return NULL
inline char* dir_cat( const char* pre_path, const char* suc_path, char* ret_path, int nsize )
{
	ASSERT( ret_path!=suc_path );
	if( ret_path!=pre_path )strcpy( ret_path, pre_path );

	if( dir_norm( ret_path, ret_path, nsize )!=NULL ){
		if( (size_t)nsize<strlen(ret_path)+strlen(suc_path)+1 ){
			errno = ERANGE;
			return NULL;
		}
		return strcat( ret_path, suc_path );
	}
	return NULL;
}

//parse_cmdline parses the command line into argv[..], cmdline will be broken into tokens and argv[i] points to the tokens in cmdline. 
//cmd (in, out)			: the command line, it will be breaked into arguments.
//path[sz_path] (out)	: the path contains the path of the command (executable file).
//argv[sz_argv] (out)	: argv array contains the argv parameters that should be passed to execv. argv[0] is the command, argv[1..n] are the arguments, argv[n+1] is NULL
//return				: the function returns number of tokens (argc) if sucessful, otherwise it returns -1.
int parse_cmdline( char* cmdline, char* path, int sz_path, char**argv, int sz_argv );


//fullpath evaluates the fullpath of a relative path.
//absPath(out) : Pointer to a buffer containing the absolute or full path name. 
//relPath(in)  : Relative path name. 
//maxLen(in)   : Maximum length of the absolute path name buffer (absPath).
//return	   : returns a pointer to a buffer containing the absolute path name (absPath). If there is an error (for example, if the value passed in relPath includes a drive letter that is not valid or cannot be found, or if the length of the created absolute path name (absPath) is greater than maxLength) the function returns NULL.
char* fullpath( char* abspath, const char* relpath, int maxlen );

/*
expandpath evaluates the fullpath of a relative path given the current working path.
For example, if workpath = "/smyan/myfolder/work1", and relpath is ".././../", the the evaluated path is abspath = "/smyan"
If workpath is NULL, then abspath is returned equaling to relpath.
If workpath is the current absolute path, the function is equivelent to fullpath(...)
Workpath can be a relative path as well, for example "../smyan/myfolder/work1", if relpath is "..", the the result is "../smyan/myfolder"
*/
//absPath(out) : Pointer to a buffer containing the absolute or full path name.
//workpath(in) : Pointer to a buffer containing the current working path
//relPath(in)  : Relative path name. 
//maxLen(in)   : Maximum length of the absolute path name buffer (absPath).
//return	   : returns a pointer to a buffer containing the absolute path name (absPath). If there is an error (for example, if the value passed in relPath includes a drive letter that is not valid or cannot be found, or if the length of the created absolute path name (absPath) is greater than maxLength) the function returns NULL.
char* expandpath( char* abspath, const char* workpath, const char* relpath, int maxlen );

#endif	//_UWSTD_H
