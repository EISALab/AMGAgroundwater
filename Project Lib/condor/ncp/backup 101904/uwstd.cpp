#ifdef _WIN32
	#include <io.h>
#endif
#include <stdio.h>
#include "uwstd.h"
typedef long	intptr_t;

double GetMilliTime()
{
#ifdef _WIN32
	static bool bInit = false;
	static double ticks_per_ms;

	if( !bInit ){
		LARGE_INTEGER freq;
		QueryPerformanceFrequency(&freq);				//get ticks per second
		ticks_per_ms = (double)freq.QuadPart/1000.0;	//convert to milliseconds
	}

	LARGE_INTEGER counter;
	QueryPerformanceCounter(&counter);				
	return (double)counter.QuadPart/ticks_per_ms;//in milliseconds
#else
	struct timeval tp;
	gettimeofday( &tp, NULL );
	return (double)tp.tv_sec*1000 + (double)tp.tv_usec/1000;
#endif
}

int erase_dir( const char* path, bool rm_cur )
{
#ifdef _WIN32
	char buf[_MAX_PATH];
	dir_cat( path, "*.*", buf, ELEMENTS(buf) );
	struct _finddata_t fileinfo;
	intptr_t hfind = _findfirst( buf, &fileinfo );
	if( hfind==-1 ){
		errno = ENOENT;
		return -1;
	}

	do{
		char path_file[_MAX_PATH];
		dir_cat( path, fileinfo.name, path_file, ELEMENTS(path_file) );
		if( fileinfo.attrib & FILE_ATTRIBUTE_DIRECTORY ){
			if( strcmp(fileinfo.name,".")!=0 && strcmp(fileinfo.name, "..")!=0 ){
				if( erase_dir( path_file, true )!=0 ){	//clear the children path
					_findclose( hfind );
					return -1;
				}
			}
		}else{
			if( remove(path_file)!=0 ){
				_findclose( hfind );
				return -1;
			}
		}
	}while( _findnext( hfind, &fileinfo )==0 );
	_findclose( hfind );
#else
	DIR* dirp = opendir(path);
	if( dirp==NULL ){
		errno = ENOENT;
		return -1;
	}
	struct dirent * dp = readdir(dirp);
	while( dp ){
		char path_file[_MAX_PATH];
		dir_cat( path, dp->d_name, path_file, ELEMENTS(path_file) );
		if( IsDirExist(path_file) ){
			if( strcmp(dp->d_name, ".")!=0 && strcmp(dp->d_name, "..")!=0 ){
				if( erase_dir( path_file, true )!=0 ){
					closedir(dirp);
					return -1;
				}
			}
		}else{
			if( remove(path_file)!=0 ){
				closedir(dirp);
				return -1;
			}
		}
		dp = readdir(dirp);
	}
	closedir(dirp);
#endif
	if( rm_cur ){
		return rmdir( path );
	}
	return 0;
}

/* 
get_first_pathtok_pos and get_next_pathtok parse a path in forward order.
	eg. "/home/smyan/test/file" is parsed as (/, home, smyan, test, file).
		"smyan/test/file" is parsed as (smyan, test, file)

get_last_pathtok_pos and get_prev_pathtok parse a path in backward order.
	eg. "/home/smyan/test/file" is parsed as (file, test, smyan, home, /)
		"smyan/test/file" is parsed as (file, test, smyan)
*/

static char* get_first_pathtok_pos( const char *path )
{
	const char* first_tok = path;
	while( *first_tok && *first_tok==PATH_SEPTCHR )first_tok++;
	if( first_tok > path )return (char*)(first_tok-1);	//the first character is '/', point pos to '/'
	return (char*)(*first_tok ? path : NULL);		//point pos to the first token, or return NULL for ""
}

static char* get_next_pathtok( const char *tok_pos, char* path_tok, int maxlen )
{
	ASSERT( path_tok!=NULL && maxlen>0 );
	ASSERT( *tok_pos );
	const char* next_tok = tok_pos;
	if( *next_tok==PATH_SEPTCHR ){			//this is the root '/'
		if( 2>maxlen )return (char*)-1;
		strcpy( path_tok, PATH_SEPTSTR );
	}else{
		const char* ret_tok = next_tok;
		while( *next_tok && *next_tok!=PATH_SEPTCHR )next_tok++;

		if( next_tok - ret_tok >= maxlen ){
			return (char*)-1;
		}

		strncpy( path_tok, ret_tok, next_tok - ret_tok );
		path_tok[ next_tok-ret_tok ] = '\0';
	}

	while( *next_tok && *next_tok==PATH_SEPTCHR )next_tok++;
	return (char*)(*next_tok ? next_tok : NULL );
}

static char* get_last_pathtok_pos( const char *path )
{
	const char* last_tok = path + strlen(path)-1;
	while( last_tok>=path && *last_tok==PATH_SEPTCHR )last_tok--;

	if( last_tok >= path )return (char*)(last_tok);		//find a tocken, return the tocken
	return (char*)( *path==PATH_SEPTCHR ? path : NULL );			//allow a single root path "/" and ""
}

static char* get_prev_pathtok( const char *path, const char* tok_pos, char* path_tok, int maxlen )
{
	ASSERT( path_tok!=NULL && maxlen>0 );
	ASSERT( *tok_pos && tok_pos>=path );
	const char* prev_tok = tok_pos;

	if( *prev_tok==PATH_SEPTCHR){
		if( maxlen<2 )return (char*)-1;
		strcpy( path_tok, "/" );		//return the root token
		return NULL;
	}

	const char* ret_tok = prev_tok;
	while( prev_tok>=path && *prev_tok!=PATH_SEPTCHR )prev_tok--;

	if( ret_tok - prev_tok >= maxlen ){
		return (char*)-1;
	}

	strncpy( path_tok, prev_tok+1, ret_tok - prev_tok );
	path_tok[ ret_tok - prev_tok ] = '\0';

	while( prev_tok>=path && *prev_tok==PATH_SEPTCHR )prev_tok--;
	if( prev_tok >= path )return (char*)(prev_tok);
	return (char*)(*path==PATH_SEPTCHR ? path : NULL);
}

int parse_cmdline( char* cmdline, char* path, int sz_path, char**argv, int sz_argv )
{
	char* delim = " \t\n";
	//break the argvs into argv[0..sz_argv]
	argv[0] = strtok( cmdline, delim );
	int i;
	for( i=1; argv[i-1]!=NULL; i++ ){
		argv[i] = strtok( NULL, delim );
		if( i>sz_argv ){
			errno = ERANGE;
			return -1;
		}
	}
	//check if argv[0] exceeds the limits of path[0..sz_path]
	if( strlen(argv[0]) >= (size_t)sz_path ){
		errno = ERANGE;
		return -1;
	}
	//copy the full path to path[sz_path], argv[0] points only the executable filename.
	strcpy( path, argv[0] );
	char* p = strrchr( argv[0], PATH_SEPTCHR );
	if( p!=NULL ){
		argv[0] = p+1;
	}
	return i-1;
}

char* fullpath( char* abspath, const char* relpath, int maxlen )
{
	char* free_buf = NULL;
	char* buf = NULL;
	char* abs_pos = NULL;
	char* rel_pos = NULL;
	char  buf_tok[_MAX_PATH];
	int	  count = 0;

	if( !relpath || !*relpath )
		return getcwd( abspath, (int)maxlen );

	/* allocate buffer if necessary */
	if( !abspath ){
		if ( !(buf = (char*)malloc(_MAX_PATH * sizeof(char))) ) {
			errno = ENOMEM;
			return( NULL );
		}else maxlen = _MAX_PATH;
	}else{
		buf = abspath;
	}

	if( relpath[0]==PATH_SEPTCHR ){	//this is already a absolute path
		if( (int)strlen(relpath)>=maxlen ){
			errno = ERANGE;
			goto error_cleanup;
		}

		strcpy( buf, relpath );
		return ( buf );
	}

	if( buf==relpath ){				//abspath and relpath is the same address. backup relpath
		free_buf = strdup( relpath );
		relpath = free_buf;
	}

	if( !getcwd( buf, (int)maxlen ) )goto error_cleanup;

	abs_pos = get_last_pathtok_pos( buf );
	rel_pos = get_first_pathtok_pos( relpath );

	while( rel_pos ){
		rel_pos = get_next_pathtok( rel_pos, buf_tok, _MAX_PATH );
		if( rel_pos==(char*)-1 ){
			errno = ERANGE;
			goto error_cleanup;
		}

		if( strcmp(buf_tok, "..")==0 ){
			abs_pos = get_prev_pathtok( buf, abs_pos, buf_tok, _MAX_PATH );
			if( abs_pos==(char*)-1 ){
				errno = ERANGE;
				goto error_cleanup;
			}
			if( abs_pos==NULL ){
				errno = ENOENT;
				goto error_cleanup;
			};
			*(abs_pos+1)='\0';				//..trunk the token
		}else if( strcmp(buf_tok, ".")==0 ){
			//do nothing
		}else{
			ASSERT( strcmp(buf_tok, PATH_SEPTSTR)!=0 );	//rel_path can't be an absolute path.
			count = (int)(abs_pos - buf + strlen(buf_tok) + 2); //abs_pos-buf+1, plus '/'
			if( count >= maxlen ){
				errno = ERANGE;
				goto error_cleanup;
			}
			if( *abs_pos==PATH_SEPTCHR ){
				strcpy( abs_pos+1, buf_tok );
			}else{
				strcpy( abs_pos+1, PATH_SEPTSTR );
				strcat( abs_pos+2, buf_tok );
			}
			abs_pos = get_last_pathtok_pos( buf );
		}
	}

	if( free_buf )free(free_buf);
	return( buf );

error_cleanup:
	if( free_buf )free(free_buf);
	if( !abspath )free(buf);
	return (NULL);
}

char* expandpath( char* abspath, const char* workpath, const char* relpath, int maxlen )
{
	char* free_buf = NULL;
	char* buf = NULL;
	char* abs_pos = NULL;
	char* rel_pos = NULL;
	char  buf_tok[_MAX_PATH];
	int	  count = 0;

	/* allocate buffer if necessary */
	if( !abspath ){
		if ( !(buf = (char*)malloc(_MAX_PATH * sizeof(char))) ) {
			errno = ENOMEM;
			return( NULL );
		}else maxlen = _MAX_PATH;
	}else{
		buf = abspath;
	}

	if( !relpath || !*relpath ){
		if( strlen(workpath)>=(size_t)maxlen ){
			errno = ERANGE;
			goto error_cleanup;
		}
		return strcpy(buf, workpath);
	}

	if( !workpath || !*workpath || relpath[0]==PATH_SEPTCHR ){	//this is already a absolute path
		if( (int)strlen(relpath)>=maxlen ){
			errno = ERANGE;
			goto error_cleanup;
		}
		return strcpy( buf, relpath );
	}

	if( buf==relpath ){				//abspath and relpath is the same address. backup relpath
		free_buf = strdup( relpath );
		relpath = free_buf;
	}else if( buf==workpath ){
		free_buf = strdup( workpath );
		workpath = free_buf;
	}

	if( strlen(workpath)>=(size_t)maxlen ){
		errno = ERANGE;
		goto error_cleanup;
	}

	strcpy( buf, workpath );

	abs_pos = get_last_pathtok_pos( buf );
	rel_pos = get_first_pathtok_pos( relpath );

	while( rel_pos ){
		rel_pos = get_next_pathtok( rel_pos, buf_tok, _MAX_PATH );
		if( rel_pos==(char*)-1 ){
			errno = ERANGE;
			goto error_cleanup;
		}

		if( strcmp(buf_tok, "..")==0 ){
			abs_pos = get_prev_pathtok( buf, abs_pos, buf_tok, _MAX_PATH );
			if( abs_pos==(char*)-1 ){
				errno = ERANGE;
				goto error_cleanup;
			}
			if( abs_pos==NULL ){
				errno = ENOENT;
				goto error_cleanup;
			};
			*(abs_pos+1)='\0';				//..trunk the token
		}else if( strcmp(buf_tok, ".")==0 ){
			//do nothing
		}else{
			ASSERT( strcmp(buf_tok, PATH_SEPTSTR)!=0 );	//rel_path can't be an absolute path.
			count = (int)(abs_pos - buf + strlen(buf_tok) + 2); //abs_pos-buf+1, plus '/'
			if( count >= maxlen ){
				errno = ERANGE;
				goto error_cleanup;
			}
			if( *abs_pos==PATH_SEPTCHR ){
				strcpy( abs_pos+1, buf_tok );
			}else{
				strcpy( abs_pos+1, PATH_SEPTSTR );
				strcat( abs_pos+2, buf_tok );
			}
			abs_pos = get_last_pathtok_pos( buf );
		}
	}

	if( free_buf )free(free_buf);
	return( buf );

error_cleanup:
	if( free_buf )free(free_buf);
	if( !abspath )free(buf);
	return (NULL);
}

