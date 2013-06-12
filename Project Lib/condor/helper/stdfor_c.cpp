#include "stdfor_c.h"
#include "stdnorm.h"
#include <iostream>
#include <fstream>
#include <algorithm>

#define MAXPATH 256
#define BUFSIZE 4096

#ifndef _WIN32
char * _itoa( int val, char *buf, int radix );
#endif

using namespace std;

int CopyDir( char* strSrc, char* strDes )
{

	printf("Sorry, this function is not implemented yet\n" );
	exit(-1);

/*
	char strFullName[MAXPATH];
	mkdir( strDes, S_IRWXU | S_IRWXG | S_IROTH );

	DIR* dirp = opendir(strSrc);

	if( dirp==NULL ){
		return -1;
	}

	struct dirent * dp = readdir(dirp);
	while( dp ){
		strcpy( strFullName, strDes );
		NormalizePath( strFullName );
		char* p = ExtractFileName( dp->d_name );
		strcat( strFullName, p );
		if( CopyFile( dp->d_name, strFullName, TRUE )!=0 ){
			closedir(dirp);
			return -1;
		}

		dp = readdir(dirp);
	}

	closedir(dirp);*/

	return 0;
}

int CompareSize( char* strSrc, char* strDes )
{
	struct stat stsrc, stdes;
	if( stat(strSrc, &stsrc)!=0 )return -1;
	if( stat(strDes, &stdes)!=0 )return -1;

	if( stsrc.st_size==stdes.st_size )return 0;
	return 1;
}

int CopyFile1( char* strSrc, char* strDes, int bForce )
{
	if( !bForce ){
		if( CompareSize(strSrc, strDes)==0 )return 0;
	}
	return ::CopyFile( strSrc, strDes, FALSE );

/*	FILE* pfSrc = fopen( strSrc, "rb" );
	if( pfSrc==NULL )return -1;

	FILE* pfDes = fopen( strDes, "wb" );
	if( pfDes==NULL ){
		fclose(pfSrc);
		return -1;
	}

	char buf[BUFSIZE];
	while( !feof(pfSrc) ){
		int nRead = fread( buf, sizeof(char), BUFSIZE, pfSrc );
		if( nRead>0 ){
			fwrite( buf, sizeof(char), nRead, pfDes );
		}
	}
	fclose( pfSrc );
	fclose( pfDes );
	struct stat statsrc;
	stat( strSrc, &statsrc );
	chmod( strDes, statsrc.st_mode );
	return 0;*/
}

int CountNonblankLines( ifstream& ifg )
{
	char buf[400]="";

	streampos pos = ifg.tellg();
	int nLines = 0;

	ifg.getline( buf, ELEMENTS(buf) );
	do{
		trim(buf);
		if( buf[0]!=NULL )nLines++;

		ifg.getline( buf, ELEMENTS(buf) );
	}while( !ifg.fail() );
	ifg.clear();
	ifg.seekg( pos, ios::beg );
	return nLines;
}

extern "C"{
	double _stdcall STR_TO_DBLE( char* pstr, int nCount )
	{
		char buf[MAXPATH];
		strftoc( pstr, nCount, buf );
		return atof( buf );
	}
	double _stdcall STRTOFC( char* pstr, int nCount )
	{ return STR_TO_DBLE( pstr, nCount ); }


	void _stdcall INT_TO_STR( int& val, char* pzRet, int nChar )
	{
		char buf[MAXPATH];
		itoa( val, buf, 10 );
		strctof( buf, pzRet, nChar );
	}

	void _stdcall STR_CAT( char* pstr1, int nChar1, char* pstr2, int nChar2, char* pstrRet, int nCharRet )
	{
		char buf1[MAXPATH];
		char buf2[MAXPATH];
		strftoc( pstr1, nChar1, buf1 );
		strftoc( pstr2, nChar2, buf2 );
		strcat( buf1, buf2 );
		strctof( buf1, pstrRet, nCharRet );
	}

	int _stdcall COPY_FILE( char* pSrcFile, int nSrcChar, char* pDesFile, int nDesChar )
	{
		char strSrc[MAXPATH];
		char strDes[MAXPATH];
		if( nSrcChar>=MAXPATH || nDesChar>=MAXPATH ){
			return -1;
		}

		strftoc( pSrcFile, nSrcChar, strSrc );
		strftoc( pDesFile, nDesChar, strDes );
	
		return CopyFile( strSrc, strDes, TRUE );
	}

	int _stdcall COPYFILEC( char* pSrcFile, int nSrcChar, char* pDesFile, int nDesChar )
	{ return COPY_FILE( pSrcFile, nSrcChar, pDesFile, nDesChar ); }

	//a simple remove path with all the files in it. all sub directories are removed as well
	int _stdcall REMOVE_DIR( char* pStrPath, int nChar )
	{
		char strPath[MAXPATH];
		if( nChar>=MAXPATH ){
			return -1;
		}

		strftoc( pStrPath, nChar, strPath );
		return erase_dir( strPath, true );
	}

	int _stdcall REMOVEDIRC( char* pStrPath, int nChar )
	{ return REMOVE_DIR( pStrPath, nChar ); }


	int _stdcall REMOVE_FILE( char* pStrFile, int nChar )
	{
		char strFile[MAXPATH];
		if( nChar>=MAXPATH ){
			return -1;
		}

		strftoc( pStrFile, nChar, strFile );
		return remove( strFile );
	}

	int _stdcall REMOVEFILEC( char* pStrFile, int nChar )
	{ return REMOVE_FILE( pStrFile, nChar ); }

	int _stdcall COPY_DIR( char* pSrcPath, int nSrcChar, char* pDesPath, int nDesChar )
	{
		char strSrc[MAXPATH];
		char strDes[MAXPATH];
		if( nSrcChar>=MAXPATH || nDesChar>=MAXPATH ){
			return -1;
		}

		strftoc( pSrcPath, nSrcChar, strSrc );
		strftoc( pDesPath, nDesChar, strDes );

		return CopyDir( strSrc, strDes );
	}

	int _stdcall COPYDIRC( char* pSrcPath, int nSrcChar, char* pDesPath, int nDesChar )
	{ return COPY_DIR( pSrcPath, nSrcChar, pDesPath, nDesChar ); }

	void _stdcall EXECUTE( char* pStrExec, int nStrChar )
	{
		char str[MAXPATH];
		if( nStrChar>=MAXPATH ){
			return;
		}

		strftoc( pStrExec, nStrChar, str );
		system( str );
	}


	void _stdcall WAITEFOREXEC( char* pStrExec, int nStrChar )
	{	EXECUTE( pStrExec, nStrChar ); }

	int _stdcall COUNT_NONBLANK_LINES( char* pStrFile, int nStrChar )
	{
		char strFile[MAXPATH];
		if( nStrChar > MAXPATH )exit(1);

		strftoc( pStrFile, nStrChar, strFile );
		return CountNonblankLines( ifstream(strFile) );
	}

}

bool AbsLess( double e1, double e2 )
{
	return fabs(e1)<fabs(e2);
}

extern "C"{
	int _stdcall FIND( int* start, int& count, int& elem )
	{
		int* end = start + count;
		int* pos = std::find( start, end, elem );
		if( pos!=end )return TRUE;
		else return FALSE;
	}
	void _stdcall ABS_SORT( double* start, int& count )
	{
		std::sort( start, start+count, AbsLess );
	}
	double _stdcall NORM_CDF( double& x )
	{
		return stdnormal_cdf( x );
	}
}


#ifndef _WIN32

/* helper routine that does the main job. */

static void xtoa (
        unsigned long val,
        char *buf,
        unsigned radix,
        int is_neg
        )
{
        char *p;                /* pointer to traverse string */
        char *firstdig;         /* pointer to first digit */
        char temp;              /* temp char */
        unsigned digval;        /* value of digit */

        p = buf;

        if (is_neg) {
            /* negative, so output '-' and negate */
            *p++ = '-';
            val = (unsigned long)(-(long)val);
        }

        firstdig = p;           /* save pointer to first digit */

        do {
            digval = (unsigned) (val % radix);
            val /= radix;       /* get next digit */

            /* convert to ascii and store */
            if (digval > 9)
                *p++ = (char) (digval - 10 + 'a');  /* a letter */
            else
                *p++ = (char) (digval + '0');       /* a digit */
        } while (val > 0);

        /* We now have the digit of the number in the buffer, but in reverse
           order.  Thus we reverse them now. */

        *p-- = '\0';            /* terminate string; p points to last digit */

        do {
            temp = *p;
            *p = *firstdig;
            *firstdig = temp;   /* swap *p and *firstdig */
            --p;
            ++firstdig;         /* advance to next two digits */
        } while (firstdig < p); /* repeat until halfway */
}

/* Actual functions just call conversion helper with neg flag set correctly,
   and return pointer to buffer. */

char * _itoa (
        int val,
        char *buf,
        int radix
        )
{
        if (radix == 10 && val < 0)
            xtoa((unsigned long)val, buf, radix, 1);
        else
            xtoa((unsigned long)(unsigned int)val, buf, radix, 0);
        return buf;
}

#endif
