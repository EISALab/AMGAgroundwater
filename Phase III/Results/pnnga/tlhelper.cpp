//#include "stdafx.h"
#include "tlhelper.h"

#include <ctype.h>
#include <tchar.h>

char* trimright( char* string )
{
	char *lpsz = string+strlen(string);

	lpsz = _tcsdec(string, lpsz);
	
	while( lpsz && _istspace(*lpsz) )
		lpsz = _tcsdec(string, lpsz);

	if( lpsz==NULL )lpsz=string;
	else lpsz++;

	*lpsz = '\0';

	return string;
}

char* trimleft( char* string )
{
	char* lpsz = string;

	while (_istspace(*lpsz))
		lpsz = _tcsinc(lpsz);

	if (lpsz != string)
	{
		// fix up data and length
		int nDataLength = strlen(string) - (lpsz - string);
		memmove(string, lpsz, (nDataLength+1)*sizeof(TCHAR));
	}
	return string;
}

char* trim( char* string )
{
	trimleft( string );
	return trimright( string );
}

BOOL SkipLines( ifstream& ifg, int nSkip )
{
	int i = 0;
	char buff[256];
	while( !ifg.eof() && i<nSkip ){
		ifg.getline( buff, 256 );
		trimleft( buff );
		trimright( buff );
		if( buff[0]!='\0' )i++;
	}
	return !ifg.eof();
}

BOOL SkipColumns( istream& ifg, int nSkip )
{
	REAL rData;
	for( int i=0; i<nSkip; i++ ){
		ifg>>rData;
	}
	return TRUE;
}

int CountRealColumns( char* buff )
{
	buff = trim(buff);

	istrstream istr( buff, strlen(buff)+1 );
	int nColumn=0;
	REAL rData;
	while( !istr.fail() ){
		istr>>rData;
		if( !istr.fail() ){
			nColumn ++;
		}
	}
	return nColumn;
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
