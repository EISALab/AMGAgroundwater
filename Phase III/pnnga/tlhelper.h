#ifndef _TLHELPER_H_
#define _TLHELPER_H_

#include <mathbase.h>

#if _MSC_VER<=1200		//for vc 6.0++, using the old iostream.h
	#include <fstream.h>
	#include <strstrea.h>
#else					//for higher version, using the iostream
	#include <fstream>
	#include <strstream>
	using namespace std;
#endif

//#include <iostream>
//#include <iomanip>
//#include <iosfwd>

typedef double REAL;
typedef int BOOL;
#define TRUE	1
#define FALSE	0
#define MAX_LINE	256

char* trimright( char* string );
char* trimleft( char* string );
char* trim( char* string );

BOOL SkipLines( ifstream& ifg, int nSkip );
BOOL SkipColumns( istream& ifg, int nSkip );
int CountRealColumns( char *buff );
int CountNonblankLines( ifstream& ifg );

#endif