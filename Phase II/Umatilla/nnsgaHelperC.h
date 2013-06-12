#pragma once

#include <stdlib.h>
#include <memory.h>
#include <stdio.h>
#include <direct.h>
#include <string.h>
#include <ctype.h>
#include <tchar.h>
#include <treetmpl.h>
#include <mathbase.h>

typedef struct _NORMALIZEPARAM
{
	REAL *prOrigMin, *prOrigMax;
	REAL *prNormMin, *prNormMax;
	int nLen;
}NormalizeParam;

inline char* strftoc( char* strFor, int nChar, char* strC )
{
	memcpy(strC, strFor, nChar*sizeof(char));
	strC[nChar] = '\0';
	return strC;
}

inline void strctof( char* strC, char* strFor, int nLen )
{
	memset( strFor, ' ', sizeof(char)*nLen );
	strcpy( strFor, strC );
	strFor[strlen(strC)] = ' ';
}
