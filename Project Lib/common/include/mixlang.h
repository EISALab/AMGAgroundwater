/*
	mixlang.h contains functions that make mixed language programming easier.
		By Shengquan Yan 06/23/05
*/

#pragma once

#include <stdlib.h>
#include <memory.h>
#include <string.h>

/* 
	functions that support mixed C and Fortran programming
*/


//convert fortran string to C string.
//fotran string doesn't have '\0', the string is always padded by blank.
inline char* strftoc( const char* strFor, int nChar, char* strC )
{
	memcpy(strC, strFor, nChar*sizeof(char));

	//find the padding position
	int pos;
	for( pos = nChar-1; pos>=0; pos-- ){
		if( strC[pos]!=' ' )break;
	}

	strC[pos+1] = '\0';
	return strC;
}

//convert C string to Fortran string.
inline void strctof( const char* strC, char* strFor, int nLen )
{
	memset( strFor, ' ', sizeof(char)*nLen );
	strcpy( strFor, strC );

	//pad the string with blank.
	strFor[strlen(strC)] = ' ';
}
