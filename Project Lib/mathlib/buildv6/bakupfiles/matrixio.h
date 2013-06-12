#if     _MSC_VER > 1000
#pragma once
#endif

/**********************************************************************************
				io interface for matrix interface
				Copyright Shengquan Yan
				The source code is not allowed to modification without the permission of the author.
				email: smyan@uiuc.edu sq_yan@hotmail.com
***********************************************************************************/

#ifndef __MATRIXIO_H__
#define __MATRIXIO_H__
#include "mathbase.h"
#include <iostream.h>


ostream& operator << ( ostream& os, const ZRealMatrix& t );
istream& operator >> ( istream& is, ZRealMatrix& t );

#endif //__MATRIXIO_H__