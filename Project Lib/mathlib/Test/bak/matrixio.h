#if     _MSC_VER > 1000
#pragma once
#endif

#ifndef __MATRIXIO_H__
#define __MATRIXIO_H__
#include "mathbase.h"
#include <iostream.h>


ostream& operator << ( ostream& os, const ZRealMatrix& t );
istream& operator >> ( istream& is, ZRealMatrix& t );

#endif //__MATRIXIO_H__