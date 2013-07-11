#ifndef __MATRIXLN_H__
#define __MATRIXLN_H__
/**********************************************************************************
				Linear algebra for matrix interface
				Copyright Shengquan Yan
				The source code is not allowed to modification without the permission of the author.
				email: smyan@uiuc.edu sq_yan@hotmail.com
***********************************************************************************/

#include "mathlnag.h"

//calculate the invert matrix of square matrix
inline BOOL MatrixInvert( ZRealMatrix& mx )
{
	ASSERT( mx.GetRow()==mx.GetCol() );
	return GaussJordanInv( mx.GetMatrixData(), mx.GetRow() );
}

//calculate the determinant value of square matrix
inline REAL MatrixDetVal( const ZRealMatrix& mx )
{
	ASSERT( mx.GetRow()==mx.GetCol() );
	ZRealMatrix temp(mx);
	return GaussDetVal( temp.GetMatrixData(), temp.GetRow() );
}

//decompose the LU matrix for square matrix mxA
inline BOOL MatrixDecomLU( const ZRealMatrix& mxA, ZRealMatrix& mxL, ZRealMatrix& mxU )
{
	ASSERT( mxA.GetRow()==mxA.GetCol() && mxL.GetRow()==mxL.GetCol() && mxU.GetRow()==mxU.GetCol() );
	ASSERT( mxL.GetRow()==mxA.GetRow() && mxU.GetRow()==mxA.GetRow() );
	return DecompLU( mxA.GetBuffAddr(), mxA.GetRow(), mxL.GetMatrixData(), mxU.GetMatrixData() );
}

//solve linear equation mxA*mxX=mxB (mxX = mxB/mxA)
//mxB can have more than one column
BOOL MatrixSolve( const ZRealMatrix& mxA, const ZRealMatrix& mxB, ZRealMatrix& mxX );
#endif //__MATRIXLN_H__