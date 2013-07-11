#include "mathbase.h"
#include "matrixln.h"

BOOL MatrixSolve( const ZRealMatrix& mxA, const ZRealMatrix& mxB, ZRealMatrix& mxX )
{
	ASSERT( mxA.GetRow()==mxA.GetCol() );
	ASSERT( mxB.GetRow()==mxA.GetRow() );

	ZRealMatrix tempA(mxA);

	BOOL bRet = FALSE;
	if( mxB.GetCol()==1 ){
		ZRealMatrix tempB(mxB);
		bRet = Gauss( tempA.GetMatrixData(), tempB.GetMatrixData(), mxX.GetMatrixData(), tempA.GetRow() );
	}else{
		mxX = mxB;
		bRet = GaussJordan( tempA.GetMatrixData(), mxX.GetMatrixData(), mxA.GetRow(), mxX.GetCol() );
	}
	return bRet;
}

