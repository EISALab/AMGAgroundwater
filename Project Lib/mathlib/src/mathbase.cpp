#include <memory.h>
#include "mathbase.h"
#include "mathfunc.h"
#include "matrixln.h"
#include "stdio.h"

#ifdef _MFC_VER
	#ifdef _DEBUG
		#define new DEBUG_NEW
	#endif
#endif

#ifdef _DEBUG
//#define new DEBUG_NEW
#endif

MATH_STATIC_DATA ZMatrixHead _mxInitHead = { 0, 0, -1, 0, 0 };
MATH_STATIC_DATA ZMatrixHead* _pmxHeadNil = &_mxInitHead;

#define MATRIX_HEAD_DATABUF(pmxHead)	(REAL*)((BYTE*)(pmxHead)+sizeof(ZMatrixHead))

/*------------------------------------------------------------------------*/
/*                                                                        */
/*  ZRealMatrix member function                                               */
/*                                                                        */
/*------------------------------------------------------------------------*/

ZRealMatrix ZRealMatrix::PowerPosInt( const ZRealMatrix& mx, int n )
{
	ASSERT( n>=1 );
	ASSERT( !mx.IsNull() );
	ASSERT( mx.GetRow()==mx.GetCol() );
	
	if( n==1 )return mx;

	if( n%2==0 ){
		ZRealMatrix temp;
		temp = PowerPosInt(mx, n/2 );
		return ::Mul( temp, temp );
	}else{
		ZRealMatrix temp;
		temp = PowerPosInt(mx, (n-1)/2);
		//this is to reduce the unnecessary of memory alloc and dealloc.
		Mul( ::Mul(temp, temp), mx, temp );
		return temp;
	}
}

void ZRealMatrix::Power( const ZRealMatrix& mx, int n, ZRealMatrix& mxRet )
{
	ASSERT( mxRet.GetRow()==mx.GetRow() && mxRet.GetCol()==mxRet.GetCol() );
	ASSERT( mx.GetRow()==mx.GetCol() );
	if( mx.IsNull() || n==1 ){
		mxRet = mx;
		return;
	}

	if( n==0 ){
		mxRet.SetIdentity();
		return;
	}
	mxRet = PowerPosInt( mx, abs(n) );
	if( n<0 ){
		mxRet.Invert();
	}
	return;
}

//the only truly multiplication calculation
void ZRealMatrix::Mul( const ZRealMatrix& mx1, const ZRealMatrix& mx2, ZRealMatrix& mxRet )
{
	ASSERT( mxRet.GetRow()==mx1.GetRow() && mxRet.GetCol()==mx2.GetCol() );

	mxRet.SetUniform( 0.0 );
	REAL* prMxData1 = mx1.GetMatrixAddr();
	REAL* prMxData2 = mx2.GetMatrixAddr();
	REAL* prMxRet = mxRet.GetMatrixAddr();
	ASSERT( prMxRet!=prMxData1 && prMxRet!=prMxData2 );

	for( int i=0; i<mx1.GetRow(); i++ ){
		for( int j=0; j<mx2.GetCol(); j++ ){
			for( int k=0; k<mx1.GetCol(); k++ ){
				prMxRet[i*mxRet.GetCol()+j] += prMxData1[i*mx1.GetCol()+k]*prMxData2[k*mx2.GetCol()+j];
			}
		}
	}
}

/*----------------------------------------------------------------------------------
			protected member function of ZRealMatrix
----------------------------------------------------------------------------------*/
void ZRealMatrix::Init()
{
	m_pMatrixHead = _pmxHeadNil;
}

ZMatrixHead* ZRealMatrix::GetData() const
{
	return m_pMatrixHead;
}

REAL* ZRealMatrix::GetMatrixAddr() const
{
	return GetData()->pMatrixData;
}

int ZRealMatrix::InternalAddRef()
{
	ASSERT( !IsNull() );
	GetData()->nRefs++;
	return GetData()->nRefs;
}

int ZRealMatrix::InternalRelease()
{
	ASSERT( !IsNull() );
	GetData()->nRefs--;
	return GetData()->nRefs;
}

void ZRealMatrix::AllocBuffer( int nRow, int nCol )
{
	ASSERT( IsNull() );
	ASSERT( nRow>=1 && nCol>=1 );

	int nSize = nRow*nCol;
	if( nSize==0 ){
		Init();
	}else{
		m_pMatrixHead = (ZMatrixHead*)new BYTE[sizeof(ZMatrixHead)+sizeof(REAL)*nSize];
		m_pMatrixHead->nAllocLen = nSize;
		m_pMatrixHead->nRow = nRow;
		m_pMatrixHead->nCol = nCol;
		m_pMatrixHead->nRefs = 0;
//		m_pMatrixHead->bTemp = FALSE;
		m_pMatrixHead->pMatrixData = MATRIX_HEAD_DATABUF(m_pMatrixHead);

		InternalAddRef();
//		printf("Alloc buffer %d-%d\n", nRow, nCol );
	}

}

void ZRealMatrix::AllocHeadSite()
{
	ASSERT( IsNull() );
	//the 1 extra byte is a trick to prevent inadvent memory releasing error.
	//when the matrix is initialized by attaching. the attached memory is ensured not equaling to pHeadAddr+sizeof(ZMatrixHead) because of the extra 1 byte is allocated to ZMatrixHead.
	//release function decides if the memory is attached or allocated by allocbuffer, then correctly releasing memory
	//In windows system, the case should not happen, but this trick may be helpful in other operating system
	m_pMatrixHead = (ZMatrixHead*)new BYTE[sizeof(ZMatrixHead)+1];
	m_pMatrixHead->nAllocLen = 0;
	m_pMatrixHead->nRow = 0;
	m_pMatrixHead->nCol = 0;
//	m_pMatrixHead->bTemp = FALSE;
	//lock the matrix head, becaue this allocation is for matrix attaching.
	m_pMatrixHead->nRefs = -1;
	m_pMatrixHead->pMatrixData = NULL;

//	printf("Alloc head site\n" );
}

void ZRealMatrix::Release()
{
	ASSERT( GetData()!=NULL );

	if( !IsNull() ){
		ASSERT(GetData()->nRefs != 0);
		if( InternalRelease()<=0 ){
			REAL* pMatrixData = GetData()->pMatrixData;
			if( NULL!=pMatrixData && pMatrixData!=MATRIX_HEAD_DATABUF(GetData()) ){
				//the memory is attached to matrix, release first
				delete[] pMatrixData;
			}
//			printf("Releasing buffer %d-%d\n", GetRow(), GetCol() );
			delete[] m_pMatrixHead;
		}
		Init();
	}
}

void ZRealMatrix::AllocBeforeCopy(int nRow, int nCol)
{
	ASSERT( nRow*nCol >0 );

	int nSize = nRow*nCol;
	if( (GetData()->nRefs<=1) && (GetData()->nAllocLen>=nSize) ){
		//The copying buffer is exclusively owned by it
		//and the holding buffer is long enough, 
		//so do not bother reallocating buffer again
		GetData()->nRow = nRow;
		GetData()->nCol = nCol;
	}else if( IsNull() || GetData()->nRefs>=1 ){
		//the buffer is referenced by other matrix, or the holding buffer is not big enough,
		//must alloc a new buffer the copy the matrix data
		Release();
		AllocBuffer( nRow, nCol );
	}else{
		//do not allow locked small buffer here
		ASSERT(FALSE);
	}
	ASSERT(GetData()->nRefs <= 1);
}

void ZRealMatrix::CopyBeforeWrite()
{
//	if ( !GetData()->bTemp && GetData()->nRefs > 1){
	if ( GetData()->nRefs > 1){
		//clone the data;
		ZMatrixHead* pmxHead = GetData();
		Release();
		AllocBuffer(pmxHead->nRow, pmxHead->nCol);
		memcpy( GetMatrixAddr(), pmxHead->pMatrixData, GetSize()*sizeof(REAL) );
	}
	ASSERT(IsNull() || GetData()->nRefs <= 1);
}

void ZRealMatrix::AssignCopy( const REAL* pMxAddr, int nRow, int nCol )
{
	//when doing a true memory copy,
	//if this is locked, always assume the buffer is long enough, if not may not copy correctly!
	//if this is not locked, release and allocate enough memory
	if( IsNull() || GetData()->nRefs>=1 ){
		//prepare the buffer, if the current buffer is big enough and singled referenced
		//then don't bother allocate another one, the original buffer is used.
		AllocBeforeCopy( nRow, nCol );
	}else{
		//the buffer is locked
		ASSERT( GetSize()>=nRow*nCol );
		if( GetSize()>=nRow*nCol ){
			GetData()->nRow=nRow;
			GetData()->nCol=nCol;
			memcpy( GetMatrixAddr(), pMxAddr, GetSize()*sizeof(REAL) );
		}
	}
	
	memcpy( GetMatrixAddr(), pMxAddr, GetSize()*sizeof(REAL) );
	return;
}

/*----------------------------------------------------------------------------------
			public property member function of ZRealMatrix
----------------------------------------------------------------------------------*/
int ZRealMatrix::Index( int i, int j )const
{
	ASSERT( i>=m_nRowBase && i<m_nRowBase+GetRow() );
	ASSERT( j>=m_nColBase && j<m_nColBase+GetCol() );

	return (i-m_nRowBase)*GetCol() + j-m_nColBase;
}

int ZRealMatrix::GetRow() const
{
	return GetData()->nRow;
}

int ZRealMatrix::GetCol() const
{
	return GetData()->nCol;
}

int ZRealMatrix::GetSize() const
{
	return GetRow()*GetCol();
}

REAL* ZRealMatrix::GetMatrixData()
{
	CopyBeforeWrite();
	return GetData()->pMatrixData;
}

const REAL* ZRealMatrix::GetBuffAddr() const
{
	return GetData()->pMatrixData;
}

void ZRealMatrix::SetBaseIndex( int nRowBase, int nColBase )
{
	m_nRowBase = nRowBase;
	m_nColBase = nColBase;
}

void ZRealMatrix::GetBaseIndex( int& nRowBase, int& nColBase ) const
{
	nRowBase = m_nRowBase;
	nColBase = m_nColBase;
}

BOOL ZRealMatrix::IsNull() const
{
	return GetData()==_pmxHeadNil; 
}
/*----------------------------------------------------------------------------------
			public operator member function of ZRealMatrix
----------------------------------------------------------------------------------*/

ZRealMatrix::ZRealMatrix()
{
	Init();
	m_nRowBase = m_nColBase = 1;
}

ZRealMatrix::ZRealMatrix( int m, int n )
{
	Init();
	m_nRowBase = m_nColBase = 1;
	Create( m, n );
}

ZRealMatrix::ZRealMatrix( int m, int n, REAL rElement )
{
	Init();
	m_nRowBase = m_nColBase = 1;
	Create( m, n, rElement );
}

ZRealMatrix::ZRealMatrix( int m, int n, const REAL* prData )
{
	Init();
	m_nRowBase = m_nColBase = 1;
	Create( m, n, prData );
}

ZRealMatrix::ZRealMatrix( int m, int n, const REAL** pprData )
{
	Init();
	m_nRowBase = m_nColBase = 1;
	Create( m, n, pprData ); 
}

ZRealMatrix::ZRealMatrix( const ZRealMatrix& mxInit )
{
	ASSERT(mxInit.GetData()->nRefs != 0);
	Init();
	m_nRowBase = m_nColBase = 1;

	Create( mxInit );
}

ZRealMatrix::~ZRealMatrix()
{
	Release();
}

void ZRealMatrix::Create( int m, int n )
{
	ASSERT( IsNull() );
	AllocBuffer( m, n );
}

void ZRealMatrix::Create( int m, int n, REAL rElement )
{
	ASSERT( IsNull() );
	Create( m, n );
	int nSize = GetSize();
	REAL* prMatrixData = GetMatrixAddr();
	for( int i=0; i<nSize; i++ ){
		prMatrixData[i] = rElement;
	}
}

void ZRealMatrix::Create( int m, int n, const REAL* prData )
{
	ASSERT( IsNull() );
	Create( m, n );
	REAL *prMatrixData = (REAL*)GetMatrixAddr();
	memcpy( prMatrixData, prData, sizeof(REAL)*m*n );
}

void ZRealMatrix::Create( int m, int n, const REAL** pprData )
{
	ASSERT( IsNull() );

	Create( m, n );
	REAL *prMatrixData = (REAL*)GetMatrixAddr();
	for( int i=0; i<m; i++ ){
		for( int j=0; j<n; j++ ){
			prMatrixData[i*n+j] = pprData[i][j];
		}
	}
}

void ZRealMatrix::Create( const ZRealMatrix& mxInit )
{
	ASSERT( IsNull() );
	ASSERT(mxInit.GetData()->nRefs != 0);

	if(mxInit.GetData()->nRefs >= 0){
		//just pass the reference
		ASSERT(!mxInit.IsNull());
		m_pMatrixHead = mxInit.GetData();
		if( !mxInit.IsNull() )InternalAddRef();
	}else{
		//do a truly memory copy
		AssignCopy( mxInit.GetMatrixAddr(), mxInit.GetRow(), mxInit.GetCol() );
	}
//	GetData()->bTemp = FALSE;
}

//creating a tempory matrix for calculation or other purpose.
//a tempory matrix indicates the memory buffer is not protected by CopyBeforeWrite even it is referenced by more than one tempory memory.
//this trick can significantly reduce the memory allocation and deallocation for multiple operator syntax for example (matA+5)*0.5+3
//whenever a tempory is assigned to a non-tempory matrix, the tempory flag is turned off, so if the flag is turned on, the referencees must be all tempory matrix.
/*void ZRealMatrix::CreateTemp( const ZRealMatrix& mxInit )
{
	ASSERT( IsNull() );
	ASSERT(mxInit.GetData()->nRefs != 0);

	if(mxInit.GetData()->bTemp){
		//just pass the reference
		ASSERT(!mxInit.IsNull());
		m_pMatrixHead = mxInit.GetData();
		if( !mxInit.IsNull() )InternalAddRef();
	}else{
		//do a truly memory copy and turn on the tempory flag
		AssignCopy( mxInit.GetMatrixAddr(), mxInit.GetRow(), mxInit.GetCol() );
		GetData()->bTemp = TRUE;
	}
}*/

void ZRealMatrix::SetUniform( REAL rElem )
{
	ASSERT( !IsNull() );

	CopyBeforeWrite();

	REAL* pMxData = GetMatrixAddr();
	int nSize = GetSize();
	for( int i=0; i<nSize; i++ ){
		pMxData[i] = rElem;
	}
}

void ZRealMatrix::SetIdentity()
{
	ASSERT( !IsNull() );
	CopyBeforeWrite();

	REAL* pMxData = GetMatrixAddr();
	int nDim = min( GetRow(), GetCol() );
	memset( pMxData, 0, sizeof(REAL)*GetSize() );

	REAL *pMxEnd = GetMatrixAddr() + nDim*nDim;
	while( pMxData<=pMxEnd ){
		*pMxData = 1.0;
		pMxData += nDim + 1;
	}
/*	for( int i=0; i<nDim; i++ ){
		pMxData[i*GetCol()+i] = 1.0;
	}*/
}

void ZRealMatrix::SetRandom( REAL rLow, REAL rUpper )
{
	ASSERT( !IsNull() );

	CopyBeforeWrite();

	REAL* pMxData = GetMatrixAddr();
	int nSize = GetSize();
	for( int i=0; i<nSize; i++ ){
		pMxData[i] = UniRand()*(rUpper-rLow)+rLow;
	}
}

void ZRealMatrix::CopyBuffer( const REAL* prData, int nCount, int nOffset, int nSkip )
{
	ASSERT( !IsNull() );

	CopyBeforeWrite();

	REAL* pMx = GetMatrixAddr()+nOffset;

	REAL* pMxEnd = GetMatrixAddr() + GetSize();

	for( int i=0; i<nCount && pMx<pMxEnd; i++ ){
		(*pMx) = (*prData++);
		pMx += 1+nSkip;
	}
}

void ZRealMatrix::CopyBuffer( const REAL **pprData, int m, int n, int nOffset, int nSkip )
{
	ASSERT( !IsNull() );

	CopyBeforeWrite();

	REAL* pMx = GetMatrixAddr()+nOffset;

	REAL* pMxEnd = GetMatrixAddr() + GetSize();

	for( int i=0; i<m; i++ ){
		for( int j=0; j<n; j++ ){
			if( pMx>=pMxEnd )return;
			(*pMx) = pprData[i][j];

			pMx += 1+nSkip;
		}
	}
}

void ZRealMatrix::Attach( REAL* pData, int nRow, int nCol )
{
	ASSERT( IsNull() );

	//alloc a wrap headsite to accept the outside pData buffer
	AllocHeadSite();
	GetData()->nRow = nRow;
	GetData()->nCol = nCol;
	GetData()->nAllocLen = nRow*nCol;

	GetData()->pMatrixData = pData;
}

REAL* ZRealMatrix::Detach()
{
	ASSERT( !IsNull() );
	ASSERT( GetData()->nRefs<=1 );
	ASSERT( MATRIX_HEAD_DATABUF(GetData()) != GetData()->pMatrixData );

	REAL* prMxData = NULL;
	if( MATRIX_HEAD_DATABUF(GetData()) != GetData()->pMatrixData ){
		prMxData = GetMatrixAddr();
		GetData()->pMatrixData = NULL;
	}else{
		//the attached buffer must be unlocked by the user and is reallocated during operation
		//copy a new buffer to the buffer. Note the assert doesn't permit this case
		prMxData = new REAL[GetSize()];
		memcpy( prMxData, GetMatrixAddr(), GetSize() );
	}
	Release();

	return prMxData;
}

void ZRealMatrix::UnlockBuffer()
{
	ASSERT( IsLocked() );
	if( IsNull() ){
		return;
	}
	if( IsLocked() ){
		GetData()->nRefs = 1;
	}
}

void ZRealMatrix::LockBuffer()
{
	if( IsNull() || GetData()->nRefs==-1 ){
		return;
	}

	CopyBeforeWrite();

	GetData()->nRefs = -1;
}

BOOL ZRealMatrix::IsLocked()
{
	return GetData()->nRefs==-1;
}

//-------------------------------------------------------------------------------------------------
ZRealMatrix& ZRealMatrix::Transpose()
{
	if( IsNull() )return *this;

	if( GetRow()==1 || GetCol()==1 ){
		CopyBeforeWrite();
		Swap( GetData()->nRow, GetData()->nCol );
		return *this;
	}
	//copy assign operator will take care reference.
	(*this) = GetTransposeMx();
	return (*this);
}

ZRealMatrix& ZRealMatrix::Invert()
{
	ASSERT( GetRow() == GetCol() );

	CopyBeforeWrite();

	if( !::MatrixInvert( *this ) ){
		//Calculation failed, set Null Matrix
		Release();
	}
	return *this;
}

ZRealMatrix& ZRealMatrix::SwapRow( int i, int j )
{
	ASSERT( i>=m_nRowBase && i<m_nRowBase+GetRow() );
	if( i!=j ){
		CopyBeforeWrite();

		REAL* pMx1 = GetMatrixAddr()+(i-m_nRowBase)*GetCol();
		REAL* pMx2 = GetMatrixAddr()+(j-m_nRowBase)*GetCol();
		for( int k=0; k<GetCol(); k++ ){
			Swap( *pMx1++, *pMx2++ );
		}
	}
	return *this;
}

ZRealMatrix& ZRealMatrix::SwapCol( int i, int j )
{
	ASSERT( i>=m_nColBase && i<m_nColBase+GetCol() );
	if( i!=j ){
		CopyBeforeWrite();

		REAL* pMx1 = GetMatrixAddr()+(i-m_nColBase);
		REAL* pMx2 = GetMatrixAddr()+(j-m_nColBase);
		for( int k=0; k<GetRow(); k++ ){
			Swap( *pMx1, *pMx2 );
			pMx1 += GetCol();
			pMx2 += GetCol();
		}
	}
	return *this;
}

ZRealMatrix& ZRealMatrix::Reshape( int nRow, int nCol )
{
	ASSERT( !IsNull() );
	ASSERT( nRow*nCol==GetSize() );
	CopyBeforeWrite();

	GetData()->nRow = nRow;
	GetData()->nCol = nCol;
	return *this;
}

//trying to make the matrix tempory. It's successful only when it's exculsively owning the buffer;
/*ZRealMatrix& ZRealMatrix::MakeTemp()
{
	if( GetData()->nRefs==1 ){
		GetData()->bTemp = TRUE;
	}
	return *this;
}*/

ZRealMatrix ZRealMatrix::GetTransposeMx() const
{
	if( IsNull() ){
		return *this;
	}
	
	int nRow = GetRow();
	int nCol = GetCol();
	
	ZRealMatrix temp( GetCol(), GetRow() );
	REAL* prMxData = GetMatrixAddr();
	REAL* prMxTemp = temp.GetMatrixAddr();
	for( int i=0; i<nRow; i++ ){
		for( int j=0; j<nCol; j++ ){
			prMxTemp[j*nRow+i] = prMxData[i*nCol+j];
		}
	}
	return temp;
}

ZRealMatrix ZRealMatrix::GetInvertMx() const
{
	ASSERT( GetRow() == GetCol() );
	
	ZRealMatrix temp( *this );
	
	temp.Invert();
	return temp;
}

ZRealMatrix ZRealMatrix::GetChildMx( int nRow, int nCol, int nRowLen, int nColLen ) const
{
	ASSERT( nRow>=m_nRowBase && nCol>=m_nColBase && nRow<m_nRowBase+GetRow() && nCol<m_nColBase+GetCol() );
	
	if( nRowLen==-1 ){
		nRowLen = m_nRowBase+GetRow()-nRow;
	}
	if( nColLen==-1 ){
		nColLen = m_nColBase+GetCol()-nCol;
	}
	
	ASSERT( nRow+nRowLen<=m_nRowBase+GetRow() && nCol+nColLen<=m_nColBase+GetCol() );
	
	ZRealMatrix temp( nRowLen, nColLen );
	REAL* prMxTemp = temp.GetMatrixAddr();
	REAL* prMxThis = GetMatrixAddr()+(nRow-m_nRowBase)*GetCol()+(nCol-m_nColBase);
	for( int i=0; i<nRowLen; i++ ){
		memcpy(prMxTemp+i*nColLen, prMxThis+i*GetCol(),sizeof(REAL)*nColLen);
	}
/*	for( int i=nRow; i<nRow+nRowLen; i++ ){
		for( int j=nCol; j<nCol+nColLen; j++ ){
			temp(i-nRow+1, j-nCol+1) = (*this)(i,j);
		}
	}*/
	
	return temp;
}

ZRealMatrix ZRealMatrix::GetRowMx( int nRow ) const
{
	ASSERT( !IsNull() );
	ASSERT( nRow>=m_nRowBase && nRow<m_nRowBase+GetRow() );

	ZRealMatrix temp(1, GetCol());

	REAL* prMxTemp = temp.GetMatrixAddr();
	REAL* prMxData = GetMatrixAddr()+(nRow-m_nRowBase)*GetCol();

	memcpy( prMxTemp, prMxData, sizeof(REAL)*GetCol() );

	return temp;
}

ZRealMatrix ZRealMatrix::GetColMx( int nCol ) const
{
	ASSERT( !IsNull() );
	ASSERT( nCol>=m_nColBase && nCol<m_nColBase+GetCol() );

	ZRealMatrix temp(GetRow(), 1);

	REAL* prMxTemp = temp.GetMatrixAddr();
	REAL* prMxData = GetMatrixAddr() + nCol - m_nColBase;

	for( int i=0; i<GetRow(); i++ ){
		prMxTemp[i] = *prMxData;
		prMxData += GetCol();
	}

	return temp;
}

void ZRealMatrix::GetRowMx( int nRow, ZRealMatrix& mxRow )
{
	ASSERT( mxRow.GetRow()==1 && mxRow.GetCol()==GetCol() );
	ASSERT( nRow>=m_nRowBase && nRow<m_nRowBase+GetRow() );

	mxRow.CopyBeforeWrite();

	REAL* prMxData = GetMatrixAddr()+(nRow-m_nRowBase)*GetCol();
	REAL* prMxRow = mxRow.GetMatrixAddr();

	memcpy( prMxData, prMxRow, sizeof(REAL)*GetCol() );
}

void ZRealMatrix::GetColMx( int nCol, ZRealMatrix& mxCol )
{
	ASSERT( mxCol.GetCol()==1 && mxCol.GetRow()==GetRow() );
	ASSERT( nCol>=m_nColBase && nCol<m_nColBase+GetCol() );

	mxCol.CopyBeforeWrite();

	REAL* prMxData = GetMatrixAddr()+(nCol-m_nColBase);
	REAL* prMxCol = mxCol.GetMatrixAddr();

	for( int i=0; i<GetRow(); i++ ){
		*prMxData = prMxCol[i];
		prMxData += GetCol();
	}
}

void ZRealMatrix::GetChildMx( int nRow, int nCol, ZRealMatrix& mxChd )
{
	ASSERT( nRow>=m_nRowBase && nCol>=m_nColBase && nRow<m_nRowBase+GetRow() && nCol<m_nColBase+GetCol() );
	
	int nRowLen = mxChd.GetRow();
	int nColLen = mxChd.GetCol();
	
	ASSERT( nRow+nRowLen<=m_nRowBase+GetRow() && nCol+nColLen<=m_nColBase+GetCol() );

	mxChd.CopyBeforeWrite();
	
	REAL* prMxChd = mxChd.GetMatrixAddr();
	REAL* prMxThis = GetMatrixAddr()+(nRow-m_nRowBase)*GetCol()+(nCol-m_nColBase);
	for( int i=0; i<nRowLen; i++ ){
		memcpy(prMxChd, prMxThis,sizeof(REAL)*nColLen);
	}
}

//get the diagonal vector, the return matrix is 1 column vector
ZRealMatrix ZRealMatrix::GetDiagMx() const
{
	int nDim = min( GetCol(), GetRow() );
	ZRealMatrix temp( nDim, 1 );
	REAL *prMxTemp = temp.GetMatrixAddr();
	REAL *prMxThis = GetMatrixAddr();
	for( int i=0; i<nDim; i++ ){
		prMxTemp[i] = *prMxThis;
		prMxThis += GetCol() + 1;
	}
	return temp;
}

//get the lower trianglar part of matrix below the k-th diagonal line. 
//k=0, the main diagonal; k<0, below the main diagonal; k>0, above the main diagonal
ZRealMatrix ZRealMatrix::GetTriLMx( int k ) const
{
	ASSERT( k>-GetRow() && k<GetCol() );

	ZRealMatrix temp( GetRow(), GetCol(), 0.0 );

	//considering zero index
	for( int i=max(-k,0); i<GetRow(); i++ ){
		int nLen = min(k+i+1, GetCol());
		REAL* prMxTemp = temp.GetMatrixAddr() + i * GetCol();
		REAL* prMxThis = GetMatrixAddr() + i*GetCol();
		memcpy( prMxTemp, prMxThis, sizeof(REAL)*nLen );
	}
	return temp;
}

//get the upper trianglar part of matrix below the k-th diagonal line. 
//k=0, the main diagonal; k<0, below the main diagonal; k>0, above the main diagonal
ZRealMatrix ZRealMatrix::GetTriUMx( int k ) const
{
	ASSERT( k>-GetRow() && k<GetCol() );

	ZRealMatrix temp( GetRow(), GetCol(), 0.0 );

	int nEnd = min( GetRow(), GetCol()-k );
	for( int i=0; i<nEnd; i++ ){
		int nLen = min( GetCol()-k-i, GetCol() );
		REAL* prMxTemp = temp.GetMatrixAddr() + i * GetCol() + max(0, i+k);
		REAL* prMxThis = GetMatrixAddr() + i*GetCol() + max(0, i+k);
		memcpy( prMxTemp, prMxThis, sizeof(REAL)*nLen );
	}
	return temp;
}

ZRealMatrix ZRealMatrix::GetReshapeMx( int nRow, int nCol ) const
{
	return ZRealMatrix(*this).Reshape(nRow, nCol);
}

ZRealMatrix ZRealMatrix::GetSumRowMx() const
{
	ZRealMatrix temp(1, GetCol(), 0.0);
	REAL *prMxTemp = temp.GetMatrixAddr();
	REAL *prMxThis = GetMatrixAddr();

	for( int i=0; i<GetRow(); i++ ){
		for( int j=0; j<GetCol(); j++ )prMxTemp[j] += prMxThis[j];
		prMxThis += GetCol();
	}
	return temp;
}

ZRealMatrix ZRealMatrix::GetSumColMx() const
{
	ZRealMatrix temp(GetRow(), 1, 0.0);
	REAL *prMxTemp = temp.GetMatrixAddr();
	REAL *prMxThis = GetMatrixAddr();

	for( int i=0; i<GetRow(); i++ ){
		for( int j=0; j<GetCol(); j++ )prMxTemp[i] += prMxThis[j];
		prMxThis += GetCol();
	}
	return temp;
}


//LU decomposition
void ZRealMatrix::GetLUMx( ZRealMatrix& mxL, ZRealMatrix& mxU ) const
{
	ASSERT( GetRow()==GetCol() );
	if( mxL.IsNull() ){
		mxL.Create( GetRow(), GetCol() );
	}
	if( mxU.IsNull() ){
		mxL.Create( GetRow(), GetCol() );
	}

	ASSERT( mxL.GetRow()==GetRow() && mxL.GetCol()==GetCol() );
	ASSERT( mxU.GetRow()==GetRow() && mxU.GetCol()==GetCol() );

	MatrixDecomLU( *this, mxL, mxU );
}

REAL ZRealMatrix::GetDetVal() const
{
	ASSERT( GetRow()==GetCol() );

	return MatrixDetVal( *this );
}

//-------------------------------------------------------------------------------------------------
ZRealMatrix ZRealMatrix::operator () ( int i ) const
{
	return GetRowMx(i);
}

ZRealMatrix ZRealMatrix::operator [] ( int i ) const
{
	return GetColMx(i);
}

REAL& ZRealMatrix::operator () ( int i, int j )
{
	CopyBeforeWrite();

	return *(GetMatrixAddr() + Index(i, j) );
}

REAL ZRealMatrix::operator() ( int i, int j ) const
{
	return *(GetMatrixAddr() + Index(i,j) );
}

void ZRealMatrix::SetAt( int i, int j, REAL rValue )
{
	CopyBeforeWrite();

	*(GetMatrixAddr() + Index(i,j)) = rValue;
}

REAL ZRealMatrix::GetAt( int i, int j ) const
{
	return *(GetMatrixAddr() + Index(i,j) );
}

ZRealMatrix::operator LPCREAL() const
{
	return GetBuffAddr();
}

//-------------------------------------------------------------------------------------------------
ZRealMatrix& ZRealMatrix::operator = ( const ZRealMatrix& mx )
{
	ASSERT( !mx.IsNull() );	//shouldn't copy a null matrix
	if( this!=&mx ){
		if( (!mx.IsNull() && mx.GetData()->nRefs<0) || GetData()->nRefs < 0 ){
			//either matrix is locked, do the real memory copy
			AssignCopy( mx.GetMatrixAddr(), mx.GetRow(), mx.GetCol() );
		}else{
			//all are free referenced, just pass the reference
			Release();
			m_pMatrixHead = mx.GetData();
			if( !mx.IsNull() )InternalAddRef();
		}
	}
	return *this;
}


//self matrix algebra operator
ZRealMatrix& ZRealMatrix::operator += ( const ZRealMatrix& mx )
{
	return Add( mx );
}

ZRealMatrix& ZRealMatrix::operator -= ( const ZRealMatrix& mx )
{
	return Sub( mx );
}

ZRealMatrix& ZRealMatrix::operator *= ( const ZRealMatrix& mx )
{
	return Mul( mx );
}

//solving linear equation x=B/A, self is B.
ZRealMatrix& ZRealMatrix::operator /= ( const ZRealMatrix& mxA )
{
	return Div( mxA );
}

ZRealMatrix& ZRealMatrix::operator ^= ( int n )
{
	return Power( n );
}

ZRealMatrix& ZRealMatrix::operator ^= ( REAL r )
{
	return Power( r );
}

ZRealMatrix& ZRealMatrix::Add( const ZRealMatrix& mx )
{
	ASSERT( GetRow()==mx.GetRow() && GetCol()==mx.GetCol() );

	CopyBeforeWrite();

	int nSize = GetSize();
	REAL* pMxData = GetMatrixAddr();
	REAL* pMxOther = mx.GetMatrixAddr();
	
	for( int i=0; i<nSize; i++ ){
		pMxData[i] += pMxOther[i];
	}
	return *this;
}

ZRealMatrix& ZRealMatrix::Sub( const ZRealMatrix& mx )
{
	ASSERT( GetRow()==mx.GetRow() && GetCol()==mx.GetCol() );

	CopyBeforeWrite();

	int nSize = GetSize();
	REAL* pMxData = GetMatrixAddr();
	REAL* pMxOther = mx.GetMatrixAddr();
	
	for( int i=0; i<nSize; i++ ){
		pMxData[i] -= pMxOther[i];
	}
	return *this;
}

ZRealMatrix& ZRealMatrix::Mul( const ZRealMatrix& mx )
{
	(*this) = ::Mul( *this, mx );
	return *this;
}

//solving linear equation x=B/A, self is B.
ZRealMatrix& ZRealMatrix::Div( const ZRealMatrix& mxA )
{
	if( !::MatrixSolve( mxA, *this, *this ) ){
		//calculation failed, set null
		Release();
	}
	return (*this);
}

ZRealMatrix& ZRealMatrix::Power( int n )
{
	Power( *this, n, *this );
	return *this;
}

ZRealMatrix& ZRealMatrix::Power( REAL r )
{
	ASSERT( FALSE );
	return *this;
}

//self matrix element operator
ZRealMatrix& ZRealMatrix::operator += ( REAL r )
{
	return ElemAdd( r );
}

ZRealMatrix& ZRealMatrix::operator -= ( REAL r )
{
	return ElemSub( r );
}

ZRealMatrix& ZRealMatrix::operator *= ( REAL r )
{
	return ElemMul( r );
}

ZRealMatrix& ZRealMatrix::operator /= ( REAL r )
{
	return ElemDiv( r );
}

ZRealMatrix& ZRealMatrix::ElemAdd( REAL r )
{
	ASSERT( !IsNull() );
	CopyBeforeWrite();

	int nSize = GetSize();
	REAL* prMxThis = GetMatrixAddr();
	for( int i=0; i<nSize; i++ ){
		prMxThis[i] += r;
	}
	return *this;
}

ZRealMatrix& ZRealMatrix::ElemSub( REAL r )
{
	return ElemAdd( -r );
}

ZRealMatrix& ZRealMatrix::ElemMul( REAL r )
{
	ASSERT( !IsNull() );
	CopyBeforeWrite();

	int nSize = GetSize();
	REAL* prMxThis = GetMatrixAddr();
	for( int i=0; i<nSize; i++ ){
		prMxThis[i] *= r;
	}
	return *this;
}

ZRealMatrix& ZRealMatrix::ElemDiv( REAL r )
{
	return ElemMul( 1.0/r );
}

ZRealMatrix& ZRealMatrix::ElemPower( REAL r )
{
	ASSERT( !IsNull() );
	if( IsNull() )return *this;
	CopyBeforeWrite();

	int nSize = GetSize();
	REAL* prMxThis = GetMatrixAddr();
	for( int i=0; i<nSize; i++ ){
		prMxThis[i] = pow( prMxThis[i], r );
	}
	return *this;
}

ZRealMatrix& ZRealMatrix::ElemAdd( const ZRealMatrix& mx )
{
	return Add( mx );
}

ZRealMatrix& ZRealMatrix::ElemSub( const ZRealMatrix& mx )
{
	return Sub( mx );
}

ZRealMatrix& ZRealMatrix::ElemMul( const ZRealMatrix& mx )
{
	ASSERT( GetRow()==mx.GetRow() && GetCol()==mx.GetCol() );

	CopyBeforeWrite();

	int nSize = GetSize();
	REAL* pMxData = GetMatrixAddr();
	REAL* pMxOther = mx.GetMatrixAddr();
	
	for( int i=0; i<nSize; i++ ){
		pMxData[i] *= pMxOther[i];
	}
	return *this;
}

ZRealMatrix& ZRealMatrix::ElemDiv( const ZRealMatrix& mx )
{
	ASSERT( GetRow()==mx.GetRow() && GetCol()==mx.GetCol() );

	CopyBeforeWrite();

	int nSize = GetSize();
	REAL* pMxData = GetMatrixAddr();
	REAL* pMxOther = mx.GetMatrixAddr();
	
	for( int i=0; i<nSize; i++ ){
		pMxData[i] /= pMxOther[i];
	}
	return *this;
}

ZRealMatrix& ZRealMatrix::ElemExp()
{
	CopyBeforeWrite();

	int nSize = GetSize();
	REAL* pMxData = GetMatrixAddr();
	
	for( int i=0; i<nSize; i++ ){
		pMxData[i] = exp( pMxData[i] );
	}
	return *this;
}

ZRealMatrix& ZRealMatrix::ElemLog()
{
	CopyBeforeWrite();

	int nSize = GetSize();
	REAL* pMxData = GetMatrixAddr();

	for( int i=0; i<nSize; i++ ){
		pMxData[i] = log( pMxData[i] );
	}
	return *this;
}

REAL ZRealMatrix::DotMul( const ZRealMatrix& mx )
{
	REAL rDotSum = 0;
	int nSize = GetSize();
	REAL* pMxData = GetMatrixAddr();
	REAL* pMxOther = mx.GetMatrixAddr();
	for( int i=0; i<nSize; i++ ){
		rDotSum += pMxData[i]*pMxOther[i];
	}
	return rDotSum;
}

ZRealMatrix& ZRealMatrix::Neg()
{
	if( IsNull() )return *this;

	CopyBeforeWrite();

	int nSize = GetSize();
	REAL* pMxData = GetMatrixAddr();
	for( int i=0; i<nSize; i++ ){
		pMxData[i] = -pMxData[i];
	}
	return *this;
}

ZRealMatrix ZRealMatrix::operator - ()
{
	if( IsNull() )return *this;

	return ZRealMatrix(*this).Neg();
}

/*------------------------------------------------------------------------*/
/*                                                                        */
/*  ZRealMatrix friend function and global operator                       */
/*                                                                        */
/*------------------------------------------------------------------------*/
//matrix operator
ZRealMatrix operator + ( const ZRealMatrix& mx1, const ZRealMatrix& mx2 )
{
	return Add( mx1, mx2 );
}

ZRealMatrix operator - ( const ZRealMatrix& mx1, const ZRealMatrix& mx2 )
{
	return Sub(mx1, mx2);
}

ZRealMatrix operator * ( const ZRealMatrix& mx1, const ZRealMatrix& mx2 )
{
	return Mul( mx1, mx2 );
}

ZRealMatrix operator / ( const ZRealMatrix& mx1, const ZRealMatrix& mx2 )
{
	return Div( mx1, mx2 );
}

ZRealMatrix operator ^ ( const ZRealMatrix& mx, int n )
{
	return Power( mx, n );
}

ZRealMatrix operator ^ ( const ZRealMatrix& mx, REAL r )
{
	return Power( mx, r );
}

ZRealMatrix Add( const ZRealMatrix& mx1, const ZRealMatrix& mx2 )
{
	return ZRealMatrix(mx1).Add(mx2);
}

ZRealMatrix Sub( const ZRealMatrix& mx1, const ZRealMatrix& mx2 )
{
	return ZRealMatrix(mx1).Sub(mx2);
}

ZRealMatrix Mul( const ZRealMatrix& mx1, const ZRealMatrix& mx2 )
{
	ASSERT( mx1.GetCol()==mx2.GetRow() );

	ZRealMatrix temp( mx1.GetRow(), mx2.GetCol() );
	//to reduce unnecessary memory allocation and deallocation
	ZRealMatrix::Mul( mx1, mx2, temp );

	return temp;
}

//solving linear equation x=B/A, mx1:B, mx2:A.
ZRealMatrix Div( const ZRealMatrix& mx1, const ZRealMatrix& mx2 )
{
	return ZRealMatrix(mx1).Div(mx2);
}

ZRealMatrix Power( const ZRealMatrix& mx, int n )
{
	return ZRealMatrix(mx).Power( n );
}

ZRealMatrix Power( const ZRealMatrix& mx, REAL r )
{
	return ZRealMatrix(mx).Power( r );
}

//matrix element operator
ZRealMatrix operator + ( const ZRealMatrix& mx, REAL r )
{
	return ElemAdd(mx, r);
}

ZRealMatrix operator - ( const ZRealMatrix& mx, REAL r )
{
	return ElemSub(mx, r);
}

ZRealMatrix operator * ( const ZRealMatrix& mx, REAL r )
{
	return ElemMul(mx, r);
}

ZRealMatrix operator * ( REAL r, const ZRealMatrix& mx )
{
	return ElemMul(mx, r);
}

ZRealMatrix operator / ( const ZRealMatrix& mx, REAL r )
{
	return ElemDiv(mx, r);
}

ZRealMatrix ElemAdd( const ZRealMatrix& mx, REAL r )
{
	return ZRealMatrix(mx).ElemAdd(r);
}

ZRealMatrix ElemSub( const ZRealMatrix& mx, REAL r )
{
	return ZRealMatrix(mx).ElemSub(r);
}

ZRealMatrix ElemMul( const ZRealMatrix& mx, REAL r )
{
	return ZRealMatrix(mx).ElemMul(r);
}

ZRealMatrix ElemDiv( const ZRealMatrix& mx, REAL r )
{
	return ZRealMatrix(mx).ElemDiv(r);
}

ZRealMatrix ElemPower( const ZRealMatrix& mx, REAL r )
{
	return ZRealMatrix(mx).ElemPower(r);
}

ZRealMatrix ElemAdd( const ZRealMatrix& mx1, const ZRealMatrix& mx2 )
{
	return ZRealMatrix(mx1).ElemAdd( mx2 );
}

ZRealMatrix ElemSub( const ZRealMatrix& mx1, const ZRealMatrix& mx2 )
{
	return ZRealMatrix(mx1).ElemSub( mx2 );
}

ZRealMatrix ElemMul( const ZRealMatrix& mx1, const ZRealMatrix& mx2 )
{
	return ZRealMatrix(mx1).ElemMul( mx2 );
}

ZRealMatrix ElemDiv( const ZRealMatrix& mx1, const ZRealMatrix& mx2 )
{
	return ZRealMatrix(mx1).ElemDiv( mx2 );
}

ZRealMatrix ElemLog( const ZRealMatrix& mx )
{
	ZRealMatrix mxTmp(mx);
	mxTmp.CopyBeforeWrite();

	REAL* prMxTmp = mxTmp.GetMatrixAddr();
	REAL* prMx = mx.GetMatrixAddr();

	for( int i=0; i<mx.GetSize(); i++ ){
		prMxTmp[i] = log( prMx[i] );
	}
	return mxTmp;
}

ZRealMatrix ElemExp( const ZRealMatrix& mx )
{
	ZRealMatrix mxTmp(mx);
	mxTmp.CopyBeforeWrite();

	REAL* prMxTmp = mxTmp.GetMatrixAddr();
	REAL* prMx = mx.GetMatrixAddr();

	for( int i=0; i<mx.GetSize(); i++ ){
		prMxTmp[i] = exp( prMx[i] );
	}
	return mxTmp;
}

