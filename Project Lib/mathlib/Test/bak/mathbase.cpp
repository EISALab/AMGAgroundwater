#include <memory.h>
#include "mathbase.h"
#include "mathfunc.h"

#ifdef _MFC_VER
	#ifdef _DEBUG
		#define new DEBUG_NEW
	#endif
#endif

#ifdef _DEBUG
//#define new DEBUG_NEW
#endif

MATH_STATIC_DATA ZMatrixHead _mxInitHead = { 0, 0, 0, 0, 0, 0 };
MATH_STATIC_DATA ZMatrixHead* _pmxHeadNil = &_mxInitHead;

#define MATRIX_HEAD_DATABUF(pmxHead)	(REAL*)((BYTE*)(pmxHead)+sizeof(ZMatrixHead))

/*------------------------------------------------------------------------*/
/*                                                                        */
/*  ZRealMatrix member function                                               */
/*                                                                        */
/*------------------------------------------------------------------------*/

/*----------------------------------------------------------------------------------
			protected member function of ZRealMatrix
----------------------------------------------------------------------------------*/
void ZRealMatrix::Init()
{
	m_pMatrixHead = _pmxHeadNil;
	m_nRowBase = m_nColBase = 1;
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

	m_pMatrixHead = (ZMatrixHead*)new BYTE[sizeof(ZMatrixHead)+sizeof(REAL)*nRow*nCol];
	m_pMatrixHead->nAllocLen = nRow*nCol;
	m_pMatrixHead->nRow = nRow;
	m_pMatrixHead->nCol = nCol;
	m_pMatrixHead->nRefs = 0;
	m_pMatrixHead->pmxLocker = this;
	m_pMatrixHead->pMatrixData = MATRIX_HEAD_DATABUF(m_pMatrixHead);

	InternalAddRef();

//	printf("Alloc buffer %d-%d\n", nRow, nCol );
}

void ZRealMatrix::Release()
{
	ASSERT( GetData()!=NULL );

	if( !IsNull() ){
		if( InternalRelease()==0 ){
			//the reference is zero, locked by itself or not, buffer must be freeed
			ASSERT( GetLocker()==NULL || GetLocker()==this );
			REAL* pMatrixData = GetData()->pMatrixData;
			if( NULL!=pMatrixData && pMatrixData!=MATRIX_HEAD_DATABUF(GetData()) ){
				delete[] pMatrixData;
			}
//			printf("Releasing buffer %d-%d\n", GetRow(), GetCol() );
			delete[] m_pMatrixHead;
		}else{
			//release the lock if it's the locker, since the owner matrix will be destroyed,
			//the other matrix holding it will release the buffer later.
			if( GetLocker()==this ){
				UnlockBuffer();
			}
		}
		Init();
	}
/*	if( !IsNull() ){
		if( GetLocker()==this ){
			//the matrix data is locked by itself, so free the buffer
			REAL* pMatrixData = GetData()->pMatrixData;
			if( NULL!=pMatrixData && pMatrixData!=MATRIX_HEAD_DATABUF(GetData()) ){
				delete[] pMatrixData;
			}
			printf("Releasing buffer %d-%d\n", GetRow(), GetCol() );
			delete[] m_pMatrixHead;
		}
		Init();
	}
*/
}

void ZRealMatrix::AllocBeforeCopy(int nRow, int nCol)
{
	int nSize = nRow*nCol;
	if( (GetData()->nRefs==1) &&
		(GetData()->nAllocLen>=nSize || GetSize()>=nSize) ){
		//The copying buffer is exclusively owned by it
		//and the holding buffer is long enough, 
		//so do not bother reallocating buffer again
		GetData()->nRow = nRow;
		GetData()->nCol = nCol;
	}else{
		//the buffer is referenced by other matrix, and must alloc a new buffer the copy
		//the matrix data
		Release();
		AllocBuffer( nRow, nCol );
	}
	ASSERT(GetData()->nRefs <= 1);
}

void ZRealMatrix::CopyBeforeWrite()
{
	if (GetData()->nRefs > 1)
	{
		ZRealMatrix* pmxLocker = GetLocker();
		ZMatrixHead* pmxHead = GetData();
		Release();
		AllocBuffer(pmxHead->nRow, pmxHead->nCol);
		memcpy( GetMatrixAddr(), pmxHead->pMatrixData, GetSize()*sizeof(REAL) );
		if( pmxLocker!=this ){
			ASSERT( pmxLocker==NULL );
			UnlockBuffer();
		}
	}
	ASSERT(IsNull() || GetData()->nRefs == 1);
}

void ZRealMatrix::AssignCopy( const ZRealMatrix& mx, BOOL bLockBuffer )
{
/*	if( mx.IsNull() ){
		Release();
		return;
	}

	if( mx.GetLocker()!=NULL && !IsNull() ){
		if( GetData()->nDataLen>=mx.GetSize() || GetSize()>=mx.GetSize() ){
			memcpy( GetMatrixAddr(), mx.GetMatrixAddr(), mx.GetSize()*sizeof(REAL) );
			GetData()->nRow = mx.GetRow();
			GetData()->nCol = mx.GetCol();
			m_nRowBase = mx.m_nRowBase;
			m_nColBase = mx.m_nColBase;
			return;
		}
	}

	if( IsNull() ){
		Release();
	}

	ASSERT( IsNull() );

	if( mx.GetLocker()==NULL ){
		m_pMatrixHead = mx.GetData();
		m_pMatrixHead->pmxLocker = this;
	}else{
		Create( mx.GetRow(), mx.GetCol(), mx.GetMatrixAddr() );
	}*/

	AssignCopy( mx.GetData(), bLockBuffer );
	m_nRowBase = mx.m_nRowBase;
	m_nColBase = mx.m_nColBase;
}

void ZRealMatrix::AssignCopy( ZMatrixHead* pmxHead, BOOL bLockBuffer )
{
	if( GetData()==pmxHead || pmxHead->pmxLocker==this )return;

	if( pmxHead==_pmxHeadNil ){
		Release();
		return;
	}

	if( pmxHead->pmxLocker==NULL ){
		//the buffer is not locked
		Release();
		m_pMatrixHead = pmxHead;
		InternalAddRef();
		if( bLockBuffer ){
			m_pMatrixHead->pmxLocker = this;
		}
		return;
	}
	
	//decide if I need alloc a new buffer
	AllocBeforeCopy( pmxHead->nRow, pmxHead->nCol );
	memcpy( GetMatrixAddr(), pmxHead->pMatrixData, GetSize()*sizeof(REAL) );

	if( bLockBuffer && GetLocker()!=this ){
		ASSERT( GetLocker()==NULL );
		LockBuffer();
	}else if( !bLockBuffer && GetLocker()!=NULL ){
		ASSERT( GetLocker()==this );
		UnlockBuffer();
	}
	return;

/*	int nSize = pmxHead->nRow*pmxHead->nCol;
	//pmxHead is not null, and GetData()!=null
	if( pmxHead->pmxLocker!=NULL && 
		(GetData()->nDataLen>=nSize || GetSize()>=nSize) ){
		//The copying buffer is locked by other matrix, but
		//the holding buffer is long enough, do not bother reallocating buffer again
		memcpy( GetMatrixAddr(), pmxHead->pMatrixData, nSize*sizeof(REAL) );
		GetData()->nRow = pmxHead->nRow;
		GetData()->nCol = pmxHead->nCol;
		return;
	}

	//must reallocate buffer or just lock the new buffer
	Release();
	ASSERT( IsNull() );

	if( pmxHead->pmxLocker==NULL ){
		//the buffer is not locked
		m_pMatrixHead = pmxHead;
		InternalAddRef();
		if( bLockBuffer ){
			m_pMatrixHead->pmxLocker = this;
		}
	}else{
		AllocBuffer( pmxHead->nRow, pmxHead->nCol );
		memcpy( GetMatrixAddr(), pmxHead->pMatrixData, nSize*sizeof(REAL) );
	}
*/
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


ZRealMatrix& ZRealMatrix::Mul( REAL f )
{
	if( IsNull() )return *this;

	CopyBeforeWrite();

	int nSize = GetSize();
	REAL* pMxData = GetMatrixAddr();
	for( int i=0; i<nSize; i++ ){
		pMxData[i] *= f;
	}
	return *this;
}

ZRealMatrix& ZRealMatrix::Div( REAL f )
{
	if( IsNull() )return *this;

	CopyBeforeWrite();

	int nSize = GetSize();
	REAL* pMxData = GetMatrixAddr();
	for( int i=0; i<nSize; i++ ){
		pMxData[i] /= f;
	}
	return *this;
}

ZRealMatrix& ZRealMatrix::Mul( const ZRealMatrix& mx )
{
	//Don't need CopyBeforeWrite because AssignCopy has already taken care of reference write
	AssignCopy( Mul( *(this), mx ), TRUE );
	return *this;
}

ZRealMatrix ZRealMatrix::Mul( const ZRealMatrix& mx1, const ZRealMatrix& mx2 )
{
	ASSERT( mx1.GetCol()==mx2.GetRow() );

	ZRealMatrix temp( mx1.GetRow(), mx2.GetCol() );
	Mul( mx1, mx2, temp );
	temp.UnlockBuffer();
	return temp;

/*	ZRealMatrix temp( mx1.GetRow(), mx2.GetCol(), 0.0 );

	REAL* prMxData1 = mx1.GetMatrixAddr();
	REAL* prMxData2 = mx2.GetMatrixAddr();
	REAL* pxMxTemp = temp.GetMatrixAddr();

	for( int i=0; i<mx1.GetRow(); i++ ){
		for( int j=1; j<mx2.GetRow(); j++ ){
			for( int k=0; k<temp.GetCol(); k++ ){
				pxMxTemp[i*temp.GetCol()+j] += prMxData1[i*mx1.GetCol()+k]*prMxData2[k*mx2.GetCol()+j];
			}
		}
	}
	temp.UnlockBuffer();
	return temp;*/
}

void ZRealMatrix::Mul( const ZRealMatrix& mx1, const ZRealMatrix& mx2, ZRealMatrix& mxRet )
{
	ASSERT( mxRet.GetRow()==mx1.GetRow() && mxRet.GetCol()==mx2.GetCol() );

	mxRet.SetUniform( 0.0 );
	REAL* prMxData1 = mx1.GetMatrixAddr();
	REAL* prMxData2 = mx2.GetMatrixAddr();
	REAL* pxMxRet = mxRet.GetMatrixAddr();

	for( int i=0; i<mx1.GetRow(); i++ ){
		for( int j=0; j<mx2.GetCol(); j++ ){
			for( int k=0; k<mx1.GetCol(); k++ ){
				pxMxRet[i*mxRet.GetCol()+j] += prMxData1[i*mx1.GetCol()+k]*prMxData2[k*mx2.GetCol()+j];
			}
		}
	}
}

ZRealMatrix& ZRealMatrix::Pow( int k )
{
	AssignCopy( Pow(*this, k), TRUE );
	return *this;
}

void ZRealMatrix::Pow( const ZRealMatrix& mx, int k, ZRealMatrix& mxRet )
{
	ASSERT( mx.GetRow()==mx.GetCol() );
	if( mx.IsNull() || k==1 ){
		mxRet = mx;
		return;
	}

	if( k==0 ){
		mxRet = mx;
		mxRet.SetIdentity();
		return;
	}
	mxRet = PosPow( mx, abs(k) );
	if( k<0 ){
		mxRet.Invert();
	}
	return;
}

ZRealMatrix ZRealMatrix::Pow( const ZRealMatrix& mx, int k )
{
	ZRealMatrix temp;
	Pow( mx, k, temp );
	temp.UnlockBuffer();
	return temp;
}


ZRealMatrix ZRealMatrix::PosPow( const ZRealMatrix& mx, int k )
{
	ASSERT( k>=1 );
	
	if( k==1 )return mx;

	if( k%2==0 ){
		ZRealMatrix temp;
		temp = PosPow(mx, k/2 );
		return Mul(temp, temp);
	}else{
		ZRealMatrix temp;
		temp = PosPow(mx, (k-1)/2);
		Mul( Mul(temp, temp), mx, temp );
		temp.UnlockBuffer();
		return temp;
	}
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
}

ZRealMatrix::ZRealMatrix( int m, int n )
{
	Init();
	Create( m, n );
}

ZRealMatrix::ZRealMatrix( int m, int n, REAL rElement )
{
	Init();
	Create( m, n, rElement );
}

ZRealMatrix::ZRealMatrix( int m, int n, const REAL* prData )
{
	Init();
	Create( m, n, prData );
}

ZRealMatrix::ZRealMatrix( int m, int n, const REAL** pprData )
{
	Init();
	Create( m, n, pprData ); 
}

ZRealMatrix::ZRealMatrix( const ZRealMatrix& mxInit )
{
	Init();
	//here I can't lock the copied buffer, this is a trick to prevent unnecessary 
	//buffer copy.
	AssignCopy( mxInit, FALSE );
//	Create( mxInit );
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

	AssignCopy( mxInit, TRUE );
}

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

	for( int i=0; i<nDim; i++ ){
		pMxData[i*GetCol()+i] = 1.0;
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

	AllocBuffer( 0, 0 );
	GetData()->nRow = nRow;
	GetData()->nCol = nCol;

	GetData()->pMatrixData = pData;
}

REAL* ZRealMatrix::Detach()
{
	ASSERT( !IsNull() );
	ASSERT( GetData()->pmxLocker==this );
	ASSERT( MATRIX_HEAD_DATABUF(GetData()) != GetData()->pMatrixData );

	REAL* prMxData = GetMatrixAddr();

	GetData()->pMatrixData = NULL;
	Release();

	return prMxData;
}

void ZRealMatrix::UnlockBuffer()
{
	if( IsNull() ){
		return;
	}

	ASSERT( GetLocker()==this );

	GetData()->pmxLocker = NULL;
}

void ZRealMatrix::LockBuffer()
{
	if( IsNull() || GetLocker()==this ){
		return;
	}

	ASSERT( GetLocker()==NULL );

	GetData()->pmxLocker = this;
}

LPREALMATRIX ZRealMatrix::GetLocker() const
{
	return GetData()->pmxLocker;
}

//-------------------------------------------------------------------------------------------------
ZRealMatrix& ZRealMatrix::Verse()
{
	if( IsNull() )return *this;

	if( GetRow()==1 || GetCol()==1 ){
		CopyBeforeWrite();
		Exchange( GetData()->nRow, GetData()->nCol );
		return *this;
	}

	//AssignCopy will take care of reference write
	AssignCopy( GetVerseMx(), TRUE );
	return (*this);
}

ZRealMatrix& ZRealMatrix::Invert()
{
	ASSERT( GetRow() == GetCol() );

	CopyBeforeWrite();

	if( !::MatInvert( GetMatrixAddr(), GetRow() ) ){
		//Set Null Matrix
		Release();
	}
	return *this;
}

ZRealMatrix ZRealMatrix::GetVerseMx() const
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
	temp.UnlockBuffer();
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
	for( int i=nRow; i<nRow+nRowLen; i++ ){
		for( int j=nCol; j<nCol+nColLen; j++ ){
			temp(i-nRow+1, j-nCol+1) = (*this)(i,j);
		}
	}
	
	temp.UnlockBuffer();
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

	temp.UnlockBuffer();
	return temp;
}

ZRealMatrix ZRealMatrix::GetColMx( int nCol ) const
{
	ASSERT( !IsNull() );
	ASSERT( nCol<=GetCol() );

	ZRealMatrix temp(1, GetCol());

	REAL* prMxTemp = temp.GetMatrixAddr();
	REAL* prMxData = GetMatrixAddr() + nCol - m_nColBase;

	for( int i=0; i<GetRow(); i++ ){
		prMxTemp[i] = *prMxData;
		prMxData += GetRow();
	}

	temp.UnlockBuffer();
	return temp;
}

void ZRealMatrix::SetRowMx( const ZRealMatrix& mxRow, int nRow )
{
	ASSERT( mxRow.GetRow()==1 && mxRow.GetCol()==GetCol() );
	ASSERT( nRow>=m_nRowBase && nRow<m_nRowBase+GetRow() );

	CopyBeforeWrite();

	REAL* prMxData = GetMatrixAddr()+(nRow-m_nRowBase)*GetCol();
	REAL* prMxRow = mxRow.GetMatrixAddr();

	memcpy( prMxData, prMxRow, sizeof(REAL)*GetCol() );
}

void ZRealMatrix::SetColMx( const ZRealMatrix& mxCol, int nCol )
{
	ASSERT( mxCol.GetCol()==1 && mxCol.GetRow()==GetRow() );
	ASSERT( nCol>=m_nColBase && nCol<m_nColBase+GetCol() );

	CopyBeforeWrite();

	REAL* prMxData = GetMatrixAddr()+(nCol-m_nColBase);
	REAL* prMxCol = mxCol.GetMatrixAddr();

	for( int i=0; i<GetRow(); i++ ){
		*prMxData = prMxCol[i];
		prMxData += GetRow();
	}
}

void ZRealMatrix::SetChildMx( const ZRealMatrix& mxChd, int nRow, int nCol )
{
	ASSERT(FALSE);
}


ZRealMatrix& ZRealMatrix::Power( int k )
{
	return Pow(k);
}

ZRealMatrix& ZRealMatrix::ElemPower( REAL t )
{
	if( IsNull() )return (*this);

	CopyBeforeWrite();

	int nSize = GetSize();
	REAL* pMxData = GetMatrixAddr();
	for( int i=0; i<nSize; i++ ){
		pMxData[i] = pow( pMxData[i], t );
	}
	return (*this);
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

//-------------------------------------------------------------------------------------------------
ZRealMatrix& ZRealMatrix::operator = ( const ZRealMatrix& mx )
{
//	CopyBeforeWrite();
	AssignCopy( mx, TRUE );
	return *this;
}

ZRealMatrix ZRealMatrix::operator - ()
{
	if( IsNull() )return *this;

	return ZRealMatrix(*this).Neg();
/*	ZRealMatrix temp;
	temp.Create(GetRow(), GetCol());

	REAL* prMxData = GetMatrixAddr();
	REAL* prMxTemp = temp.GetMatrixAddr();

	int nSize = GetSize();
	for( int i=0; i<nSize; i++ ){
		prMxTemp[i] = -prMxData[i];
	}
	temp.UnlockBuffer();
	return temp;*/
}

ZRealMatrix& ZRealMatrix::operator += ( const ZRealMatrix& mx )
{
	return Add( mx );
}

ZRealMatrix& ZRealMatrix::operator -= ( const ZRealMatrix& mx )
{
	return Sub( mx );
}

ZRealMatrix& ZRealMatrix::operator *= ( REAL f )
{
	return Mul( f );
}

ZRealMatrix& ZRealMatrix::operator /= ( REAL f )
{
	return Div( f );
}

ZRealMatrix& ZRealMatrix::operator ^= ( int k )
{
	Pow( k );
	return *this;
}

	
/*------------------------------------------------------------------------*/
/*                                                                        */
/*  ZRealMatrix protected member function                                     */
/*                                                                        */
/*------------------------------------------------------------------------*/


/*------------------------------------------------------------------------*/
/*                                                                        */
/*  ZRealMatrix friend function                                               */
/*                                                                        */
/*------------------------------------------------------------------------*/

ZRealMatrix operator + ( const ZRealMatrix& mx1, const ZRealMatrix& mx2 )
{
	return ZRealMatrix(mx1).Add(mx2);
/*	ZRealMatrix temp;
	temp.Create( mx1 );

	temp += mx2;
	temp.UnlockBuffer();
	return temp;*/
}


ZRealMatrix operator - ( const ZRealMatrix& mx1, const ZRealMatrix& mx2 )
{
	return ZRealMatrix(mx1).Sub(mx2);

/*	ZRealMatrix temp;
	temp.Create( mx1 );

	temp -= mx2;
	temp.UnlockBuffer();
	return temp;*/
}


ZRealMatrix operator * ( const ZRealMatrix& mx1, const ZRealMatrix& mx2 )
{
	return ZRealMatrix::Mul( mx1, mx2 );

/*	ASSERT( mx1.GetCol()==mx2.GetRow() );

	ZRealMatrix temp( mx1.GetRow(), mx2.GetCol(), 0.0 );

	REAL* prMxData1 = mx1.GetMatrixAddr();
	REAL* prMxData2 = mx2.GetMatrixAddr();
	REAL* pxMxTemp = temp.GetMatrixAddr();

	for( int i=0; i<mx1.GetRow(); i++ ){
		for( int j=0; j<mx2.GetCol(); j++ ){
			for( int k=0; k<mx1.GetCol(); k++ ){
				pxMxTemp[i*temp.GetCol()+j] += prMxData1[i*mx1.GetCol()+k]*prMxData2[k*mx2.GetCol()+j];
			}
		}
	}
	temp.UnlockBuffer();
	return temp;*/
}


ZRealMatrix operator * ( REAL f, const ZRealMatrix& mx )
{
	return ZRealMatrix( mx ).Mul(f);
}

ZRealMatrix operator * ( const ZRealMatrix& mx, REAL f )
{
	return ZRealMatrix( mx ).Mul(f);
}

ZRealMatrix operator / ( const ZRealMatrix& mx, REAL f )
{
	return ZRealMatrix( mx ).Div(f);
}

ZRealMatrix operator ^ ( const ZRealMatrix& mx, int k )
{
	return ZRealMatrix::Pow(mx, k);
}