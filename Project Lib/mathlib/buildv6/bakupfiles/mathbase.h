#ifndef __MATHBASE_H__
#define __MATHBASE_H__
/****************************************************************************
	Published version of ZRealMatrix 1.0
	Copyright Shengquan Yan
	The source code is not allowed to modification without the permission of the author.
	email: smyan@uiuc.edu sq_yan@hotmail.com

	linear algebra algorithm for matrix interface is in matrixln.h

	One pitfall of ZRealMatrix is the unnecessaried memory allocation and deallocation problem when doing a concatenated algebra
	for example: ((mx*0.5) + 0.5) / 2;
	for each returned tempory matrix, the complier call copy construction for a new tempory matrix on stack,
	then destruct the returned tempory matrix and pass the new tempory matrix to next calling.
	the next calling doesn't know it's a tempory matrix and still implements the mempory protection policy, which is unnecessary.

	unfinished interface of ZRealMatrix:
		1. SetRandom(...);
		2. Power( REAL r );
	need refrinement :
		1. Power( int n );
		2. solving linear equation x=B/A, self is B.
			Div( const ZRealMatrix& mxA );
		3. GetDetVal();
*****************************************************************************/
#include <math.h>

#ifndef ASSERT
	#if     _MSC_VER > 1000
		#include <crtdbg.h>
		#define ASSERT _ASSERT
	#else
		#include <assert.h>
		#define ASSERT assert
	#endif
#endif

//----------------------------lib automaic include, for visual c++ only-------------------------
#ifdef _MSC_VER
	#ifdef _DLL
		#ifdef _DEBUG
			#pragma comment(lib, "MathSubDTD.lib")
		#else
			#pragma comment(lib, "MathSubDT.lib")
		#endif
	#else
		#ifdef _MT
			#ifdef _DEBUG
				#pragma comment(lib, "MathSubMTD.lib")
			#else
				#pragma comment(lib, "MathSubMT.lib")
			#endif	
		#else
			#ifdef _DEBUG
				#pragma comment(lib, "MathSubSTD.lib")
			#else
				#pragma comment(lib, "MathSubST.lib")
			#endif	
		#endif
	#endif
#endif


#define _MATHBASE_INLINE inline
#define MATH_STATIC_DATA static
/*---------------------------------------------------------------------------------------
									type definition
----------------------------------------------------------------------------------------*/
typedef double REAL;
typedef REAL* LPREAL;
typedef const REAL* LPCREAL;
typedef unsigned char BYTE;
typedef int BOOL;

//define data type
#ifndef TRUE
#define TRUE 1
#endif

#ifndef FALSE
#define FALSE 0
#endif

#ifndef NULL
#define NULL 0
#endif

/*---------------------------------------------------------------------------------------
									const definition
----------------------------------------------------------------------------------------*/
//*************define PI constant number
#ifndef M_PI
#define M_PI        3.14159265358979323846
#endif

#ifndef M_PI_2
#define M_PI_2      1.57079632679489661923
#endif

#ifndef M_PI_4
#define M_PI_4      0.785398163397448309616
#endif

#ifndef M_1_PI
#define M_1_PI      0.318309886183790671538
#endif

#ifndef M_2_PI
#define M_2_PI      0.636619772367581343076
#endif

#ifndef M_2_SQRTPI
#define M_2_SQRTPI	1.12837916709551257390
#endif

#ifndef M_2PI
#define M_2PI		(2*M_PI)
#endif

#ifndef PI
#define PI			M_PI
#endif


//*************define EPS constant number
#undef EPS
#undef EPS_COS
#define EPS			1e-10
#define EPS_COS     0.999999

/*---------------------------------------------------------------------------------------
									helper macros
----------------------------------------------------------------------------------------*/
#define ELEMENTS(array)		(sizeof(array)/sizeof((array)[0]))	//elements number of an arrray

//*************define min, max, MIN, MAX
#ifndef min
#define min math_min
#endif
#ifndef max
#define max math_max
#endif
#ifndef MIN
#define MIN math_min
#endif
#ifndef MAX
#define MAX math_max
#endif
#ifndef Swap
#define Swap math_swap
#endif
//*************define SQR and sqr
#undef SQR
#undef sqr

#define SQR(x)				((x)*(x))
#define sqr					SQR

//*************define macro to convert between radius and degree
#undef D_TO_R
#undef R_TO_D
#undef D2R
#undef R2D

#define	D_TO_R(x)			((x)/180.0*M_PI)					//from degree to radian
#define R_TO_D(x)			((x)*180.0/M_PI)					//from radian to degree
#define D2R					D_TO_R
#define R2D					R_TO_D

/*---------------------------------------------------------------------------------------
									helper inline function
----------------------------------------------------------------------------------------*/
//swap function
template<class T>
void math_swap( T& x, T& y )
{ T t = x; x = y; y = t; }

template<class  T>
const T& math_max(const T& x, const T& y)
{ return x>y ? x : y; }

template<class T>
const T& math_min(const T& x, const T& y)
{ return x>y ? y : x; }
 
REAL math_max(REAL x1, REAL x2);
REAL math_min(REAL x1,REAL x2);

_MATHBASE_INLINE REAL math_max(REAL x1, REAL x2)
{ return (x1>x2)?x1:x2; }

_MATHBASE_INLINE REAL math_min(REAL x1,REAL x2)
{ return (x1>x2)?x2:x1; }

_MATHBASE_INLINE BOOL IsRealZero(REAL value)
{ return value+1.0==1.0; }

class ZRealMatrix;
typedef ZRealMatrix* LPREALMATRIX;
typedef const ZRealMatrix* LPCREALMATRIX;

typedef struct tagMatrixHead
{
	int nRow, nCol;
	int nAllocLen;
	int nRefs;
//	BOOL bTemp;
	REAL* pMatrixData;
}ZMatrixHead;

class ZRealMatrix
{
private:
	ZMatrixHead* m_pMatrixHead;
	int m_nRowBase;
	int m_nColBase;

private:
	//calculate the positive integer(n) power ( mx^n )
	//this helper function is to accelerate the calculation of mx^n
	static ZRealMatrix PowerPosInt( const ZRealMatrix& mx, int n );
	//the only truly power calculation
	static void Power( const ZRealMatrix& mx, int n, ZRealMatrix& mxRet );
	//the only truly multiplication calculation
	static void Mul( const ZRealMatrix& mx1, const ZRealMatrix& mx2, ZRealMatrix& mxRet );
protected:
	void Init();
	ZMatrixHead* GetData() const;
	REAL* GetMatrixAddr() const;
	//reference increasement and decreasement
	int InternalAddRef();
	int InternalRelease();
	//allocate enough buffer for matrix(nRow,nCol);
	void AllocBuffer(int nRow, int nCol);
	//alloc a wrap headsite to accept the outside pData buffer for Attach
	void AllocHeadSite();
	void Release();

	//alloc memory (if necessary) for AssignCopy
	void AllocBeforeCopy(int nRow, int nCol);
	//ensure multi-referenced buffer is copied when being written
	void CopyBeforeWrite();
	//AssignCopy do a true memory copy, if necessary and can, it will allocate new memory
	void AssignCopy( const REAL* pMxAddr, int nRow, int nCol );

//property
public:
	int Index(int nRow, int nCol) const;
	int GetRow() const;
	int GetCol() const;
	int GetSize() const;
	BOOL IsNull() const;
	void SetBaseIndex( int nRowBase=0, int nColBase=0 );
	void GetBaseIndex( int& nRowBase, int& nColBase ) const;

	//reference is ensured to be single referenced
	REAL* GetMatrixData();
	//return the buffer address, without reference protection
	LPCREAL GetBuffAddr() const;
public:
// Constructors
	ZRealMatrix();						// uninitialized matrix
    ZRealMatrix( int m, int n );		// initialized matrix with random data
	ZRealMatrix( int m, int n, REAL rElement );
	ZRealMatrix( int m, int n, const REAL* prData );
	ZRealMatrix( int m, int n, const REAL** pprData );
    ZRealMatrix( const ZRealMatrix& mxInit );	//copy constructor
// Destructor
    ~ZRealMatrix();

//Initilization
	void Create( int m, int n);
	void Create( int m, int n, REAL rElement );
	void Create( int m, int n, const REAL* prData );
	void Create( int m, int n, const REAL** pprData );
    void Create( const ZRealMatrix& mxInit );
//	void CreateTemp( const ZRealMatrix& mxInit );

	//when matrix is attaching a data buffer, the buffer is locked automatically
	void Attach( REAL *pData, int nRow, int nCol );
	REAL* Detach();

	void SetUniform( REAL rElem=0 );
	void SetIdentity();
	void SetRandom( REAL rLow, REAL rUpper );

//buffer direct operation
	void UnlockBuffer();
	void LockBuffer();
	BOOL IsLocked();

	//copy data from prData to matrix. nCount - the length of prData
	//nOffset, the offset of the current matrix from which the copy begins
	//nSkip, how many skip on prData after each element copy
	void CopyBuffer( const REAL* prData, int nCount, int nOffset=0, int nSkip=0 );
	void CopyBuffer( const REAL **pprData, int m, int n, int nOffset=0, int nSkip=0 );

//matrix operation
	ZRealMatrix& Verse();
	ZRealMatrix& Invert();
	ZRealMatrix& SwapRow( int i, int j );
	ZRealMatrix& SwapCol( int i, int j );
	//reshape the matrix into a new (nRow,nCol)matrix, (nRow*nCol) must equals GetSize();
	ZRealMatrix& Reshape( int nRow, int nCol );
	//trying to make the matrix tempory. It's successful only when it's exculsively owning the buffer;
//	ZRealMatrix& MakeTemp();

//matrix access
	ZRealMatrix GetVerseMx() const;
	ZRealMatrix GetInvertMx() const;
	ZRealMatrix GetRowMx(int nRow) const;
	ZRealMatrix GetColMx(int nCol) const;
	ZRealMatrix GetChildMx( int nRow, int nCol, int nRowLen=-1, int nColLen=-1 ) const;
	void GetRowMx( int nRow, ZRealMatrix& mxRow );
	void GetColMx( int nCol, ZRealMatrix& mxCol );
	void GetChildMx( int nRow, int nCol, ZRealMatrix& mxChd );
	//get the diagonal vector, the return matrix is 1 column vector
	ZRealMatrix GetDiagMx() const;
	//get the lower trianglar part of matrix below the k-th diagonal line. 
	//k=0, the main diagonal; k<0, below the main diagonal; k>0, above the main diagonal
	ZRealMatrix GetTriLMx( int k=0 ) const;
	//get the upper trianglar part of matrix below the k-th diagonal line. 
	//k=0, the main diagonal; k<0, below the main diagonal; k>0, above the main diagonal
	ZRealMatrix GetTriUMx( int k=0 ) const;
	//reshape the matrix into a new (nRow,nCol)matrix, (nRow*nCol) must equals GetSize();
	ZRealMatrix GetReshapeMx( int nRow, int nCol ) const;

	//LU decomposition of square matrix
	void GetLUMx( ZRealMatrix& mxL, ZRealMatrix& mxU ) const;
	//determinant value of square matrix
	REAL GetDetVal() const;

//matrix operator
	//Access operator
	ZRealMatrix operator() (int i) const;		//Get a row vector
	ZRealMatrix operator[] (int i) const;		//Get a column vector
    REAL& operator () ( int i, int j );
    REAL  operator () ( int i, int j ) const;
	void SetAt( int i, int j, REAL rValue );
	REAL GetAt( int i, int j ) const;
	//return the buffer address, without reference protection
	operator LPCREAL() const;

    ZRealMatrix& operator = ( const ZRealMatrix& mx );

	//self matrix algebra operator
    ZRealMatrix& operator += ( const ZRealMatrix& mx );
    ZRealMatrix& operator -= ( const ZRealMatrix& mx );
	ZRealMatrix& operator *= ( const ZRealMatrix& mx );
	//solving linear equation x=B/A, self is B.
	ZRealMatrix& operator /= ( const ZRealMatrix& mxA );
	ZRealMatrix& operator ^= ( int n );
	ZRealMatrix& operator ^= ( REAL r );
	ZRealMatrix& Add( const ZRealMatrix& mx );
	ZRealMatrix& Sub( const ZRealMatrix& mx );
	ZRealMatrix& Mul( const ZRealMatrix& mx );
	//solving linear equation x=B/A, self is B.
	ZRealMatrix& Div( const ZRealMatrix& mxA );
	ZRealMatrix& Power( int n );
	ZRealMatrix& Power( REAL r );

	//self matrix element operator
	ZRealMatrix& operator += ( REAL r );
	ZRealMatrix& operator -= ( REAL r );
    ZRealMatrix& operator *= ( REAL r );
    ZRealMatrix& operator /= ( REAL r );
	ZRealMatrix& ElemAdd( REAL r );
	ZRealMatrix& ElemSub( REAL r );
	ZRealMatrix& ElemMul( REAL r );
	ZRealMatrix& ElemDiv( REAL r );
	ZRealMatrix& ElemPower( REAL r );

	//special negative operator
	ZRealMatrix& Neg();
    ZRealMatrix operator - ();
protected:
//global operator
	//matrix operator
    friend ZRealMatrix operator + ( const ZRealMatrix& mx1, const ZRealMatrix& mx2 );
    friend ZRealMatrix operator - ( const ZRealMatrix& mx1, const ZRealMatrix& mx2 );
    friend ZRealMatrix operator * ( const ZRealMatrix& mx1, const ZRealMatrix& mx2 );
	friend ZRealMatrix operator / ( const ZRealMatrix& mx1, const ZRealMatrix& mx2 );
	friend ZRealMatrix operator ^ ( const ZRealMatrix& mx, int n );
	friend ZRealMatrix operator ^ ( const ZRealMatrix& mx, REAL r );
	friend ZRealMatrix Add( const ZRealMatrix& mx1, const ZRealMatrix& mx2 );
	friend ZRealMatrix Sub( const ZRealMatrix& mx1, const ZRealMatrix& mx2 );
	friend ZRealMatrix Mul( const ZRealMatrix& mx1, const ZRealMatrix& mx2 );
	//solving linear equation x=B/A, mx1:B, mx2:A.
	friend ZRealMatrix Div( const ZRealMatrix& mx1, const ZRealMatrix& mx2 );
	friend ZRealMatrix Power( const ZRealMatrix& mx, int n );
	friend ZRealMatrix Power( const ZRealMatrix& mx, REAL r );

	//matrix element operator
    friend ZRealMatrix operator + ( const ZRealMatrix& mx, REAL r );
    friend ZRealMatrix operator - ( const ZRealMatrix& mx, REAL r );
    friend ZRealMatrix operator * ( const ZRealMatrix& mx, REAL r );
    friend ZRealMatrix operator * ( REAL r, const ZRealMatrix& mx );
	friend ZRealMatrix operator / ( const ZRealMatrix& mx, REAL r );

	friend ZRealMatrix ElemAdd( const ZRealMatrix& mx, REAL r );
	friend ZRealMatrix ElemSub( const ZRealMatrix& mx, REAL r );
	friend ZRealMatrix ElemMul( const ZRealMatrix& mx, REAL r );
	friend ZRealMatrix ElemDiv( const ZRealMatrix& mx, REAL r );
	friend ZRealMatrix ElemPower( const ZRealMatrix& mx, REAL r );

	//global element operator
	friend ZRealMatrix ElemLog( const ZRealMatrix& mx );
	friend ZRealMatrix ElemExp( const ZRealMatrix& mx );

	//matrix linear algebra functions
	friend BOOL MatrixSolve( const ZRealMatrix& mxA, const ZRealMatrix& mxB, ZRealMatrix& mxX );
	friend BOOL MatrixDecomLU( const ZRealMatrix& mxA, ZRealMatrix& mxL, ZRealMatrix& mxU );
};

//matrix operator
ZRealMatrix operator + ( const ZRealMatrix& mx1, const ZRealMatrix& mx2 );
ZRealMatrix operator - ( const ZRealMatrix& mx1, const ZRealMatrix& mx2 );
ZRealMatrix operator * ( const ZRealMatrix& mx1, const ZRealMatrix& mx2 );
ZRealMatrix operator / ( const ZRealMatrix& mx1, const ZRealMatrix& mx2 );
ZRealMatrix operator ^ ( const ZRealMatrix& mx, int n );
ZRealMatrix operator ^ ( const ZRealMatrix& mx, REAL r );
ZRealMatrix Add( const ZRealMatrix& mx1, const ZRealMatrix& mx2 );
ZRealMatrix Sub( const ZRealMatrix& mx1, const ZRealMatrix& mx2 );
ZRealMatrix Mul( const ZRealMatrix& mx1, const ZRealMatrix& mx2 );
//solving linear equation x=B/A, mx1:B, mx2:A.
ZRealMatrix Div( const ZRealMatrix& mx1, const ZRealMatrix& mx2 );
ZRealMatrix Power( const ZRealMatrix& mx, int n );
ZRealMatrix Power( const ZRealMatrix& mx, REAL r );

//matrix element operator
ZRealMatrix operator + ( const ZRealMatrix& mx, REAL r );
ZRealMatrix operator - ( const ZRealMatrix& mx, REAL r );
ZRealMatrix operator * ( const ZRealMatrix& mx, REAL r );
ZRealMatrix operator * ( REAL r, const ZRealMatrix& mx );
ZRealMatrix operator / ( const ZRealMatrix& mx, REAL r );

ZRealMatrix ElemAdd( const ZRealMatrix& mx, REAL r );
ZRealMatrix ElemSub( const ZRealMatrix& mx, REAL r );
ZRealMatrix ElemMul( const ZRealMatrix& mx, REAL r );
ZRealMatrix ElemDiv( const ZRealMatrix& mx, REAL r );
ZRealMatrix ElemPower( const ZRealMatrix& mx, REAL r );

//global element operator
ZRealMatrix ElemLog( const ZRealMatrix& mx );
ZRealMatrix ElemExp( const ZRealMatrix& mx );

#endif //__MATHBASE_H__