#ifndef _MATHFUNC
#define _MATHFUNC

#include "mathbase.h"
#include <memory.h>
//#include <exception>

/*******************全局定义*******************/
#define cstGold			0.61803399	//黄金分割数		
#define cstGoldTol		1e-10		//黄金搜索缺省计算精度

typedef REAL (*PCOSTFUNC)(void* Param, REAL x);
		//代价函数定义

/*****************异常原因********************/
#define cstEMath_MatrixZero		0x00000001
		//矩阵奇异错误
//异常处理类
/*class EMathSub : public exception
{
public:
	int m_ErrorCode ;
	EMathSub(int ErrorCode):m_ErrorCode(ErrorCode){};
	EMathSub(int ErrorCode, char * msg):m_ErrorCode(ErrorCode),exception(msg){};
};*/

/********************推荐函数************************/
//求解一组变量线性方程
inline BOOL SolveLineEqu1( REAL *Ma, REAL *Mb, REAL *Mx, int n );
//求解m组变量线性方程
inline BOOL SolveLineEqu( REAL *Ma, REAL *Mb, REAL *Mx, int n, int m );
//求解方阵的逆阵
inline BOOL MatInvert( REAL *Mat, int n );
//求解行列式的值
inline REAL MatValue( REAL *Mat, int n );

/********************Math Function*******************/
//高斯全选消元解线性方程	MA-方程系数矩阵(解方程后被破坏)  
//						   MB-方程右边项(解方程后被破坏)
//						   MX-方程代求变量
BOOL Gauss( REAL *Ma, REAL *Mb, REAL *Mx, int n );

//方阵求逆		a-代求方阵，求逆后方阵
BOOL GaussJordanInv( REAL * a, int n );

//求行列式的值  a-代求行列式(n*n),求解后破坏
REAL GaussMatValue(REAL * a,int n);

//Gauss-Jordan法求线性方程(m组变量)
//						a-方程系数常数项(n*n),求解后破坏
//						b-方程右端m组常数项，求解后变量值
BOOL GaussJordan(REAL * a,REAL * b,int n,int m);

//黄金分割法一维搜索
//			a,b初始搜索区间,对a,b大小顺序无要求
//			Param代价函数的参数, CostFunc代价函数指针
//			pMinv最小代价，可为NULL
//			tol精度为计算机最小精度的平方根
//			return-搜索到的最小值所在点
REAL GoldSearch( REAL a, REAL b, PCOSTFUNC CostFunc, void* Param,
				 REAL *pMinv=NULL, REAL tol=cstGoldTol );
/*********************************inline function******************/
//求解一组变量线性方程
inline BOOL SolveLineEqu1( REAL *Ma, REAL *Mb, REAL *Mx, int n )
{
	//因为高斯消元要求Mb!=Mx,所以...
	BOOL bRet = FALSE;
	if(Mb==Mx){
		REAL *Buff = new REAL[n];
		bRet = Gauss( Ma, Mb, Buff, n );
		if( bRet ){
			memcpy( Mx, Buff, sizeof(REAL)*n );
//			for( int i=0; i<n; i++ )Mx[i] = Buff[i];
		}
		delete[] Buff;
	}else{
		bRet = Gauss( Ma, Mb, Mx, n );
	}
	return bRet;
}
//求解m组变量线性方程
inline BOOL SolveLineEqu( REAL *Ma, REAL *Mb, REAL *Mx, int n, int m )
{
	BOOL bRet = GaussJordan( Ma, Mb, n, m );
	//因为Gauss-Jordan消元要求Mb=Mx,所以...
	if( bRet && Mb!=Mx ){
		memcpy( Mx, Mb, sizeof(REAL)*n*m );
		/*for( int i=0; i<n*m; i++ ){
			Mx[i]=Mb[i];
		}*/
	}
	return bRet;
}
//求解方阵的逆阵
inline BOOL MatInvert( REAL *Mat, int n )
{
	return GaussJordanInv( Mat, n );
}
//求解行列式的值
inline REAL MatValue( REAL *Mat, int n )
{
	return GaussMatValue( Mat, n );
}
/*******************************new linera sovler to compitable with matrix definition**********/
inline BOOL SolveLineEqu1( ZRealMatrix& Ma, ZRealMatrix& Mb, ZRealMatrix& Mx )
{
	ASSERT( (Mb.GetCol()==1) && (Mx.GetCol()==1) && (Ma.GetCol()==Ma.GetRow()) 
		&& (Ma.GetCol()==Mx.GetRow()) && (Ma.GetCol()==Mb.GetRow()) );
	return SolveLineEqu1( (REAL*)Ma.GetMatrixData(), (REAL*)Mb.GetMatrixData(), (REAL*)Mx.GetMatrixData(), Mb.GetRow() );
}

/*inline void SolveLineEqu1( ZRealMatrix& Ma, ZVect& Vb, ZVect& Vx )
{
	ASSERT( (Ma.GetCol()==Ma.GetRow()) && (Ma.GetCol()==Vb.Size()) && (Ma.GetCol()==Vx.Size()) );
	SolveLineEqu1( (REAL*)Ma.GetMatrixData(), (REAL*)Vb.GetMatrixData(), (REAL*)Vx.GetMatrixData(), Vb.Size() );
}*/

inline BOOL SolveLineEqu( ZRealMatrix& Ma, ZRealMatrix& Mb, ZRealMatrix& Mx )
{
	ASSERT( (Mb.GetCol()==Mx.GetCol()) && (Ma.GetCol()==Ma.GetRow()) 
		&& (Ma.GetCol()==Mx.GetRow()) && (Ma.GetCol()==Mb.GetRow()) );
	return SolveLineEqu( (REAL*)Ma.GetMatrixData(), (REAL*)Mb.GetMatrixData(), (REAL*)Mx.GetMatrixData(),
		Mb.GetRow(), Mb.GetCol() );
}

#endif
