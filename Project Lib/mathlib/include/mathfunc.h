#ifndef _MATHFUNC
#define _MATHFUNC
/*********************************************************************************
						general math functions library interface
						Copyright Shengquan Yan
						The source code is not allowed to modification without the permission of the author.
						email: smyan@uiuc.edu sq_yan@hotmail.com
						
						all functions here discarded except GoldSearch(...)

						discarded functions:
							SolveLineEqu1( REAL *Ma, REAL *Mb, REAL *Mx, int n );
							SolveLineEqu( REAL *Ma, REAL *Mb, REAL *Mx, int n, int m );
							MatInvert( REAL *Mat, int n );
							MatValue( REAL *Mat, int n );
							
**********************************************************************************/

#include "mathbase.h"
#include "mathlnag.h"
#include <memory.h>
//#include <exception>

/*******************ȫ�ֶ���*******************/
#define cstGold			0.61803399	//�ƽ�ָ���		
#define cstGoldTol		1e-10		//�ƽ�����ȱʡ���㾫��

typedef REAL (*PCOSTFUNC)(void* pParam, REAL x);
		//���ۺ�������

//�ƽ�ָһά����
//			a,b��ʼ��������,��a,b��С˳����Ҫ��
//			Param���ۺ����Ĳ���, CostFunc���ۺ���ָ��
//			pMinv��С���ۣ���ΪNULL
//			tol����Ϊ�������С���ȵ�ƽ����
//			return-����������Сֵ���ڵ�
REAL GoldSearch( REAL a, REAL b, PCOSTFUNC pCostFunc, void* pParam,
				 REAL *pMinv=NULL, REAL tol=cstGoldTol );

//this is a hybrid searching algorithm combing gold search and quadratic search

REAL BrentSearch( REAL x_lower, REAL x_upper, PCOSTFUNC pf, void* pParam, int maxiter );


/*****************�쳣ԭ��********************/
#define cstEMath_MatrixZero		0x00000001
		//�����������
//�쳣������
/*class EMathSub : public exception
{
public:
	int m_ErrorCode ;
	EMathSub(int ErrorCode):m_ErrorCode(ErrorCode){};
	EMathSub(int ErrorCode, char * msg):m_ErrorCode(ErrorCode),exception(msg){};
};*/

/********************�Ƽ�����************************/
//���һ��������Է���
inline BOOL SolveLineEqu1( REAL *Ma, REAL *Mb, REAL *Mx, int n );
//���m��������Է���
inline BOOL SolveLineEqu( REAL *Ma, REAL *Mb, REAL *Mx, int n, int m );
//��ⷽ�������
inline BOOL MatInvert( REAL *Mat, int n );
//�������ʽ��ֵ
inline REAL MatValue( REAL *Mat, int n );

/*********************************inline function******************/
//���һ��������Է���
inline BOOL SolveLineEqu1( REAL *Ma, REAL *Mb, REAL *Mx, int n )
{
	//��Ϊ��˹��ԪҪ��Mb!=Mx,����...
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
//���m��������Է���
inline BOOL SolveLineEqu( REAL *Ma, REAL *Mb, REAL *Mx, int n, int m )
{
	BOOL bRet = GaussJordan( Ma, Mb, n, m );
	//��ΪGauss-Jordan��ԪҪ��Mb=Mx,����...
	if( bRet && Mb!=Mx ){
		memcpy( Mx, Mb, sizeof(REAL)*n*m );
		/*for( int i=0; i<n*m; i++ ){
			Mx[i]=Mb[i];
		}*/
	}
	return bRet;
}
//��ⷽ�������
inline BOOL MatInvert( REAL *Mat, int n )
{
	return GaussJordanInv( Mat, n );
}
//�������ʽ��ֵ
inline REAL MatValue( REAL *Mat, int n )
{
	return GaussDetVal( Mat, n );
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
