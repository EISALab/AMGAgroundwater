#ifndef __MATHLNAG_H__
#define __MATHLNAG_H__
/*******************************************************************
				linear algebra algorithm
				Copyright Shengquan Yan
				The source code is not allowed to modification without the permission of the author.
				email: smyan@uiuc.edu sq_yan@hotmail.com
********************************************************************/

//高斯全选消元解线性方程	MA-方程系数矩阵(解方程后被破坏)  
//						   MB-方程右边项(解方程后被破坏)
//						   MX-方程代求变量
BOOL Gauss( REAL *Ma, REAL *Mb, REAL *Mx, int n );

//Gauss-Jordan法求线性方程(m组变量)
//						a-方程系数常数项(n*n),求解后破坏
//						b-方程右端m组常数项，求解后变量值
BOOL GaussJordan(REAL * a,REAL * b,int n,int m);

//方阵求逆		a-代求方阵，求逆后方阵
BOOL GaussJordanInv( REAL * a, int n );

//求行列式的值  a-代求行列式(n*n),求解后破坏
REAL GaussDetVal(REAL * a,int n);


//LU decomposition function.
//parameters:
//prMat(in): point to the memory block of square matrix(nN*nN)
//nN (in) : row and column number of matrix
//prL (out): point to the memory block of L matrix
//prU (out): point to the memory block of U matrix
BOOL DecompLU( const REAL* prMat, int nN, REAL* prL, REAL* prU );

//Sovle x by LU factor
//parameters:
//prL, prU (in) : point the L and U matrix(nN*nN)
//prb (in) : point to the right side of equation(nN*1)
//nN (in): dimension of matrix
//prx (out) : point the solution of equation(nN*1)
void SolveLU( const REAL* prL, const REAL* prU, const REAL* prb, int nN, REAL* prx );

//LU decomposition for banded matrix.
//parameters:
//prMat(in): point to the memory block of matrix(nN*nN)
//nN (in) : row and column number of matrix
//nB (in) : Band width of the banded matrix
//prLU (out): point to the memory block of LU matrix (L and U are unioned together by take out the 1 diagonal of L
BOOL DecompBandLU( const REAL* prMat, int nN, int nB, REAL* prLU );

//Sovle x by LU factor of a banded matrix
//parameters:
//prLU (in) : point the union of L and U matrix(nN*nN)
//prb (in) : point to the right side of equation(nN*1)
//nN (in): dimension of matrix
//nB (in): band width of the matrix
//prx (out) : point the solution of equation(nN*1)
void SolveBandLU( const REAL* prLU, const REAL* prb, int nN, int nB, REAL* prx );

#endif //__MATHLNAG_H__