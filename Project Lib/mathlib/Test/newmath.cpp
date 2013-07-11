// newmath.cpp : Defines the entry point for the console application.
//

#include "stdafx.h"
#include "iostream.h"
#include <mathbase.h>
#include <matrixio.h>
#include <mathfunc.h>

void PrintMx( ZRealMatrix& mx )
{
//	MATRIX_LOCK( mx );

	ZRealMatrix mx3;
	cout<<mx;
	mx3 = mx;
	mx3.SetIdentity();
	cout<<mx3;
	cout<<mx;
}

void TestSolver()
{
	REAL a[] = { 5,9,10,-2,12,6,9,-10,20};
	REAL b[] = { 1,10,-10, 3, 9, -5 };
	ZRealMatrix mxA(3,3,a);
	ZRealMatrix mxB(2,3,b);
	mxB.Verse();
	ZRealMatrix mx1;
	mx1 = mxB/mxA;
	cout<<mx1<<endl;

	ZRealMatrix mx2 = mxA.GetInvertMx()*mxB;
	cout<<mx2<<endl;

	cout<<mxA<<endl;
	cout<<mxB<<endl;
}

void TestMemory()
{
	ZRealMatrix mx(3,3);
	ZRealMatrix mx2 = mx*mx;
}

void TestPower()
{
	REAL a[] = { 5,9,10,-2,12,6,9,-10,20};
	ZRealMatrix mxA(3,3,a);
	mxA.SetBaseIndex( 2,2 );
	mxA.SwapCol( 2, 3 );
	cout<<Power(mxA,2)<<endl;
	cout<<(mxA^2)<<endl;
	cout<<(mxA^-1)<<endl;

	cout<<mxA<<endl;
	mxA.Reshape( 1, 9 );
	cout<<mxA<<endl;
}

void TestAttach()
{
	REAL a[] = { 5,9,10,-2,12,6,9,-10,20 };
	ZRealMatrix mxA;
	mxA.Attach( a, 3, 3 );
	mxA.SetBaseIndex( 2,2 );
	mxA.SwapCol( 2, 3 );

	cout<<mxA<<endl;
	mxA.Reshape( 1, 9 );
	cout<<mxA<<endl;
	ZRealMatrix mx(3,3);
	mx.SetIdentity();
	mxA = mx;

	//ZRealMatrix mxT = mxA + 5.0;
	cout<<ElemPower((mxA + 5.0)*0.5, 0.5);
	mxA.Detach();
}

REAL a[] = {3,9,4,2,1,8,4,9,1};

void TestTemp()
{
/*	ZRealMatrix mx1(3,3);
	mx1.SetIdentity();
	cout<<mx1;
*/
	ZRealMatrix mx2(3,3);
	mx2.SetUniform(5);
	cout<<mx2;

	{cout<<mx2*mx2*mx2*mx2*mx2*mx2*mx2;}
	cout<<"power\n";
	mx2 = (mx2^2);

	ZRealMatrix mxt;
	mxt = mx2.GetRowMx(2);
	mxt.SetIdentity();
	mxt.Verse();
	mx2.GetColMx( 3, mxt );
	mx2.GetColMx( 1, mxt );
	{cout<<(mx2^200);}

//	PrintMx( mx1*mx2 );
//	cout<<mx1*mx2;
//	ZRealMatrix mx3;
//	mx3 = mx1*mx2;
//	mx3 = mx1.GetColMx(2);
//	cout<<mx3;
//	mx3 = -mx3;
//	mx3.Create( mx1 );
//	mx1 = mx2 = mx1*mx2;
//	mx1*mx2 = mx1*mx2;
//	cout<<mx2*mx1;
//	ZRealMatrix mx3;
//	mx2*mx1;
//	cout<<mx1;
//	mx2.SetIdentity( );
	ZRealMatrix m;
	m.Create( 3, 3, 0.0 );
	m.CopyBuffer( a, 9 );
	cout<<m;
	mx2 = m;
	cout<<mx2;

	mxt = mx2.GetInvertMx();
	cout<< mxt;
	cout<<mxt*mx2;
}


void TestChild()
{
	REAL a[] = { 5,9,10,-2,12,6,9,-10,20 };
	ZRealMatrix mxA;
	mxA.Attach( a, 3, 3 );
	mxA.SwapCol( 2, 3 );

	cout<<mxA<<endl;

	cout<<mxA.GetChildMx(1, 1, 2 );

	mxA.Detach();
}

REAL y_x2( void* p, REAL x )
{
	return x*x;
}


int main(int argc, char* argv[])
{
	REAL x = BrentSearch( -1, 100, y_x2, NULL, 100 );

	TestSolver();
	TestMemory();
	TestPower();
	TestAttach();

	TestTemp();


	TestChild();

	return 0;
}

