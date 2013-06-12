// test.cpp : Defines the entry point for the console application.
//

#include "stdafx.h"
#include <iostream.h>
#include "..\\mathsub\\mathbase.h"
#include "..\\mathsub\\matrixio.h"

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
	cout<<ElemPower((mxA + 5)*0.5, 0.5);
	mxA.Detach();
}

int main(int argc, char* argv[])
{
//	TestSolver();
//	TestMemory();
//	TestPower();
	TestAttach();
	return 0;
}
