#include "mathfunc.h"

#define cstEMsgGauss	"Gauss消去法解方程出错！"
#define cstEMsgGaussJordan "Gauss-Jordan消去法解方程出错！"
#define cstEMsgGaussJordanInv "Gauss-Jordan消去法求逆出错！"

REAL GoldSearch( REAL a, REAL b, PCOSTFUNC pCostFunc, void* pParam,
				REAL *pMinv, REAL tol )
{
	REAL Minx, Miny;
	REAL x1, x2, f1, f2;
	x1 = b - cstGold*(b-a);
	x2 = a + cstGold*(b-a);
	f1 = (*pCostFunc)(pParam, x1);
	f2 = (*pCostFunc)(pParam, x2);
	while( fabs(b-a)>tol*(fabs(x1)+fabs(x2)) ){
		if( f1>=f2 ){
			a  = x1;
			x1 = x2;
			x2 = a + cstGold*(b-a);
			f1 = f2;
			f2 = (*pCostFunc)(pParam, x2);
		}else{
			b  = x2;
			x2 = x1;
			x1 = b - cstGold*(b-a);
			f2 = f1;
			f1 = (*pCostFunc)(pParam, x1);
		}
	}
	if( f1<f2 ){
		Minx = x1;
		Miny = f1;
	}else{
		Minx = x2;
		Miny = f2;
	}
	if( pMinv!=NULL )*pMinv = Miny;
	return Minx;
}