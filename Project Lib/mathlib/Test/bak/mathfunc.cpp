#include "mathfunc.h"

#define cstEMsgGauss	"Gauss消去法解方程出错！"
#define cstEMsgGaussJordan "Gauss-Jordan消去法解方程出错！"
#define cstEMsgGaussJordanInv "Gauss-Jordan消去法求逆出错！"

BOOL Gauss(REAL *a, REAL *b, REAL *x, int n )
{ 
	int *js,k,i,j,is,p,q;
    REAL d,t;
    js= new int[n];
	BOOL success = FALSE;
	
    for (k=0;k<=n-2;k++){
        d=0.0;
        for (i=k;i<=n-1;i++){
			for (j=k;j<=n-1;j++){ 
				t=fabs(a[i*n+j]);
				if (t>d) { d=t; js[k]=j; is=i;}
            }
		}
		if (IsRealZero(d))goto clear;
		if (js[k]!=k){
			for (i=0;i<=n-1;i++){ 
				p=i*n+k; q=i*n+js[k];
				t=a[p]; a[p]=a[q]; a[q]=t;
			}
		}
		if (is!=k){
			for (j=k;j<=n-1;j++){
				p=k*n+j; q=is*n+j;
				t=a[p]; a[p]=a[q]; a[q]=t;
			}
			t=b[k]; b[k]=b[is]; b[is]=t;
		}
		d=a[k*n+k];
		for (j=k+1;j<=n-1;j++){ 
			p=k*n+j; a[p]=a[p]/d;
		}
		b[k]=b[k]/d;
		for (i=k+1;i<=n-1;i++){
			for (j=k+1;j<=n-1;j++){
				p=i*n+j;
				a[p]=a[p]-a[i*n+k]*a[k*n+j];
			}
			b[i]=b[i]-a[i*n+k]*b[k];
		}
	}
    d=a[(n-1)*n+n-1];
	
    if ( IsRealZero(fabs(d)) )goto clear;
	
    x[n-1]=b[n-1]/d;
    for (i=n-2;i>=0;i--){ 
		t=0.0;
		for (j=i+1;j<=n-1;j++)
			t=t+a[i*n+j]*x[j];
		x[i]=b[i]-t;
	}
    js[n-1]=n-1;
    for (k=n-1;k>=0;k--){
		if (js[k]!=k){ 
			t=x[k]; x[k]=x[js[k]]; x[js[k]]=t;
		}
	}
	success = TRUE;
clear:
	delete[] js;
	return success;
//	if( !success )throw new EMathSub( cstEMath_MatrixZero, cstEMsgGauss );
}

BOOL GaussJordanInv(REAL *a,int n)
{ 
	int *is,*js,i,j,k,l,u,v;
    REAL d,p;
    is=new int[n];
    js=new int[n];
	BOOL success = FALSE;
	
    for (k=0; k<=n-1; k++){ 
		d=0.0;
		for (i=k; i<=n-1; i++){
			for (j=k; j<=n-1; j++){ 
				l=i*n+j; p=fabs(a[l]);
				if (p>d) { d=p; is[k]=i; js[k]=j;}
			}
		}
		if (IsRealZero(d))goto clear; //An Error Orcurred
		
		if (is[k]!=k){
			for (j=0; j<=n-1; j++){ 
				u=k*n+j; v=is[k]*n+j;
				p=a[u]; a[u]=a[v]; a[v]=p;
			}
		}
		if (js[k]!=k){
			for (i=0; i<=n-1; i++){ 
				u=i*n+k; v=i*n+js[k];
				p=a[u]; a[u]=a[v]; a[v]=p;
			}
		}
		l=k*n+k;
		a[l]=1.0/a[l];
		for (j=0; j<=n-1; j++){
			if (j!=k){ 
				u=k*n+j; a[u]=a[u]*a[l];
			}
		}
		for (i=0; i<=n-1; i++){
			if (i!=k){
				for (j=0; j<=n-1; j++){
					if (j!=k){ 
						u=i*n+j;
						a[u]=a[u]-a[i*n+k]*a[k*n+j];
					}
				}
			}
		}
		for (i=0; i<=n-1; i++){
			if (i!=k){ 
				u=i*n+k; a[u]=-a[u]*a[l];
			}
		}
	}
    for (k=n-1; k>=0; k--){ 
		if (js[k]!=k){
			for (j=0; j<=n-1; j++){ 
				u=k*n+j; v=js[k]*n+j;
				p=a[u]; a[u]=a[v]; a[v]=p;
			}
		}
		if (is[k]!=k){
			for (i=0; i<=n-1; i++){ 
				u=i*n+k; v=i*n+is[k];
				p=a[u]; a[u]=a[v]; a[v]=p;
			}
		}
	}
	success = TRUE;
	
clear:
    delete[] is;
	delete[] js;
	return success;
//	if( !success )throw new EMathSub(cstEMath_MatrixZero, cstEMsgGaussJordanInv);
}


REAL GaussMatValue(REAL * a, int n)
{
	int i,j,k,is,js,l,u,v;
	REAL f,det,q,d;
	f=1.0;
	det=1.0;
	for (k=0;k<=n-2;k++)
	{
		q=0.0;
		for (i=k;i<n-1;i++){
			for (j=k;j<n-1;j++)
			{
				l=i*n+j;
				d=fabs(a[l]);
				if (d>q) {q=d;is=i;js=j;}
			}
		}
		if ( IsRealZero(q) ){
			det=0.0;return det;
		}
		if (is!=k){
			f=-f;
			for (j=k;j<=n-1;j++){
				u=k*n+j;
				v=is*n+j;
				d=a[u];
				a[u]=a[v];
				a[v]=d;
			}
		}
		if (js!=k){
			f=-f;
			for (i=k;i<=n-1;i++){
				u=i*n+js;
				v=i*n+k;
				d=a[u];
				a[u]=a[v];
				a[v]=d;
			}
		}
		l=k*n+k;
		det=det*a[l];
		for (i=k+1;i<=n-1;i++){
			d=a[i*n+k]/a[l];
			for (j=k+1;j<=n-1;j++){
				u=i*n+j;
				a[u]-=d*a[k*n+j];
			}
		}
	}
	det=f*det*a[n*n-1];
	return det;
}

BOOL GaussJordan(REAL * a,REAL * b,int n,int m)
{
	int *js,k,i,j,is,p,q;
	REAL d,t;
	js=new int [n];
	BOOL success = FALSE;
	for (k=0;k<=n-1;k++){
		d=0.0;
		for (i=k;i<=n-1;i++){
			for (j=k;j<=n-1;j++){
				t=fabs(a[i*n+j]);
				if (t>d){ d=t; js[k]=j; is=i; }
            }
		}
		if (IsRealZero(d)) goto clear; //error occored
		
		if (js[k]!=k){
			for (i=0;i<=n-1;i++){
				p=i*n+k;
				q=i*n+js[k];
				t=a[p];
				a[p]=a[q];
				a[q]=t;
			}
		}
		if (is!=k){
			for (j=k;j<=n-1;j++){
				p=k*n+j;
				q=is*n+j;
				t=a[p];
				a[p]=a[q];
				a[q]=t;
			}
			for (j=0;j<=m-1;j++){
				p=k*m+j;
				q=is*m+j;
				t=b[p];
				b[p]=b[q];
				b[q]=t;
			}
		}
		
		d=a[k*n+k];
		
		for (j=k+1;j<=n-1;j++){	p=k*n+j; a[p]/=d; }
		
		for (j=0;j<=m-1;j++){ p=k*m+j; b[p]=b[p]/d; }
		
		for (j=k+1;j<=n-1;j++){
			for (i=0;i<=n-1;i++){
				p=i*n+j;
				if (i!=k)a[p]-=a[i*n+k]*a[k*n+j];
            }
		}
		for (j=0;j<=m-1;j++){
			for (i=0;i<=n-1;i++){
				p=i*m+j;
				if (i!=k)b[p]-=a[i*n+k]*b[k*m+j];
			}
		}
	}//end of for(k=0...)
	
	for (k=n-1;k>=0;k--){
		if (js[k]!=k){
			for (j=0;j<=m-1;j++){
				p=k*m+j;
				q=js[k]*m+j;
				t=b[p];
				b[p]=b[q];
				b[q]=t;
			}
		}
	}
	success = TRUE;
clear:
	delete [] js;
	return success;
//	if( !success )throw new EMathSub( cstEMath_MatrixZero, cstEMsgGaussJordan );
}

REAL GoldSearch( REAL a, REAL b, PCOSTFUNC CostFunc, void* Param,
				REAL *pMinv, REAL tol )
{
	REAL Minx, Miny;
	REAL x1, x2, f1, f2;
	x1 = b - cstGold*(b-a);
	x2 = a + cstGold*(b-a);
	f1 = (*CostFunc)(Param, x1);
	f2 = (*CostFunc)(Param, x2);
	while( fabs(b-a)>tol*(fabs(x1)+fabs(x2)) ){
		if( f1>=f2 ){
			a  = x1;
			x1 = x2;
			x2 = a + cstGold*(b-a);
			f1 = f2;
			f2 = (*CostFunc)(Param, x2);
		}else{
			b  = x2;
			x2 = x1;
			x1 = b - cstGold*(b-a);
			f2 = f1;
			f1 = (*CostFunc)(Param, x1);
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