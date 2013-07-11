#include "mathfunc.h"
#include "mathlnag.h"

//LU decomposition function.
//parameters:
//prMat(in): point to the memory block of square matrix(nN*nN)
//nN (in) : row and column number of matrix
//prL (out): point to the memory block of L matrix
//prU (out): point to the memory block of U matrix
BOOL DecompLU( const REAL* prMat, int nN, REAL* prL, REAL* prU )
{
	//copy original matrix to pU
	memcpy(prU, prMat, nN*nN*sizeof(REAL) );
	//clear prL to zero matrix
	memset(prL, 0, nN*nN*sizeof(REAL) );

	//main loop
	for( int t=0; t<nN; t++ ){
		//first, push the column values into L matrix, from up to bottom
		for( int k=t; k<nN; k++ ){
			//the elements should be prL[k,t] = prL[k,t]/prU[i,t]
			prL[k*nN+t] = prU[k*nN+t]/prU[t*nN+t];
		}

		if( prU[t*nN+t]+1.0 == 1.0 )return FALSE;

		//next, make one step of Gauss elimination
		for( int i=t+1; i<nN; i++ ){
			//attention, must sweep from right to left
			for( int j=nN-1; j>=t; j-- ){
				//prU[i,j] = prU[i,j] - (prU[i,t]/prU[t,t])*prU[t,j]
				prU[i*nN+j] -= prU[i*nN+t]*prU[t*nN+j]/prU[t*nN+t];
			}
		}
	}
	return TRUE;
}

//Sovle x by LU factor
//parameters:
//prL, prU (in) : point the L and U matrix(nN*nN)
//prb (in) : point to the right side of equation(nN*1)
//nN (in): dimension of matrix
//prx (out) : point the solution of equation(nN*1)
void SolveLU( const REAL* prL, const REAL* prU, const REAL* prb, int nN, REAL* prx )
{
	memcpy( prx, prb, nN*sizeof(REAL) );
	//step 1, forward substitiution for y (y is stored in x)
	for( int i=0; i<nN; i++ ){
		for( int j=0; j<i; j++ ){
			prx[i] -= prL[i*nN+j]*prx[j];
		}
		prx[i] /= prL[i*nN+i];
	}

	//step 2, backward substitution for x
	for( i=nN-1; i>=0; i-- ){
		for( int j=nN-1; j>i; j-- ){
			prx[i] -= prU[i*nN+j]*prx[j];
		}
		prx[i] /= prU[i*nN+i];
	}
}

//LU decomposition for banded matrix.
//parameters:
//prMat(in): point to the memory block of matrix(nN*nN)
//nN (in) : row and column number of matrix
//nB (in) : Band width of the banded matrix
//prLU (out): point to the memory block of LU matrix (L and U are unioned together by take out the 1 diagonal of L
BOOL DecompBandLU( const REAL* prMat, int nN, int nB, REAL* prLU )
{
	//copy original matrix to pU
	memcpy(prLU, prMat, nN*nN*sizeof(REAL) );

	//main loop, for each column j=0..nN-1
	for( int j=0; j<nN; j++ ){
		if( prLU[j*nN+j] +1.0 == 1.0 )return FALSE;

		//for each row i=j+1..min(nN-1,j+nB)
		for( int i=j+1; i<=min(nN-1,j+nB); i++ ){
			//generating L
			prLU[i*nN+j] /= prLU[j*nN+j];

			//for each column k=j+1..min(nN-1, j+B)
			for( int k=j+1; k<=min(nN-1,j+nB); k++ ){
				//generating U
				prLU[i*nN+k] -= prLU[i*nN+j]*prLU[j*nN+k];
			}
		}
	}

	return TRUE;
}

//Sovle x by LU factor of a banded matrix
//parameters:
//prLU (in) : point the union of L and U matrix(nN*nN)
//prb (in) : point to the right side of equation(nN*1)
//nN (in): dimension of matrix
//nB (in): band width of the matrix
//prx (out) : point the solution of equation(nN*1)
void SolveBandLU( const REAL* prLU, const REAL* prb, int nN, int nB, REAL* prx )
{
	memcpy( prx, prb, nN*sizeof(REAL) );
	//step 1, forward substitiution for y (y is stored in x)
	for( int i=1; i<nN; i++ ){
		for( int j=max(0,i-nB); j<i; j++ ){
			prx[i] -= prLU[i*nN+j]*prx[j];
		}
//		prx[i] /= prL[i*nN+i];
	}

	//step 2, backward substitution for x
	for( i=nN-1; i>=0; i-- ){
		for( int j=min(nN-1,i+nB); j>i; j-- ){
			prx[i] -= prLU[i*nN+j]*prx[j];
		}
		prx[i] /= prLU[i*nN+i];
	}
}

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


REAL GaussDetVal(REAL * a, int n)
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