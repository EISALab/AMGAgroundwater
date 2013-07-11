#include <stdlib.h>
#include <mathbase.h>

typedef struct _NORMALIZEPARAM
{
	REAL *prOrigMin, *prOrigMax;
	REAL *prNormMin, *prNormMax;
	int nLen;
}NormalizeParam;

//normParam.nLen is the vector length of each touple, which is the same length of prMin and prMax
//n is the number of touples that will be normalized.
//prData points to the data, the touples should be stored one by one sequentially.
static void Normalize( NormalizeParam* pNormParam, REAL* prData, int n );
static void Unnormalize( NormalizeParam* pNormParam, REAL* prData, int n );

extern "C"{
//////////////////////////for normalize and unnormalize
	NormalizeParam* _stdcall CREATENORMALIZE( REAL* prOrigMin, REAL* prOrigMax, REAL* prNormMin, REAL* prNormMax, int& nLen )
	{
		NormalizeParam* pNormParam = new NormalizeParam;
		pNormParam->prOrigMin = prOrigMin;
		pNormParam->prOrigMax = prOrigMax;
		pNormParam->prNormMin = prNormMin;
		pNormParam->prNormMax = prNormMax;
		pNormParam->nLen = nLen;
		return pNormParam;
	}
	void _stdcall RELEASENORMALIZE( NormalizeParam*& pNormParam )
	{
		delete pNormParam;
	}
	void _stdcall NORMALIZE( NormalizeParam*& pNormParam, REAL* prData, int& n )
	{
		Normalize( pNormParam, prData, n );
	}
	void _stdcall UNNORMALIZE( NormalizeParam*& pNormParam, REAL* prData, int& n )
	{
		Unnormalize( pNormParam, prData, n );
	}
}
	

//normParam.nLen is the vector length of each touple, which is the same length of prMin and prMax
//n is the number of touples that will be normalized.
//prData points to the data, the touples should be stored one by one sequentially.
void Normalize( NormalizeParam* pNormParam, REAL* prData, int n )
{
	for( int i=0; i<pNormParam->nLen; i++ ){
		REAL t1 = pNormParam->prOrigMax[i] - pNormParam->prOrigMin[i];
		REAL t2 = pNormParam->prNormMax[i] - pNormParam->prNormMin[i];
		for( int j=0; j<n; j++ ){
			int id = i + j * pNormParam->nLen;
			prData[id] = (prData[id] - pNormParam->prOrigMin[i]) / t1 * t2 + pNormParam->prNormMin[i];
		}
	}
}

//normParam.nLen is the vector length of each touple, which is the same length of prMin and prMax
//n is the number of touples that will be normalized.
//prData points to the data, the touples should be stored one by one sequentially.
void Unnormalize( NormalizeParam* pNormParam, REAL* prData, int n )
{
	for( int i=0; i<pNormParam->nLen; i++ ){
		REAL t1 = pNormParam->prNormMax[i] - pNormParam->prNormMin[i];
		REAL t2 = pNormParam->prOrigMax[i] - pNormParam->prOrigMin[i];
		for( int j=0; j<n; j++ ){
			int id = i + j * pNormParam->nLen;
			prData[id] = (prData[id] - pNormParam->prNormMin[i]) / t1 * t2 + pNormParam->prOrigMin[i];
		}
	}
}