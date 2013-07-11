#include "nnsgaHelperC.h"
#include "mininnet.h"
//#include "neuranet.h"

#define _ATL_CSTRING_EXPLICIT_CONSTRUCTORS	// some CString constructors will be explicit
#include <atlbase.h>

#define MAXPATH 256

#import "c:\matlabr12\bin\win32\mlapp.tlb" no_namespace
#define SET_DEFAULT_PARAM(t) {(t).vt=VT_ERROR; (t).scode=DISP_E_PARAMNOTFOUND;}

static _bstr_t RunMatlab( char* strMFile );
static void LoadMatlabNN( CMiniNeuralNet* pNet, char* strWeightFile );

//normParam.nLen is the vector length of each touple, which is the same length of prMin and prMax
//n is the number of touples that will be normalized.
//prData points to the data, the touples should be stored one by one sequentially.
static void Normalize( NormalizeParam* pNormParam, REAL* prData, int n );
static void Unnormalize( NormalizeParam* pNormParam, REAL* prData, int n );

extern "C"{
	int _stdcall CREATENET( NeuralNetParam* netParam )
	{
		CMiniNeuralNet* pNet = new CMiniNeuralNet();
		pNet->Create( netParam );

		//conver pointer to handle
		return (int)pNet;
	}
	
	void _stdcall SIMNET( CMiniNeuralNet*& pNet, REAL* prInput, REAL* prOutput )
	{
		pNet->Predict( prInput, prOutput );
	}

	double _stdcall TESTNET( CMiniNeuralNet*& pNet, REAL* prInput, REAL* prOutput, int& nPattern )
	{
		return pNet->TestPatternError( prInput, prOutput, nPattern );
	}

	void _stdcall RELEASENET( CMiniNeuralNet*& pNet )
	{
		delete pNet;
	}

	double _stdcall TRAINMATLABNETC1( char* pstrM, int nMChar, char* pstrTrain, int nTrainChar, char* pstrNet, int nNetChar )
	{
		char strM[MAXPATH], strTrain[MAXPATH], strNet[MAXPATH];
		strftoc( pstrM, nMChar, strM );
		strftoc( pstrTrain, nTrainChar, strTrain );
		strftoc( pstrNet, nNetChar, strNet );

		FILE* pf = fopen( "netcmd.txt", "w" );
		fprintf( pf, "cd neutrain\n" );
		fprintf( pf, "put %s\n", strTrain );
		fprintf( pf, "run matlabnn %s %s %s mse.txt\n", strM, strTrain, strNet );
		fprintf( pf, "get %s mse.txt\n", strNet );
		fclose( pf );

		system( "ncptln cee-zzqr smyan netcmd.txt" );

		double mse = 0;
		pf = fopen( "mse.txt", "r" );
		fscanf( pf, "%lf", &mse );
		fclose( pf );

		return mse;
	}

	double _stdcall TRAINMATLABNETC2( char* pstrM, int nMChar, char* pstrTrain, int nTrainChar, char* pstrNet, int nNetChar, char* pstrValid, int nValidChar )
	{
		char strM[MAXPATH], strTrain[MAXPATH], strNet[MAXPATH], strValid[MAXPATH];
		strftoc( pstrM, nMChar, strM );
		strftoc( pstrTrain, nTrainChar, strTrain );
		strftoc( pstrNet, nNetChar, strNet );
		strftoc( pstrValid, nValidChar, strValid );

		FILE* pf = fopen( "netcmd.txt", "w" );
		fprintf( pf, "cd neutrain\n" );
		fprintf( pf, "put %s %s\n", strTrain, strValid );
		fprintf( pf, "run matlabnn %s %s %s mse.txt %s\n", strM, strTrain, strNet, strValid );
		fprintf( pf, "get %s mse.txt\n", strNet );
		fclose( pf );

		system( "ncptln cee-zzqr matlab netcmd.txt" );

		double mse = 0;
		pf = fopen( "mse.txt", "r" );
		fscanf( pf, "%lf", &mse );
		fclose( pf );

		return mse;
	}

/*	double _stdcall TRAINMATLABNET( char* pstrM, int nMChar, char* pstrTrain, int nTrainChar, char* pstrNet, int nNetChar )
	{
		char strM[MAXPATH], strTrain[MAXPATH], strNet[MAXPATH];
		getcwd( strTrain, MAXPATH );
		strcat( strTrain, "\\" );

		getcwd( strNet, MAXPATH );
		strcat( strNet, "\\" );

		strftoc( pstrM, nMChar, strM );
		strftoc( pstrTrain, nTrainChar, &strTrain[strlen(strTrain)] );
		strftoc( pstrNet, nNetChar, &strNet[strlen(strNet)] );

		strcat( strM, "('" );
		strcat( strM, strTrain );
		strcat( strM, "','" );
		strcat( strM, strNet );
		strcat( strM, "')" );

		_bstr_t ret = RunMatlab( strM );

		double mse = 0;
		//parse the mse from the returned string
		char* str_ret = new char[ret.length()+1];
		strcpy( str_ret, ret );

		char* p = strstr( str_ret, "ans =" );
		if( p!=NULL ){
			p += strlen( "ans =" );
			sscanf( p, "%lf", &mse );
		}

		printf( "%s\n", str_ret );
		delete[] str_ret;

		return mse;
	}*/
	void _stdcall LOADNETFROMMATLAB( CMiniNeuralNet*& pNet, char* pstrNet, int nNetChar )
	{
		char strNet[MAXPATH];
		strftoc( pstrNet, nNetChar, strNet );

		LoadMatlabNN( pNet, strNet );
	}
	void _stdcall ASSOCLAYERNETPARAM( NetLayerParam* layerParam, NeuralNetParam* netParam )
	{
		netParam->pLayerParam = layerParam;
		netParam->funNeuro = NeuroTanSig;
	}

//////////////////////////for normalize and unnormalize
	void _stdcall NORMALIZE( NormalizeParam* normParam, REAL* prData, int& n )
	{
		Normalize( normParam, prData, n );
	}
	void _stdcall UNNORMALIZE( NormalizeParam* normParam, REAL* prData, int& n )
	{
		Unnormalize( normParam, prData, n );
	}
	void _stdcall ASSOCNORMALIZEPARAM( REAL* prOrigMin, REAL* prOrigMax, REAL* prNormMin, REAL* prNormMax, NormalizeParam* pNormParam )
	{
		pNormParam->prOrigMin = prOrigMin;
		pNormParam->prOrigMax = prOrigMax;
		pNormParam->prNormMin = prNormMin;
		pNormParam->prNormMax = prNormMax;
	}
}

_bstr_t RunMatlab( char* strMFile )
{
	CoInitialize(NULL);

	_bstr_t ret;
	//ensure the smarter pointer is destroyed before CoUninitialize
	{
		IDispatchPtr spDisp;
		spDisp.CreateInstance( "Matlab.Application.Single" );

		DIMLAppPtr spMatlabApp;
		spDisp.QueryInterface( __uuidof(spDisp), &spMatlabApp );	
		spDisp = NULL;

		spMatlabApp->Execute( _bstr_t("rand(seed, 999)") );
		ret = spMatlabApp->Execute( _bstr_t(strMFile) );
	}

	CoUninitialize();

	return ret;
}

void LoadMatlabNN( CMiniNeuralNet* pNet, char* strWeightFile )
{
	ifstream ifWeight(strWeightFile);
	ZRealMatrix mxWeight( 1, pNet->GetNetWeightSize() );
	ifWeight>>mxWeight;

//	cout<<mxWeight<<endl;

	pNet->LoadWeightFrom( mxWeight );

	//now begin to propogate neural layers
	NetLayerList::iterator iter = pNet->m_lstLayers.begin();
	//skip the input layer
	iter++;

	while( iter!=pNet->m_lstLayers.end() ){
		ZNetLayer* pLayer = *iter++;
		//matrix read from matlab is column major. so change it to the correct matrix
		pLayer->m_pmxWeight->Reshape( pLayer->m_pmxWeight->GetCol(), pLayer->m_pmxWeight->GetRow() );
		pLayer->m_pmxWeight->Verse();

		//matlab assume the bias has input of +1. CMiniNeuralNet assumes it's -1. So change the bias.
		for( int i=1; i<=pLayer->m_pmxWeight->GetRow(); i++ ){
			(*pLayer->m_pmxWeight)(i, pLayer->m_pmxWeight->GetCol()) *= -1;
		}
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