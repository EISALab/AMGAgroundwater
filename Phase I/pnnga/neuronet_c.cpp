#include "nnsgaHelperC.h"
#include "mininnet.h"

#define _ATL_CSTRING_EXPLICIT_CONSTRUCTORS	// some CString constructors will be explicit
#include <atlbase.h>

#define MAXPATH 256

#import "c:\matlabr12\bin\win32\mlapp.tlb" no_namespace
#define SET_DEFAULT_PARAM(t) {(t).vt=VT_ERROR; (t).scode=DISP_E_PARAMNOTFOUND;}

static _bstr_t RunMatlab( char* strMFile );

extern "C"{
	int _stdcall CREATENET( int* pnNeuros, int* pnLayers )
	{
		CMiniNeuralNet* pNet = new CMiniNeuralNet();
		pNet->Create( pnNeuros, *pnLayers, NeuroTanSig );

		//conver pointer to handle
		return (int)pNet;
	}
	
	void _stdcall SIMNET( CMiniNeuralNet*& pNet, REAL* prInput, REAL* prOutput )
	{
		pNet->Simulate( prInput, prOutput );
	}

	double _stdcall TESTNET( CMiniNeuralNet*& pNet, REAL* prInput, REAL* prOutput, int& nPattern )
	{
		return pNet->GetMeanSquareError( prInput, prOutput, nPattern );
	}

	void _stdcall RELEASENET( CMiniNeuralNet*& pNet )
	{
		delete pNet;
	}

	void _stdcall LOADMATLABNET( CMiniNeuralNet*& pNet, char* pstrNetFile, int nNetChar )
	{
		char strNetFile[MAXPATH];
		strftoc( pstrNetFile, nNetChar, strNetFile );

		pNet->LoadMatlabNet( strNetFile );
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
	double _stdcall TRAINMATLABNETEX( char* pstrM, int nMChar, int* pnNodes, int& nLayers, double* prNetBias, char* pstrTrain, int nTrainChar, char* pstrNet, int nNetChar, char* pstrValid, int nValidChar )
	{
		char strNodes[MAXPATH], strM[MAXPATH], strBias[MAXPATH], strTrain[MAXPATH], strNet[MAXPATH], strValid[MAXPATH];
		strftoc( pstrM, nMChar, strM );
		strftoc( pstrTrain, nTrainChar, strTrain );
		strftoc( pstrNet, nNetChar, strNet );
		strftoc( pstrValid, nValidChar, strValid );

		sprintf( strNodes, "[%d", pnNodes[0] );
		for( int i=1; i<nLayers; i++ ){
			sprintf( strNodes, "%s,%d", strNodes, pnNodes[i] );
		}
		strcat( strNodes, "]" );

		sprintf( strBias, "[%g", prNetBias[0] );
		for( i=1; i<pnNodes[nLayers-1]+1; i++ ){
			sprintf( strBias, "%s,%g", strBias, prNetBias[i] );
		}
		strcat( strBias, "]" );

		FILE* pf = fopen( "netcmd.txt", "w" );
		fprintf( pf, "cd neutrain\n" );
		fprintf( pf, "put %s %s\n", strTrain, strValid );
		fprintf( pf, "run matlabnn %s %s %s %s %s mse.txt %s\n", strM, strNodes, strBias, strTrain, strNet, strValid );
		fprintf( pf, "get %s mse.txt\n", strNet );
		fclose( pf );

		system( "ncptln cee-zzqr matlab netcmd.txt" );

		double mse = 0;
		pf = fopen( "mse.txt", "r" );
		fscanf( pf, "%lf", &mse );
		fclose( pf );

		return mse;
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
