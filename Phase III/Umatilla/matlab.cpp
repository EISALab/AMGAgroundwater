#include "netbase.h"
#include "netstd.h"
#include <string>
using namespace std;

#include "matlab.h"

#import "c:\matlabr12\bin\win32\mlapp.tlb" no_namespace
#define SET_DEFAULT_PARAM(t) {(t).vt=VT_ERROR; (t).scode=DISP_E_PARAMNOTFOUND;}

#define MAXPATH	512

double RunMatlabNet( const char* pszNetCmd, int* parrNodes, int nLayers, double* parrBias, const char* pszTrain, const char* pszNet, const char* pszValid )
{
	char strNodes[MAXPATH], strBias[MAXPATH];

	//translate the nodes into string parameters
	sprintf( strNodes, "[%d", parrNodes[0] );
	for( int i=1; i<nLayers; i++ ){
		sprintf( strNodes, "%s,%d", strNodes, parrNodes[i] );
	}
	strcat( strNodes, "]" );

	//translate the bias into string parameters.
	sprintf( strBias, "[%g", parrBias[0] );
	for( i=1; i<parrNodes[nLayers-1]+1; i++ ){
		sprintf( strBias, "%s,%g", strBias, parrBias[i] );
	}
	strcat( strBias, "]" );

	FILE* pf = fopen( "netcmd.txt", "w" );
	fprintf( pf, "cd neutrain\n" );
	fprintf( pf, "put %s %s\n", pszTrain, pszValid );
	fprintf( pf, "run matlabnn %s %s %s %s %s mse.txt %s\n", pszNetCmd, strNodes, strBias, pszTrain, pszNet, pszValid );
	fprintf( pf, "get %s mse.txt\n", pszNet );
	fclose( pf );

	char cmd[MAX_PATH];
	sprintf( cmd, "ncptln %s matlab netcmd.txt", GetLocalName().c_str() );
	system( cmd );
//	system( "ncptln cee-zzvo matlab netcmd.txt" );

	double mse = 0;
	pf = fopen( "mse.txt", "r" );
	fscanf( pf, "%lf", &mse );
	fclose( pf );

	return mse;
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
