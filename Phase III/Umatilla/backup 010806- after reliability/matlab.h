#ifndef _MATLAB_H_
#define _MATLAB_H_

#define _ATL_CSTRING_EXPLICIT_CONSTRUCTORS	// some CString constructors will be explicit
#include <atlbase.h>
#include <comutil.h>

_bstr_t RunMatlab( char* strMFile );
double RunMatlabNet( const char* pszNetCmd, int* parrNodes, int nLayers, double* parrBias, const char* pszTrain, const char* pszNet, const char* pszValid );

#endif