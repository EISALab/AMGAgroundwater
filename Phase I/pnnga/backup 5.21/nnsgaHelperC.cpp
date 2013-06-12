#include "nnsgaHelperC.h"
#include "D:\\personal\\smyan\\Projects\\VC Projects\\bpnnet\\neuranet.h"
//#include "neuranet.h"

#define _ATL_CSTRING_EXPLICIT_CONSTRUCTORS	// some CString constructors will be explicit
#include <atlbase.h>

#define MAXPATH 256

#import "c:\matlabr12\bin\win32\mlapp.tlb" no_namespace
#define SET_DEFAULT_PARAM(t) {(t).vt=VT_ERROR; (t).scode=DISP_E_PARAMNOTFOUND;}

static _bstr_t RunMatlab( char* strMFile );
static void LoadMatlabNN( ZNeuralNet* pNet, char* strWeightFile );

//normParam.nLen is the vector length of each touple, which is the same length of prMin and prMax
//n is the number of touples that will be normalized.
//prData points to the data, the touples should be stored one by one sequentially.
static void Normalize( NormalizeParam* pNormParam, REAL* prData, int n );
static void Unnormalize( NormalizeParam* pNormParam, REAL* prData, int n );

//int g_nChromLen=0;

class CChromItem
{
public:
	int* m_pChrom;
	int m_nChromLen;
	double* m_prVars;
	int m_nVars;
	CChromItem( int* pChrom, int nChromLen, double* prVars, int nVars ){
		m_pChrom = new int[nChromLen];
		m_nChromLen = nChromLen;
		m_prVars = new double[nVars];
		m_nVars = nVars;
		memcpy( m_pChrom, pChrom, nChromLen*sizeof(int) );
		memcpy( m_prVars, prVars, nVars*sizeof(double) );
	}
	~CChromItem(){
		delete[] m_pChrom;
		delete[] m_prVars;
	}
	int Compare( const CChromItem* pItem )const {
		int nRet = 0;
		for( int i=0; i<m_nChromLen; i++ ){
			nRet = m_pChrom[i]-pItem->m_pChrom[i];
			if( nRet!=0 )return nRet;
		}
		return nRet;
	}
	int Compare( int* pChrom )const {
		int nRet = 0;
		for( int i=0; i<m_nChromLen; i++ ){
			nRet = m_pChrom[i]-pChrom[i];
			if( nRet!=0 )return nRet;
		}
		return nRet;
	}
};

class CChromItemTraits : public CElementTraits<CChromItem*, int*>
{
public:
	static int CompareElements( const CChromItem*& pItem1, const CChromItem*& pItem2 )
	{
		return pItem1->Compare( pItem2 );
	}
	static int CompareToKey( const CChromItem*& pItem, int* pChrom )
	{
		return pItem->Compare( pChrom );
	}
};

//a helper function to free the AVL tree
BOOL FreeChromItem( CChromItem*& pItem, LPVOID)
{
	delete pItem;
	return TRUE;
}

CAvlTree<CChromItem*, CChromItem*, int*, CChromItemTraits> g_ChromTree;
typedef CAvlTree<CChromItem*, CChromItem*, int*, CChromItemTraits> CAvlChromTree;

extern "C"{
	int _stdcall CREATECHROMCACHE()
	{
		CAvlChromTree* pAvlChroms = new CAvlChromTree();
		return (int)pAvlChroms;
	}
	void _stdcall INSERTCHROMCACHE( CAvlChromTree*& pAvlChroms, int* pChrom, int* pnChromLen, double* prFits, int* pnFits )
	{
		CChromItem* pItem = new CChromItem( pChrom, *pnChromLen, prFits, *pnFits );
		pAvlChroms->Insert( pItem );
	}
	int _stdcall SEARCHCHROMCACHE( CAvlChromTree*& pAvlChroms, int* pChrom, int* pnChromLen, double* prFits, int* pnFits )
	{
		POSITION pos = pAvlChroms->SearchByKey( pChrom );
		if( pos!=0 ){
			memcpy( prFits, pAvlChroms->GetAt(pos)->m_prVars, (*pnFits)*sizeof(double) );
			return TRUE;
		}
		return FALSE;
	}
	void _stdcall RELEASECHROMCACHE( CAvlChromTree*& pAvlChroms )
	{
		pAvlChroms->VisitTree( FreeChromItem );
		pAvlChroms->RemoveAll();
		delete pAvlChroms;
	}


//the old cache management subroutines are obsolete now, Shengquan Yan 10/08/03
/*	void _stdcall INITCHROMCACHE( int* pnChromLen )
	{
		g_ChromTree.VisitTree( FreeChromItem );
		g_ChromTree.RemoveAll();
	}
	void _stdcall INSERTCHROMCACHE( int* pChrom, int* pnChromLen, double* prFitness )
	{
		CChromItem* pItem = new CChromItem( pChrom, *pnChromLen, *prFitness );
		g_ChromTree.Insert( pItem );
	}
	int _stdcall SEARCHCHROMCACHE( int* pChrom, int* pnChromLen, double* prFitness )
	{
		POSITION pos = g_ChromTree.SearchByKey( pChrom );
		if( pos!=0 ){
			*prFitness = g_ChromTree.GetAt(pos)->m_rFitness;
			return TRUE;
		}
		return FALSE;
	}
	void _stdcall RELEASECHROMCACHE()
	{
		g_ChromTree.VisitTree( FreeChromItem );
		g_ChromTree.RemoveAll();
	}*/

	int _stdcall CREATENET( NeuralNetParam* netParam )
	{
		ZNeuralNet* pNet = new ZNeuralNet();
		pNet->Create( netParam );

		//conver pointer to handle
		return (int)pNet;
	}
	
	void _stdcall SIMNET( ZNeuralNet*& pNet, REAL* prInput, REAL* prOutput )
	{
		pNet->Predict( prInput, prOutput );
	}

	double _stdcall TESTNET( ZNeuralNet*& pNet, REAL* prInput, REAL* prOutput, int& nPattern )
	{
		return pNet->TestPatternError( prInput, prOutput, nPattern );
	}

	void _stdcall RELEASENET( ZNeuralNet*& pNet )
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
	void _stdcall LOADNETFROMMATLAB( ZNeuralNet*& pNet, char* pstrNet, int nNetChar )
	{
		char strNet[MAXPATH];
		strftoc( pstrNet, nNetChar, strNet );

		LoadMatlabNN( pNet, strNet );
	}
	void _stdcall ASSOCLAYERNETPARAM( NetLayerParam* layerParam, NeuralNetParam* netParam )
	{
		netParam->pLayerParam = layerParam;
		netParam->funNeuro = NeuroTanSig;
		netParam->funDNeuro = NeuroDTanSig;
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

void LoadMatlabNN( ZNeuralNet* pNet, char* strWeightFile )
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

		//matlab assume the bias has input of +1. ZNeuralNet assumes it's -1. So change the bias.
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