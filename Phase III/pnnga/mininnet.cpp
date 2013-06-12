// neuranet.cpp: implementation of the CNetLayer class.
//
//////////////////////////////////////////////////////////////////////

#include "mininnet.h"
#include "mathfunc.h"
#include "float.h"
#include "tlhelper.h"

#if _MSC_VER<=1200		//for vc 6.0++, using the old iostream.h
	#include <iomanip.h>
#else					//for higher version, using the iostream
	#include <iomanip>	
	using namespace std;
	#define nocreate in
#endif

#define TRACE	printf

/*#ifdef _DEBUG
#undef THIS_FILE
static char THIS_FILE[]=__FILE__;
#define new DEBUG_NEW
#endif*/

void NeuroSigmoid( ZMatrix* pmx, LPVOID lpParam )
{
	for( int i=1; i<=pmx->GetRow(); i++ ){
		for( int j=1; j<=pmx->GetCol(); j++ ){
			REAL x = (*pmx)(i,j);
			(*pmx)(i,j) = 1.0/(1.0+exp(-x));
		}
	}
}

void NeuroTanSig( ZMatrix* pmx, LPVOID lpParam )
{
	for( int i=1; i<=pmx->GetRow(); i++ ){
		for( int j=1; j<=pmx->GetCol(); j++ ){
			REAL x = (*pmx)(i,j);
			(*pmx)(i,j) = 2.0/(1.0+exp(-2.0*x))-1.0;
		}
	}
}

void RandomMatrix( ZMatrix& mx, REAL rMin, REAL rMax )
{
	for( int i=1; i<=mx.GetRow(); i++ ){
		for( int j=1; j<=mx.GetCol(); j++ ){
			 mx(i,j) = rMin + rand()*(rMax-rMin)/RAND_MAX;
		}
	}
}

REAL GetMatrixNumber( ZMatrix& mx )
{
	REAL rSum = 0;
	for( int i=1; i<=mx.GetRow(); i++ ){
		for( int j=1; j<=mx.GetCol(); j++ ){
			rSum += SQR(mx(i,j));
		}
	}
	return rSum;
}

void PrintMatrix( ZMatrix& mx )
{
	for( int i=1; i<=mx.GetRow(); i++ ){
		for( int j=1; j<mx.GetCol(); j++ ){
			TRACE("%f, ", mx(i,j) );
		}
		TRACE("%f\n", mx(i,j) );
	}
}

typedef REAL (*PCOSTFUNC2)(LPVOID pParam, REAL* prX, int n);
typedef void (*PDIFFFUNC2)(LPVOID pParam, REAL* prX, int n, REAL* prDiff);
#define REAL_MAX	DBL_MAX

//////////////////////////////////////////////////////////////////////
// Construction/Destruction
//////////////////////////////////////////////////////////////////////
CMiniNeuralNet::CNetLayer::CNetLayer( int nInput )
{
	ASSERT( nInput>=1 );
	m_nNeuros = nInput;
	m_pmxWeight = NULL;
	m_pmxOutput = new ZMatrix( m_nNeuros+1, 1 );
}

CMiniNeuralNet::CNetLayer::CNetLayer( int nInput, int nNeuros )
{
	ASSERT( nNeuros>=1 && nInput>=1 );
	m_nNeuros = nNeuros;

	m_pmxWeight = new ZMatrix( nNeuros, nInput+1 );
	m_pmxOutput = new ZMatrix( nNeuros+1, 1 );
}

CMiniNeuralNet::CNetLayer::~CNetLayer()
{
	if( m_pmxWeight )delete m_pmxWeight;
	if( m_pmxOutput )delete m_pmxOutput;
}

void CMiniNeuralNet::CNetLayer::SetOutput( const REAL *prData )
{
	ZMatrix& mx = (*m_pmxOutput);
	mx.CopyBuffer( prData, m_nNeuros );
//	mx.Assign( (REAL*)prData, m_nNeuros );
	mx(mx.GetRow(), 1) = -1;
//	mx(mx.GetRow(), 1) = 1;		//to make it compitable with matlab, this is changed to 1.
}

void CMiniNeuralNet::CNetLayer::GetOutput( REAL *prData )
{
	ZMatrix& mx = (*m_pmxOutput);
	for( int i=0; i<mx.GetRow()-1; i++ ){
		prData[i] = mx(i+1, 1);
	}
}

//////////////////////////////////////////////////////////////////////
// Construction/Destruction
//////////////////////////////////////////////////////////////////////

CMiniNeuralNet::CMiniNeuralNet()
{
	m_rNeuroScaleMin = 0.1;
	m_rNeuroScaleMax = 0.9;
}

CMiniNeuralNet::~CMiniNeuralNet()
{
	while( !m_lstLayers.empty() ){
		CNetLayer* pLayer = m_lstLayers.front();
		m_lstLayers.pop_front();
		delete pLayer;
	}
}


int CMiniNeuralNet::GetNetWeightSize()
{
	CMiniNetLayerList::iterator iter = m_lstLayers.begin();
	iter++;

	int nSize = 0;
	while( iter!=m_lstLayers.end() ){
		CNetLayer* pLayer = *iter++;
		nSize += pLayer->m_pmxWeight->GetSize();;
	}
	return nSize;
}

void CMiniNeuralNet::CopyWeightTo( ZMatrix& mxWeight )
{
	ASSERT( mxWeight.GetRow()==1 && mxWeight.GetCol()==GetNetWeightSize() );
	CMiniNetLayerList::iterator iter = m_lstLayers.begin();
	iter++;

	int nCol = 1;
	while( iter!=m_lstLayers.end() ){
		CNetLayer* pLayer = *iter++;
		for( int i=1; i<=pLayer->m_pmxWeight->GetRow(); i++ ){
			for( int j=1; j<=pLayer->m_pmxWeight->GetCol(); j++ ){
				mxWeight(1,nCol++) = (*pLayer->m_pmxWeight)(i,j);
			}
		}
	}
	ASSERT( nCol == mxWeight.GetCol()+1 );
}

void CMiniNeuralNet::CopyWeightFrom( const ZMatrix& mxWeight )
{
	ASSERT( mxWeight.GetRow()==1 && mxWeight.GetCol()==GetNetWeightSize() );
	CMiniNetLayerList::iterator iter = m_lstLayers.begin();
	iter++;

	int nCol = 1;
	while( iter!=m_lstLayers.end() ){
		CNetLayer* pLayer = *iter++;
		for( int i=1; i<=pLayer->m_pmxWeight->GetRow(); i++ ){
			for( int j=1; j<=pLayer->m_pmxWeight->GetCol(); j++ ){
				(*pLayer->m_pmxWeight)(i,j)=mxWeight(1,nCol++);
			}
		}
	}
	ASSERT( nCol == mxWeight.GetCol()+1 );
}


//pnNeuros the integer array of how many neuros in each layer. (incluing hidden layers and input layer).
//nLayers is the number of hidden layers + input layer.
void CMiniNeuralNet::Create( int *pnNeuros, int nLayers, NEUROFUN funNeuro )
{
	ASSERT( pnNeuros!=NULL );
	ASSERT( nLayers>1 );

	m_pFunNeuro = funNeuro;
	if( m_pFunNeuro==NULL ){
		m_pFunNeuro = NeuroSigmoid;
	}

	m_lstLayers.push_back( new CNetLayer(pnNeuros[0]) );
	int nOutput = pnNeuros[0];
	for( int i=1; i<nLayers; i++ ){
		m_lstLayers.push_back( new CNetLayer( nOutput, pnNeuros[i] ) );
		nOutput = pnNeuros[i];
	}
}

void CMiniNeuralNet::LoadMatlabNet( char* strWeightFile )
{
	ASSERT( GetLayerCount()>=2 );

	ifstream ifWeight(strWeightFile);
	ZRealMatrix mxWeight( 1, GetNetWeightSize() );
	ifWeight>>mxWeight;

//	cout<<mxWeight<<endl;

	CopyWeightFrom( mxWeight );

	//now begin to propogate neural layers
	CMiniNetLayerList::iterator iter = m_lstLayers.begin();
	//skip the input layer
	iter++;

	while( iter!=m_lstLayers.end() ){
		CNetLayer* pLayer = *iter++;
		//matrix read from matlab is column major. so change it to the correct matrix
		pLayer->m_pmxWeight->Reshape( pLayer->m_pmxWeight->GetCol(), pLayer->m_pmxWeight->GetRow() );
		pLayer->m_pmxWeight->Verse();

		//matlab assume the bias has input of +1. ZNeuralNet assumes it's -1. So change the bias.
		for( int i=1; i<=pLayer->m_pmxWeight->GetRow(); i++ ){
			(*pLayer->m_pmxWeight)(i, pLayer->m_pmxWeight->GetCol()) *= -1;
		}
	}
}

int CMiniNeuralNet::GetLayerCount()
{
	return (int)m_lstLayers.size();
}

int CMiniNeuralNet::GetHiddenCount()
{
	return GetLayerCount()-1;
}

CMiniNeuralNet::CNetLayer* CMiniNeuralNet::GetInputLayer()
{
	return m_lstLayers.front();
}

CMiniNeuralNet::CNetLayer* CMiniNeuralNet::GetOutputLayer()
{
	return m_lstLayers.back();
}

int CMiniNeuralNet::GetInputCount()
{
	ASSERT( GetLayerCount()>=2 );
	return GetInputLayer()->m_nNeuros;
}

int CMiniNeuralNet::GetOutputCount()
{
	ASSERT( GetLayerCount()>=2 );
	return GetOutputLayer()->m_nNeuros;
}

void CMiniNeuralNet::SetInput( const REAL *prInput )
{
	ASSERT( GetLayerCount()>=2 );

	GetInputLayer()->SetOutput( prInput );
}

void CMiniNeuralNet::GetInput( REAL *prInput )
{
	ASSERT( GetLayerCount()>=2 );

	GetInputLayer()->GetOutput( prInput );
}

void CMiniNeuralNet::GetOutput( REAL *prOutput )
{
	ASSERT( GetLayerCount()>=2 );

	GetOutputLayer()->GetOutput( prOutput );
}

void CMiniNeuralNet::ForwardPropagate( REAL* prInput )
{
	ASSERT( GetLayerCount()>=2 );

	if( prInput!=NULL ){
		SetInput( prInput );
	}

	//now begin to propogate neural layers
	CMiniNetLayerList::iterator iter = m_lstLayers.begin();
	//skip the input layer
	iter++;

	ZMatrix* pmxInput = GetInputLayer()->m_pmxOutput;
	CNetLayer* pLayer = NULL;
	while( iter!=m_lstLayers.end() ){
		pLayer = *iter++;
		ZMatrix mx;
		mx = (*pLayer->m_pmxWeight)*(*pmxInput);
		(*m_pFunNeuro)( &mx, NULL );
		pLayer->SetOutput( mx.GetMatrixData() );
		pmxInput = pLayer->m_pmxOutput;
	}		
}

void CMiniNeuralNet::Simulate( const REAL* prInput, REAL* prOutput )
{
	ASSERT( prInput!=NULL && prOutput!=NULL );

	ForwardPropagate( (REAL*)prInput );
	if( prOutput!=NULL ){
		GetOutput( prOutput );
	}
}

REAL CMiniNeuralNet::GetMeanSquareError( REAL* prInput, REAL* prOutput, int nPattern )
{
	//calculate the error function under the current weight
	REAL rError = 0;
	for( int i=0; i<nPattern; i++ ){
		ForwardPropagate( prInput+i*GetInputCount() );
		CNetLayer* pLayer = GetOutputLayer();
		for( int k=0; k<GetOutputCount(); k++ ){
			rError += SQR( (*pLayer->m_pmxOutput)(k+1, 1) - (prOutput+i*GetOutputCount())[k] )/GetOutputCount();
		}
	}
	return rError/nPattern;
}

REAL CMiniNeuralNet::GetRootMeanSquareError( REAL* prInput, REAL* prOutput, int nPattern )
{
	//calculate the error function under the current weight
	return sqrt( GetMeanSquareError(prInput, prOutput, nPattern) );
}

void CMiniNeuralNet::SetNeuroScaler( REAL rMin, REAL rMax )
{
	ASSERT( rMin<rMax );
	m_rNeuroScaleMin = rMin;
	m_rNeuroScaleMax = rMax;
}

void CMiniNeuralNet::GetNeuroScaler( REAL& rMin, REAL& rMax )
{
	rMin = m_rNeuroScaleMin;
	rMax = m_rNeuroScaleMax;
}

void CMiniNeuralNet::ScaleIn( REAL* prInput, int nCount, REAL rMin, REAL rMax )
{
	ASSERT( rMin<rMax );
	REAL r = (m_rNeuroScaleMax-m_rNeuroScaleMin)/(rMax-rMin);
	for( int i=0; i<nCount; i++ ){
		prInput[i] = (prInput[i]-rMin)*r + m_rNeuroScaleMin;
	}
}

void CMiniNeuralNet::ScaleOut( REAL* prOutput, int nCount, REAL rMin, REAL rMax )
{
	ASSERT( rMin<rMax );
	REAL r = (rMax-rMin)/(m_rNeuroScaleMax-m_rNeuroScaleMin);
	for( int i=0; i<nCount; i++ ){
		prOutput[i] = (prOutput[i]-m_rNeuroScaleMin)*r + rMin;
	}
}

void CMiniNeuralNet::TraceNetwork()
{
	CMiniNetLayerList::iterator iter = m_lstLayers.begin();

	int i = 0;
	while( iter!=m_lstLayers.end() ){
		CNetLayer* pLayer = *iter++;
		TRACE( "%d Layer Weight\n", i++ );
		if( pLayer->m_pmxWeight )PrintMatrix( *pLayer->m_pmxWeight );
	}
}

void CMiniNeuralNet::SaveToFile( ofstream& ofg )
{
	ofg<<GetLayerCount()<<endl;
	
	CMiniNetLayerList::iterator iter = m_lstLayers.begin();
	while( iter!=m_lstLayers.end() ){
		CNetLayer* pLayer = *iter++;
		ofg<<pLayer->m_nNeuros<<"\t";
	}
	ofg<<endl;

	ofg<<setprecision(10);
	iter = m_lstLayers.begin();
	iter++;

	while( iter!=m_lstLayers.end() ){
		CNetLayer* pLayer = *iter++;
		ofg<<(*pLayer->m_pmxWeight);
	}
}

void CMiniNeuralNet::SaveToFile( char* strNetFile )
{
	ofstream ofNN(strNetFile);
	SaveToFile( ofNN );
}

void CMiniNeuralNet::CreateFromFile( ifstream& ifg )
{
	int nLayerCount;
	ifg>>nLayerCount;
	int * pnNeuros = new int[nLayerCount];
	for( int i=0; i<nLayerCount; i++ ){
		ifg>>pnNeuros[i];
	}
	Create( pnNeuros, nLayerCount, NeuroSigmoid);

	delete[] pnNeuros;

	CMiniNetLayerList::iterator iter = m_lstLayers.begin();
	iter++;

	while( iter!=m_lstLayers.end() ){
		CNetLayer* pLayer = *iter++;
		ifg>>(*pLayer->m_pmxWeight);
	}
}

void CMiniNeuralNet::CreateFromFile( char* strNetFile )
{
	ifstream ifNN(strNetFile);
	CreateFromFile( ifNN );
}