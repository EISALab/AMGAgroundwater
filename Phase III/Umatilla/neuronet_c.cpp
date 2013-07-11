#include "nnsgaHelperC.h"
#include "mininnet.h"

#include <list>
#include <algorithm>
#include <vector>
#include <iterator>
#include <numeric>
#include "matlab.h"
#include "ofdump.h"

//#if _MSC_VER<=1200		//for vc 6.0++, using the old iostream.h
//	#include <fstream.h>
//	#include <iostream.h>
//	#include <strstrea.h>
//#else					//for higher version, using the iostream
	#include <fstream>
	#include <iostream>
	#include <strstream>
//#endif

using namespace std;

#define MAXPATH 256
#define MAXLINE 512


using namespace std;

typedef pair<int, REAL> dist_pair;
typedef vector< dist_pair > dist_pair_vector;

#define TRAIN_FILE	"train.dat"
#define VALID_FILE	"valid.dat"
#define NET_FILE	"net.dat"
#define NET_CMD		"univnet"

REAL uni_rand()
{
	return (REAL)(rand())/RAND_MAX;
}

template< typename T >
class op_delptr
{
public:
	void operator() (T* ptr)
	{ delete ptr; }
};

class CSphereRegion
{
public:
	vector<REAL> m_ptCenter;
	vector<REAL> m_Radius;
public:
	CSphereRegion(){};
	CSphereRegion( vector<REAL>& ptCen, vector<REAL>& radius ){
		SetRegion( ptCen, radius );
	}
	void SetRegion( vector<REAL>& ptCen, vector<REAL>& radius ){
		int ndim = ptCen.size();
		m_ptCenter.resize( ndim );
		m_Radius.resize( ndim );
		m_ptCenter.assign( ptCen.begin(), ptCen.end() );
		m_Radius.assign( radius.begin(), radius.end() );
	}
	bool PtInRegion( vector<REAL>& pt )
	{
		ASSERT( pt.size()==m_ptCenter.size() && pt.size()==m_Radius.size() );

		int ndim = pt.size();
		for( int i=0; i<ndim; i++ ){
			if( fabs(pt[i]-m_ptCenter[i])>m_Radius[i] )return false;
		}
		return true;
	}
	double GetAvgRadius(){
		return accumulate( m_Radius.begin(), m_Radius.end(), 0.0 ) / m_Radius.size();
	}
};

class CNeuralNetNode
{
	CSphereRegion m_TrustRegion;
	CMiniNeuralNet m_NeuralNet;
public:

	CMiniNeuralNet* GetNeuralNet(){
		return &m_NeuralNet;
	}
	CSphereRegion* GetTrustRegion(){
		return &m_TrustRegion;
	}
};

class op_not_trust_node
{
	vector<REAL>& m_pt;
public:
	op_not_trust_node( vector<REAL>& pt ) : m_pt(pt){};
	bool operator()( CNeuralNetNode* pNode ){
		return !pNode->GetTrustRegion()->PtInRegion(m_pt);
	}
};
class op_region_compare
{
public:
	bool operator()( CNeuralNetNode* pNode1, CNeuralNetNode* pNode2 ){
		return pNode1->GetTrustRegion()->GetAvgRadius() - pNode2->GetTrustRegion()->GetAvgRadius() < 0 ; 
	}
};

class op_dist_less : public binary_function<dist_pair, dist_pair, bool>
{
public:
	result_type operator()( const first_argument_type& d1, const second_argument_type& d2 )const
	{
		return d1.second < d2.second;
	}
};

template <class Type> class diff_sqr: 
binary_function<Type, Type, Type> 
{
public:
   result_type operator( ) ( first_argument_type a, 
                 second_argument_type b )
   {
      return (result_type) ( ( a - b )*( a - b ) );
   }
};

class CNeuralNetSet
{
private:
	list<CNeuralNetNode*> m_NetSet;

	void Release()
	{
		for_each( m_NetSet.begin(), m_NetSet.end(), op_delptr<CNeuralNetNode>() );
		m_NetSet.clear();
	}

	void FindTrustNet( REAL* prInput, vector<CNeuralNetNode*>& hitNets ){
		ASSERT( !m_NetSet.empty() );
		int nInputs = m_NetSet.front()->GetNeuralNet()->GetInputCount();
		vector<REAL> vcInput( prInput, prInput+nInputs );

		remove_copy_if( ++m_NetSet.begin(), m_NetSet.end(), back_inserter(hitNets), op_not_trust_node(vcInput) );

		//the first neural net is the global prediction, always available.
		hitNets.push_back( m_NetSet.front() );
	}

	void FindBestNet( REAL* prInput, vector<CNeuralNetNode*>& hitNets ){
		FindTrustNet( prInput, hitNets );

		vector<CNeuralNetNode*>::iterator pos = min_element( hitNets.begin(), hitNets.end(), op_region_compare() );
		ASSERT( pos!=hitNets.end() );

		CNeuralNetNode* pNode = *pos;
		hitNets.clear();

		hitNets.push_back( pNode );
	}

public:
	CNeuralNetSet(){};
	~CNeuralNetSet(){ Release(); }

	int GetInputCount(){
		ASSERT( !m_NetSet.empty() );
		return m_NetSet.front()->GetNeuralNet()->GetInputCount();
	}
	int GetOutputCount(){
		ASSERT( !m_NetSet.empty() );
		return m_NetSet.front()->GetNeuralNet()->GetOutputCount();
	}
	void SimNet( REAL* prInput, REAL* prOutput ){
		int nOutputs = GetOutputCount();

		vector<CNeuralNetNode*>hitNets;
//		FindTrustNet( prInput, hitNets );
		FindBestNet( prInput, hitNets );

		int nNets = hitNets.size();
		vector< vector<REAL> > vcOutputs(nNets);
		for( int i=0; i<nNets; i++ ){
			vcOutputs[i].resize(nOutputs);
			hitNets[i]->GetNeuralNet()->Simulate( prInput, &vcOutputs[i][0] );
		}
		vector< REAL > vcSum( nOutputs, 0.0 );
		REAL rSumInvRadius = 0;
		for( i=0; i<nNets; i++ ){
			REAL rInvRadius = 1.0 / hitNets[i]->GetTrustRegion()->GetAvgRadius();
			for( int j=0; j<nOutputs; j++ ){
				vcSum[j] += vcOutputs[i][j]*rInvRadius;
			}
			rSumInvRadius += rInvRadius;
		}

		transform( vcSum.begin(), vcSum.end(), vcSum.begin(), bind2nd(divides<REAL>(), rSumInvRadius) );
		copy( vcSum.begin(), vcSum.end(), prOutput );

		cdump<<"pred..."<<nNets<<" nets\t";
	}
	void ReadTrainingData( char* strDataFile, int nInputs, int nOutputs, vector< vector<REAL> >& vcInputs, vector< vector<REAL> >& vcOutputs )
	{
		ifstream ifg(strDataFile);
		char buf[MAXLINE];
		while( true ){
			ifg.getline( buf, ELEMENTS(buf) );
			if( ifg.fail() || ifg.eof() )break;

			istrstream istr(buf);
			vector<REAL> vci(nInputs);
			for( int i=0; i<nInputs; i++ )istr>>vci[i];

			vector<REAL> vco(nOutputs);
			for( i=0; i<nOutputs; i++ )istr>>vco[i];

			vcInputs.push_back( vector<REAL>() );
			vcInputs.back().swap( vci );

			vcOutputs.push_back( vector<REAL>() );
			vcOutputs.back().swap( vco );
		}
	}
	void ComputeDistance( vector< vector<REAL> >& vcInputs, vector<REAL>& vcPtCen, dist_pair_vector& vcDists )
	{
		int nsize = vcInputs.size();
		vcDists.clear();
		vcDists.reserve( nsize );
		for( int i=0; i<nsize; i++ ){
			REAL dist = inner_product( vcInputs[i].begin(), vcInputs[i].end(), vcPtCen.begin(), 0.0,
				plus<REAL>(), diff_sqr<REAL>() );
			dist = sqrt(dist);
			vcDists.push_back( dist_pair(i, dist) );
		}
		sort( vcDists.begin(), vcDists.end(), op_dist_less() );
	}

	//vcDists: sorted pair<id, dist>. 
	void SubdividTrainingData( dist_pair_vector& vcDists, vector< vector<int> >& vcIdSet, int min_pts )
	{
		//step 1. find the media's id.
		REAL rMaxDist = vcDists[ vcDists.size()-1 ].second;
		REAL rMinDist = vcDists[ 0 ].second;

		dist_pair_vector::iterator pos = find_if( vcDists.begin(), vcDists.end(), bind1st(op_dist_less(), make_pair(0, (rMaxDist+rMinDist)/2.0)) );
		if( pos>vcDists.end()-min_pts ){
			pos = max( vcDists.begin(), vcDists.end()-min_pts );
		}
		if( vcDists.end() - pos > min_pts ) pos = vcDists.end() - min_pts;

		//check how many points can be sampled.
		int nFirstHalfPts = pos - vcDists.begin();
		int nSecndHalfPts = vcDists.size() - nFirstHalfPts;
		if( 2*nSecndHalfPts > vcDists.size() || nFirstHalfPts<min_pts){
			//there are not enough points to support the next subdivition, stop it.
			vector<int> vcIds;
			vcIds.resize( vcDists.size() );
			for( int i=0; i<vcDists.size(); i++ )vcIds[i] = vcDists[i].first;
			vcIdSet.push_back( vcIds );
		}else{
			//save the first half dist pairs
			dist_pair_vector vcTmpDists( vcDists.begin(), pos );

			//will sample from the first half ids.
			vector<int> vcTmpIds( nFirstHalfPts );
			for( int i=0 ; i<nFirstHalfPts; i++ )vcTmpIds[i] = vcDists[i].first;

			//create the sampled vector.
			vector< int > vcIds;
			vcIds.reserve( 2*nSecndHalfPts );
			vcIds.resize( nSecndHalfPts );
			for( i=0; i<nSecndHalfPts; i++ )vcIds[i] = (*pos++).first;
			ASSERT( vcIds.size()==nSecndHalfPts );

			random_shuffle( vcTmpIds.begin(), vcTmpIds.end() );
			copy( vcTmpIds.begin(), vcTmpIds.begin()+nSecndHalfPts, back_inserter(vcIds) );
			vcIdSet.push_back( vcIds );

			//need a subdivition.
			SubdividTrainingData( vcTmpDists, vcIdSet, min_pts );
		}
	}


	void WriteTraingFile( char* strTrain, char* strValid, vector< vector<REAL> >& vcInputs, vector< vector<REAL> >& vcOutputs, vector<int>& vcIds, REAL rValidRatio )
	{
		std::ofstream ofgTrain( strTrain );
		std::ofstream ofgValid( strValid );

		size_t nsize = vcIds.size();
		int id;
		for( int i=0; i<nsize; i++ ){
			ofstream* pofg = NULL;
			if( uni_rand()<rValidRatio )pofg = &ofgValid;
			else pofg = &ofgTrain;
			
			id = vcIds[i];
			copy( vcInputs[id].begin(), vcInputs[id].end(), ostream_iterator<REAL>(*pofg, "\t") );
			copy( vcOutputs[id].begin(), vcOutputs[id].end(), ostream_iterator<REAL>(*pofg, "\t") );
			*pofg << endl;
		}
	}

	void ComputeClusterSphere( vector< vector<REAL> >& vcInputs, vector<int>& vcIds, CSphereRegion* pRegion )
	{
		int nDim = vcInputs[0].size();
		int nCount = vcIds.size();
		vector<REAL> vcPtMean(nDim, 0.0);

		//compute the mean of each input variable
		for( int i=0; i<nCount; i++ ){
			for( int j=0; j<nDim; j++ ){
				vcPtMean[j] += vcInputs[vcIds[i]][j];
			}
		}
		transform( vcPtMean.begin(), vcPtMean.end(), vcPtMean.begin(), bind2nd( divides<REAL>(), nCount ) );

		//compute the standard deviation.
		vector<REAL> vcStd(nDim, 0.0);
		for( i=0; i<nCount; i++ ){
			for( int j=0; j<nDim; j++ ){
				vcStd[j] += diff_sqr<REAL>()(vcInputs[vcIds[i]][j], vcPtMean[j]);
			}
		}
		transform( vcStd.begin(), vcStd.end(), vcStd.begin(), bind2nd( divides<REAL>(), nCount ) );
		transform( vcStd.begin(), vcStd.end(), vcStd.begin(), sqrt );
//		transform( vcStd.begin(), vcStd.end(), vcStd.begin(), bind2nd( multiplies<REAL>(), 2.5 ) );

		pRegion->SetRegion( vcPtMean, vcStd );
	}

			
	void TrainOnDataSet( char* strDataFile, REAL* prPtCen, int* parrNodes, int nLayers, double* parrBias, int nMinPts )
	{
		int nInputs = parrNodes[0];
		int nOutputs = parrNodes[nLayers-1];

		vector< vector<REAL> > vcInputs;
		vector< vector<REAL> > vcOutputs;
		vector< REAL > vcPtCen( prPtCen, prPtCen+nInputs );
		dist_pair_vector vcDists;
		vector< vector<int> > vcIdSet;

		Release();

		//read all the data into vcInputs and vcOutputs.
		ReadTrainingData( strDataFile, nInputs, nOutputs, vcInputs, vcOutputs );

		//compute the distance to the center point.
		ComputeDistance( vcInputs, vcPtCen, vcDists );

		//subdivid the training data into clusters.
		SubdividTrainingData( vcDists, vcIdSet, nMinPts );

		//create the training set for each hierarch.
		for( int i=0; i<vcIdSet.size(); i++ ){
			//write a training set to files and run the matlab trainer
			WriteTraingFile( TRAIN_FILE, VALID_FILE, vcInputs, vcOutputs, vcIdSet[i], 0.2 );
			RunMatlabNet( NET_CMD, parrNodes, nLayers, parrBias, TRAIN_FILE, NET_FILE, VALID_FILE );
			//OK, now I have the network weights in NET_FILE, create a neural network wrapping it.
			CNeuralNetNode* pNode = new CNeuralNetNode();
			pNode->GetNeuralNet()->Create( parrNodes, nLayers, NeuroTanSig );
			pNode->GetNeuralNet()->LoadMatlabNet( NET_FILE );
			ComputeClusterSphere( vcInputs, vcIdSet[i], pNode->GetTrustRegion() );

			m_NetSet.push_back( pNode );
		}
		cdump<<"training finsihed:"<<vcIdSet.size()<<" nets were trained!"<<endl;
	}
};

extern "C"{
	int _stdcall CREATE_NET_CLUSTER()
	{
		CNeuralNetSet* pNets = new CNeuralNetSet();
		return (int)pNets;
	}

	void _stdcall RELEASE_NET_CLUSTER( CNeuralNetSet*& pNets )
	{
		delete pNets;
	}

	void _stdcall TRAIN_NET_CLUSTER( CNeuralNetSet*& pNets, char* pstrDataFile, int nChars, double* prPtCen, int* pnNodes, int& nLayers, double* prNetBias, int& nMinPts )
	{
		char strTrainData[MAXPATH];
		strftoc( pstrDataFile, nChars, strTrainData );

		pNets->TrainOnDataSet( strTrainData, prPtCen, pnNodes, nLayers, prNetBias, nMinPts );
	}

	void _stdcall SIM_NET_CLUSTER( CNeuralNetSet*& pNets, REAL* prInput, REAL* prOutput )
	{
		pNets->SimNet( prInput, prOutput );
	}

	double _stdcall TEST_NET_CLUSTER( CNeuralNetSet*& pNets, REAL* prInput, REAL* prOutput, int& nPattern )
	{
		int nInputs = pNets->GetInputCount();
		int nOutputs = pNets->GetOutputCount();
		vector<REAL> vcOutput(nOutputs);

		REAL rError = 0;
		for( int i=0; i<nPattern; i++ ){
			pNets->SimNet( prInput+i*nInputs, &vcOutput[0] );
			rError += inner_product( vcOutput.begin(), vcOutput.end(), prOutput, 0.0,
				plus<REAL>(), diff_sqr<REAL>() ) / nOutputs;
		}
		return rError / nPattern;
	}
			
	int _stdcall CREATENET( int* pnNeuros, int& nLayers )
	{
		CMiniNeuralNet* pNet = new CMiniNeuralNet();
		pNet->Create( pnNeuros, nLayers, NeuroTanSig );

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
	//(strM, nMChar) the M file. (pnNodes, nLayers), the nodes array defines ANN's architecture.
	//(prNetBias, nLayers) training bias.
	//(pstrTrain, nTrainChar) training file.
	//(pstrNet, nNetChar) net file.
	//(pstrValid, nValidChar) validation file.
	double _stdcall TRAINMATLABNETEX( char* pstrM, int nMChar, int* pnNodes, int& nLayers, double* prNetBias, char* pstrTrain, int nTrainChar, char* pstrNet, int nNetChar, char* pstrValid, int nValidChar )
	{
//		char strNodes[MAXPATH], strM[MAXPATH], strBias[MAXPATH], strTrain[MAXPATH], strNet[MAXPATH], strValid[MAXPATH];
		char strM[MAXPATH], strTrain[MAXPATH], strNet[MAXPATH], strValid[MAXPATH];
		strftoc( pstrM, nMChar, strM );
		strftoc( pstrTrain, nTrainChar, strTrain );
		strftoc( pstrNet, nNetChar, strNet );
		strftoc( pstrValid, nValidChar, strValid );

		return RunMatlabNet( strM, pnNodes, nLayers, prNetBias, strTrain, strNet, strValid );

/*		sprintf( strNodes, "[%d", pnNodes[0] );
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

		return mse;*/
	}
}

