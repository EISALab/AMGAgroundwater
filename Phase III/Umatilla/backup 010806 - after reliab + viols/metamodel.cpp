#include "metamodel.h"
#include "uwstd.h"
#include <functional>
#include "commdef.h"
#include <mathfunc.h>

int CSvmModel::m_nNextModelId = 0;

template< typename T >
struct minus_pow : public std::binary_function<T, T, T>
{
	T	exp;
public:
	minus_pow( T _exp ):exp(_exp){}
	T operator()( T x, T y )const{
		return pow( x-y, exp );
	}
};

template< typename T >
class op_del_obj
{
public:
	void operator() (T* ptr)
	{ if( ptr!=NULL )delete ptr; }
};

class op_not_in_region
{
	vector<REAL>& m_pt;
public:
	op_not_in_region( vector<REAL>& pt ) : m_pt(pt){};
	bool operator()( const CTrustRegion* pRegion ){
		return !pRegion->PtInRegion(m_pt);
	}
};

class op_region_compare
{
public:
	bool operator()( CTrustRegion* pReg1, CTrustRegion* pReg2 ){
		return pReg1->GetSphereRadius() - pReg2->GetSphereRadius() < 0 ; 
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


REAL CTrustRegion::PtInRegionProb(const vector<REAL>& pt )const
{
	int nInputs = pt.size();
	vector<REAL> vcDistSqr(nInputs, 0.0);
	vector<REAL> vcRadSqr(nInputs, 0.0);
	vector<REAL> vcProbs(nInputs,0.0);

	transform( pt.begin(), pt.end(), m_ptCen.begin(), vcDistSqr.begin(), diff_sqr<REAL>() );
//	cout<<"dist sqr:";
//	copy( vcDistSqr.begin(), vcDistSqr.end(), ostream_iterator<REAL>(cout, " ") ); cout<<endl;
	transform( m_vcRadius.begin(), m_vcRadius.end(), m_vcRadius.begin(), vcRadSqr.begin(), multiplies<REAL>() );
//	cout<<"radius sqr:";
//	copy( vcRadSqr.begin(), vcRadSqr.end(), ostream_iterator<REAL>(cout, " ") ); cout<<endl;
	transform( vcDistSqr.begin(), vcDistSqr.end(), vcRadSqr.begin(), vcProbs.begin(), divides<REAL>() );
//	cout<<"probs :";
//	copy( vcProbs.begin(), vcProbs.end(), ostream_iterator<REAL>(cout, " ") ); cout<<endl;
	REAL rProb = accumulate( vcProbs.begin(), vcProbs.end(), 0.0) / nInputs;
	rProb = max( 1-rProb, 0.0 );

	return rProb;
}


REAL CMetaModel::Test( const REAL* prInputs, const REAL* prOutputs, int count )
{
	vector<REAL> vcOutputs( GetOutputs() );

	//calculate the error function under the current weight
	REAL rError = 0;
	for( int i=0; i<count; i++ ){
		Predict( prInputs + i*GetInputs(), &vcOutputs[0] );
		rError += inner_product( vcOutputs.begin(), vcOutputs.end(), prOutputs, 0.0,
			plus<REAL>(), minus_pow<REAL>(2.0) ) / GetOutputs();
	}
	return rError/count;
}

CNeuralModel::CNeuralModel( int* pLayerNodes, int nLayers )
{
	m_pNeuralNet = NULL;
	SetLayerNodes( pLayerNodes, nLayers );
}

void CNeuralModel::SetLayerNodes( int* prLayerNodes, int nLayers )
{
	ASSERT( nLayers>=3 );
	m_vcLayerNodes.assign( prLayerNodes, prLayerNodes+nLayers );

	SetInputs( m_vcLayerNodes[0] );
	SetOutputs( m_vcLayerNodes[nLayers-1] );
}

void CNeuralModel::SetTrainBias( REAL* prTrainBias )
{
	ASSERT( m_vcLayerNodes.size() >=3 );
	//the last element is the weight coefficient
	m_vcTrainBias.assign( prTrainBias, prTrainBias+GetOutputs()+1 );
}

void CNeuralModel::Train( const string& strTrainData, const string& strValidData )
{
	if( m_pNeuralNet!=NULL ){
		delete m_pNeuralNet;
		m_pNeuralNet = NULL;
	}

	double rValidRatio = 0.3;
	int count = 0;
	char buf[_MAX_LINE];
	{
		std::ifstream ifg( strTrainData.c_str() );
		std::ofstream ofgTrain( TRAIN_FILE );
		std::ofstream ofgValid( VALID_FILE );

		ifg.getline( buf, ELEMENTS(buf) );
		while( !ifg.eof() && !ifg.fail() ){
			trimleft( buf );
			if( buf[0]!=NULL ){
				count++;
				if( count%4 == 0 ){
//				if( uni_rand()<rValidRatio ){
					ofgValid<<buf<<endl;
				}else{
					ofgTrain<<buf<<endl;
				}
			}
			ifg.getline( buf, ELEMENTS(buf) );
		}
	}

	m_pNeuralNet = new CMiniNeuralNet();
	double rError = RunMatlabNet( NET_CMD, &m_vcLayerNodes[0], m_vcLayerNodes.size(), &m_vcTrainBias[0], 
		TRAIN_FILE, MODEL_FILE, VALID_FILE );
	std::cout<<"Error: "<<rError<<endl;
	m_pNeuralNet->Create( &m_vcLayerNodes[0], m_vcLayerNodes.size(), NeuroTanSig );
	m_pNeuralNet->LoadMatlabNet( MODEL_FILE );
}

void CNeuralModel::Predict( const REAL* prInputs, REAL* prOutputs )
{
	m_pNeuralNet->Simulate( prInputs, prOutputs );
}

CSvmModel::CSvmModel()
{
}

string CSvmModel::CreateUniqueModelName()
{
	string strModel = MODEL_FILE;
	char buf[20];
	itoa( m_nNextModelId++, buf, 10 );
	string::size_type idx = strModel.find( "." );
	if( idx!=string::npos ){
		strModel.insert( idx, buf );
	}else{
		strModel += buf;
	}
	return strModel;
}

string CSvmModel::GetOptions( int id)
{
	ASSERT( id>=0 && id<GetOutputs() );
	return m_vcOptions[id];
//	return m_strOptions;
}

void CSvmModel::SetOptions( const string& strOptions, int id )
{
	ASSERT( id>=0 && id<GetOutputs() );

	if( m_vcOptions.size()<GetOutputs() )m_vcOptions.resize( GetOutputs() );
	m_vcOptions[id] = strOptions;
//	m_strOptions = strOptions;
}

void CSvmModel::Train( const string& strTrainData, const string& strValidData )
{
	Release();

	for( int i=0; i<GetOutputs(); i++ ){
		TransformToSvmData( strTrainData, GetInputs(), GetOutputs(), i, SVM_TRAIN_FILE );
		string strModelFile = CreateUniqueModelName();
		RunSvmTrain( SVM_TRAIN_FILE, /*parameters*/GetOptions(i), strModelFile );
		m_vcModelFiles.push_back( strModelFile );

		SVM_MODEL* model=svm_load_model( strModelFile.c_str() );
		m_vcModels.push_back( model );
	}
}

void CSvmModel::Predict( const REAL* prInputs, REAL* prOutputs )
{
	//write the inputs into a temporary test file
/*	{
		ofstream ofg(TEST_FILE);
		vector<REAL> vcInputs( prInputs, prInputs+GetInputs() );
		vector<REAL> vcOutputs;
		TransformSvmLine( ofg, vcInputs, vcOutputs, 0 );
	}*/

	//predict for each model and put each output into prOutputs[i]
	for( int i=0; i<GetOutputs(); i++ ){
		svm_predict( m_vcModels[i], prInputs, GetInputs(), &prOutputs[i] );
//		RunSvmPredict( TEST_FILE, m_vcModelFiles[i], prOutputs[i] );
	}
}

void CSvmModel::TransformToSvmData( const string& strDataFile, int nInputs, int nOutputs, int nPredId, const string& strOutput )
{
	vector<REAL> vcInputs(nInputs);
	vector<REAL> vcOutputs(nOutputs);

	ifstream ifg( strDataFile.c_str() );
	ofstream ofg( strOutput.c_str() );

	while( !ifg.eof() && !ifg.fail() ){
		//read all the input and input vectors at each line
		for( int i=0; i<nInputs; i++ )ifg>>vcInputs[i];
		for( i=0; i<nOutputs; i++ )ifg>>vcOutputs[i];

		if( ifg.eof() || ifg.fail() )break;

		TransformSvmLine( ofg, vcInputs, vcOutputs, nPredId );
	}
}

void CSvmModel::TransformSvmLine( ofstream& ofg, const vector<REAL>& vcInputs, const vector<REAL>& vcOutputs, int nPredId )
{
	int nInputs = vcInputs.size();
	int nOutputs = vcOutputs.size();
	//write the output variable if necessary
	if( nOutputs>0 && nPredId<nOutputs ){
		ofg<<fixed<<setprecision(6)<<vcOutputs[nPredId]<<"\t\t";
	}else{
		//must write something, or svm predict will crash
		ofg<<fixed<<setprecision(6)<<0.0<<"\t\t";
	}
	//write the input variables
	for( int i=0; i<nInputs; i++ ){
		ofg<<(i+1)<<":"<<fixed<<setprecision(6)<<vcInputs[i]<<"\t";
	}
	ofg<<endl;
}


/*
`svm-train' Usage
=================

Usage: svm-train [options] training_set_file [model_file]
options:
-s svm_type : set type of SVM (default 0)
	0 -- C-SVC
	1 -- nu-SVC
	2 -- one-class SVM
	3 -- epsilon-SVR
	4 -- nu-SVR
-t kernel_type : set type of kernel function (default 2)
	0 -- linear: u'*v
	1 -- polynomial: (gamma*u'*v + coef0)^degree
	2 -- radial basis function: exp(-gamma*|u-v|^2)
	3 -- sigmoid: tanh(gamma*u'*v + coef0)
-d degree : set degree in kernel function (default 3)
-g gamma : set gamma in kernel function (default 1/k)
-r coef0 : set coef0 in kernel function (default 0)
-c cost : set the parameter C of C-SVC, epsilon-SVR, and nu-SVR (default 1)
-n nu : set the parameter nu of nu-SVC, one-class SVM, and nu-SVR (default 0.5)
-p epsilon : set the epsilon in loss function of epsilon-SVR (default 0.1)
-m cachesize : set cache memory size in MB (default 40)
-e epsilon : set tolerance of termination criterion (default 0.001)
-h shrinking: whether to use the shrinking heuristics, 0 or 1 (default 1)
-b probability_estimates: whether to train an SVC or SVR model for probability estimates, 0 or 1 (default 0)
-wi weight: set the parameter C of class i to weight*C in C-SVC (default 1)
-v n: n-fold cross validation mode
*/

void CSvmModel::RunSvmTrain( const string& strTrainFile, const string& strOptions, const string& strModelFile )
{
	//the svm training command is,
	//Usage: svm-train [options] training_set_file [model_file]

	remove( strModelFile.c_str() );

	char buf[_MAX_LINE];
	ostrstream ostr( buf, ELEMENTS(buf) );
	ostr<<SVM_TRAIN_CMD<<" "<<strOptions.c_str()<<" "<<strTrainFile.c_str()<<" "<<strModelFile.c_str()<<endl;
	system( buf );

	ASSERT( IsFileExist(strModelFile.c_str()) );
}

/*
`svm-predict' Usage
===================

Usage: svm-predict [options] test_file model_file output_file
options:
-b probability_estimates: whether to predict probability estimates, 0 or 1 (default 0); one-class SVM not supported yet

model_file is the model file generated by svm-train.
test_file is the test data you want to predict.
svm-predict will produce output in the output_file.
*/

void CSvmModel::RunSvmPredict( const string& strDataFile, const string& strModelFile, REAL& rOutput )
{
	//the svm prediction command is,
	//Usage: svm-predict [options] test_file model_file output_file

	char buf[_MAX_LINE];
	ostrstream ostr( buf, ELEMENTS(buf) );
	ostr<<SVM_PREDICT_CMD<<" "<<strDataFile.c_str()<<" "<<strModelFile.c_str()<<" "<<SVM_PREDICT_FILE<<endl;
	system( buf );

	ifstream ifg( SVM_PREDICT_FILE );
	ifg>>rOutput;
}

void CRegionalMetaModel::Release()
{
	for_each( m_vcRegions.begin(), m_vcRegions.end(), op_del_obj<CTrustRegion>() );
	for_each( m_vcMetaModels.begin(), m_vcMetaModels.end(), op_del_obj<CMetaModel>() );
	m_vcRegions.clear();
	m_vcMetaModels.clear();

	if( m_pmxGating!=NULL )delete m_pmxGating;
	m_pmxGating = NULL;
}

void CRegionalMetaModel::FindTrustRegions( const REAL* prInput, vector<CTrustRegion*>& hitRegions )
{
	ASSERT( !m_vcMetaModels.empty() );
	hitRegions.clear();

	int nInputs = GetInputs();
	vector<REAL> vcInput( prInput, prInput+nInputs );

	remove_copy_if( m_vcRegions.begin()+1, m_vcRegions.end(), back_inserter(hitRegions), op_not_in_region(vcInput) );

	//the first neural net is the global prediction, always available.
	//modifed on 02/12/05. 
	hitRegions.insert( hitRegions.begin(), m_vcRegions.front() );
}

void CRegionalMetaModel::GetTrustRegionVector( const REAL* prInput, REAL* regs )
{
	ASSERT( !m_vcMetaModels.empty() );
//	regs.clear();

	int nInputs = GetInputs();
	vector<REAL> vcInput( prInput, prInput+nInputs );

//	regs.resize( GetModelCount() );
	for( int i=1; i<GetModelCount(); i++ ){
		if( m_vcRegions[i]->PtInRegionProb(vcInput)>0 )
//		if( m_vcRegions[i]->PtInRegion2( vcInput ) )
			regs[i] = 1.0;
		else
			regs[i] = 0.0;
	}
	//the first neural net is the global prediction, always available.
	regs[0] = 1.0;
}

void CRegionalMetaModel::FindBestRegion( const REAL* prInput, vector<CTrustRegion*>& hitRegions )
{
	FindTrustRegions( prInput, hitRegions );

	vector<CTrustRegion*>::iterator pos = min_element( hitRegions.begin(), hitRegions.end(), op_region_compare() );
	ASSERT( pos!=hitRegions.end() );

	CTrustRegion* pMinRegion = *pos;
	hitRegions.clear();

	hitRegions.push_back( pMinRegion );
}


void CRegionalMetaModel::Predict( const REAL* prInputs, REAL* prOutputs )
{
	PredictEx( prInputs, prOutputs );
	return;

	vector< CTrustRegion* >hitRegions;

	//best region or multiple region?
	FindTrustRegions( prInputs, hitRegions );
//	FindBestRegion( prInputs, hitRegions );

	int nOutputs = GetOutputs();
	int nHitRegions = hitRegions.size();
	//for each trusted regional model, predict the result to vcOutputs[i][1...nOutputs]
	vector< vector<REAL> > vcOutputs(nHitRegions);
	for( int i=0; i<nHitRegions; i++ ){
		vcOutputs[i].resize( nOutputs );
		CMetaModel* pModel = m_vcMetaModels[ hitRegions[i]->GetModelId() ];
		pModel->Predict( prInputs, &vcOutputs[i][0] );
	}

	int nInputs = GetInputs();
	REAL rSumWeights = 0;
	vector< REAL > vcSum( nOutputs, 0.0 );
	//modified on 02/012/05 using trust probability
	for( i=0; i<nHitRegions; i++ ){
		ASSERT( nInputs==hitRegions[i]->m_ptCen.size() );
		vector<REAL> vcDistSqr(nInputs, 0.0);
		vector<REAL> vcRadSqr(nInputs, 0.0);
		vector<REAL> vcProbs(nInputs,0.0);
		transform( prInputs, prInputs+nInputs, hitRegions[i]->m_ptCen.begin(), vcDistSqr.begin(), diff_sqr<REAL>() );
//		cout<<"dist sqr:";
//		copy( vcDistSqr.begin(), vcDistSqr.end(), ostream_iterator<REAL>(cout, " ") ); cout<<endl;
		transform( hitRegions[i]->m_vcRadius.begin(), hitRegions[i]->m_vcRadius.end(), hitRegions[i]->m_vcRadius.begin(), vcRadSqr.begin(), multiplies<REAL>() );
//		cout<<"radius sqr:";
//		copy( vcRadSqr.begin(), vcRadSqr.end(), ostream_iterator<REAL>(cout, " ") ); cout<<endl;
		transform( vcDistSqr.begin(), vcDistSqr.end(), vcRadSqr.begin(), vcProbs.begin(), divides<REAL>() );
//		cout<<"probs :";
//		copy( vcProbs.begin(), vcProbs.end(), ostream_iterator<REAL>(cout, " ") ); cout<<endl;
		REAL rProb = accumulate( vcProbs.begin(), vcProbs.end(), 0.0) / nInputs;
		rProb = max( 1-rProb, 0.0 );

		//the first global model is always trusted.
		if( i==0 && rProb<=0 )rProb = max( rProb, 1e-3 );

		cdump<<"prob "<<i<<" "<<rProb<<"\t";

//		REAL rWeight = rProb / hitRegions[i]->GetSphereRadius();
		REAL rWeight = rProb;
		for( int j=0; j<nOutputs; j++ ){
			vcSum[j] += vcOutputs[i][j]*rWeight;
		}
		rSumWeights += rWeight;
	}
	if( rSumWeights > 0 ){
		transform( vcSum.begin(), vcSum.end(), vcSum.begin(), bind2nd(divides<REAL>(), rSumWeights) );
		copy( vcSum.begin(), vcSum.end(), prOutputs );
	}else{
		copy( vcOutputs[0].begin(), vcOutputs[0].end(), prOutputs );
	}

	//compute the average outputs according to inverse sphere radius
/*	vector< REAL > vcSum( nOutputs, 0.0 );
	REAL rSumInvRadius = 0;
	for( i=0; i<nHitRegions; i++ ){
		REAL rInvRadius = 1.0 / hitRegions[i]->GetSphereRadius();
		for( int j=0; j<nOutputs; j++ ){
			vcSum[j] += vcOutputs[i][j]*rInvRadius;
		}
		rSumInvRadius += rInvRadius;
	}
	transform( vcSum.begin(), vcSum.end(), vcSum.begin(), bind2nd(divides<REAL>(), rSumInvRadius) );
	copy( vcSum.begin(), vcSum.end(), prOutputs );
*/
	cdump<<"pred..."<<nHitRegions<<" nets"<<endl;
}

void CRegionalMetaModel::PredictEx( const REAL* prInputs, REAL* prOutputs )
{
	int nInputs = GetInputs();
	int nOutputs = GetOutputs();
	int nModels = GetModelCount();

	//use matrix operator to wrap the inputs
	ZRealMatrix mxTrain( nInputs, 1, prInputs );

	//compute  mxU[nModels, 1]. mxGating[nInputs, nModels], mxTrain[nModels, 1];
	ZRealMatrix mxU = ElemExp( m_pmxGating->GetTransposeMx() * mxTrain );

	//get the trust region test matrix
	ZRealMatrix mxRegs(nModels, 1);
	GetTrustRegionVector( prInputs, &mxRegs(1,1) );

	//compute mxG[nModels,1]
	REAL rSum = 0;						//rSum is sum{ t(i)*exp[u(i)] }
	for( int i=1; i<=nModels; i++ ){
		mxU(i,1) *= mxRegs(i,1);
		rSum += mxU(i,1);
	}
	ZRealMatrix mxG = mxU / rSum;
//	cdump<<"prob "<<mxG.GetTransposeMx();


	//compute internal model outputs.
	ZRealMatrix mxYin(nModels, nOutputs);
	for( i=0; i<nModels; i++ ){
		m_vcMetaModels[i]->Predict( prInputs, &mxYin(i+1,1) );
	}
	mxYin.Transpose();		//mxYin is [nOutputs, nModels]

	//compute the final output mxY[nOutputs, 1]
	ZRealMatrix mxY(nOutputs, 1, 0.0);
	for( i=1; i<=nModels; i++ ){
		mxY += mxYin.GetColMx(i)*mxG(i,1);
	}

	for( i=0; i<nOutputs; i++ )prOutputs[i] = mxY(i+1,1);
}


void CRegionalMetaModel::ReadTrainingData( const string& strDataFile, int nInputs, int nOutputs, vector< vector<REAL> >& vcInputs, vector< vector<REAL> >& vcOutputs )
{
	ifstream ifg( strDataFile.c_str() );
	char buf[_MAX_LINE];
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

/*void CRegionalMetaModel::ComputeDistance( vector< vector<REAL> >& vcInputs, vector<REAL>& vcPtCen, dist_pair_vector& vcDists )
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
}*/

//the compute distance is undertaken an overhaull. I prefere to compute the probability instead of the 
//euclidean distance.
void CRegionalMetaModel::ComputeDistance( vector< vector<REAL> >& mxInputs, vector<REAL>& vcPtCen, dist_pair_vector& vcDists )
{
	int rows = mxInputs.size();
	int cols = mxInputs.front().size();

	/*compute the center of the hyperecllipsoid*/
	vector<REAL> center(cols, 0.0);
	for( vector< vector<REAL> >::iterator iter=mxInputs.begin(); iter!=mxInputs.end(); iter++ )
		transform( center.begin(), center.end(), iter->begin(), center.begin(), plus<REAL>() );
	transform( center.begin(), center.end(), center.begin(), bind2nd(divides<REAL>(), rows) );

	/*compute the standard deviation of the columns*/
	//compute sum( (x-u)*(x-u) ) for each column
	vector<REAL> stds(cols, 0.0);
	for( int i=0; i<rows; i++ )
		for( int j=0; j<cols; j++ )stds[j] += sqr( center[j] - mxInputs[i][j] );
	
	//using compose function operator to do "sqrt( sum/N-1 )"
	transform( stds.begin(), stds.end(), stds.begin(), 
		compose_f_gx( 
			ptr_fun(sqrt), bind2nd(divides<double>(), rows-1) 
		)
	);

	/* compute the radius */
	REAL alpha = 2.5;
	vector<REAL> radius(cols);
	transform( stds.begin(), stds.end(), radius.begin(), bind2nd(multiplies<REAL>(), alpha) );
	for( i=0; i<cols; i++ )if( radius[i]<=0.0001 )radius[i]=0.0001;

	/* compute the distance for each point */
	vector<REAL> radius_sqr(cols);
	transform( radius.begin(), radius.end(), radius.begin(), radius_sqr.begin(), multiplies<REAL>() );
	for( i=0; i<rows; i++ ){
		vector<REAL> temp(cols);
		transform( mxInputs[i].begin(), mxInputs[i].end(), center.begin(), temp.begin(), diff_sqr<REAL>() );
		REAL distance = inner_product( temp.begin(), temp.end(), radius_sqr.begin(), 0.0, plus<REAL>(), divides<REAL>() );
		vcDists.push_back( dist_pair(i, distance) );
	}
	sort( vcDists.begin(), vcDists.end(), op_dist_less() );
}

vector<int> CRegionalMetaModel::SampleIds( const dist_pair_vector& vcDists, int nSamples )
{
	vector<int> vcSamples;

	if( true ){
		//this algorithm tries to make a uniform sampling over the set.
		int nBuckets = 50;
		REAL rMin = vcDists[0].second;
		REAL rMax = vcDists[ vcDists.size()-1 ].second;
		REAL rStep = (rMax-rMin)/nBuckets;

		//compute the histogram. Each bucket contains the ids.
		vector< vector<int> > vcBuckets(nBuckets);
		for( int i=0; i<vcDists.size(); i++ ){
			int nId = (int)( (vcDists[i].second - rMin) / rStep );
			nId = min( nId, vcBuckets.size()-1 );

			vcBuckets[nId].push_back( vcDists[i].first );
		}

		//compute the max density of the buckets.
		int nMaxRate = 0;
		for( i=0; i<nBuckets; i++ ){
			if( vcBuckets[i].size()>nMaxRate )nMaxRate = vcBuckets[i].size();
		}

		//find the sampling rate at each bucket.
		int r1 = 0;
		int r2 = nMaxRate;
		int r = (r1+r2)/2;

		while( (r>r1)&&(r2>r) ){
			int nSumSamples = 0;
			for( int i=0; i<nBuckets; i++ ){
				if( r > vcBuckets[i].size() ){
					nSumSamples += vcBuckets[i].size();
				}else{
					nSumSamples += r;
				}
			}
			if( nSumSamples == nSamples )break;
			if( nSumSamples < nSamples ){
				r1 = r;
			}else{
				r2 = r;
			}
			r = (r1 + r2 ) /2;
		}

		//now do the actual sampling. nRate -- the maximum sampling number at each bucket.
		int nRate = r;
		for( i=0; i<nBuckets; i++ ){
			vector<int>::iterator pos_end = min( vcBuckets[i].begin()+nRate, vcBuckets[i].end() );
			//shuffle it if the bucket has more points than the sampling rate.
			if( vcBuckets[i].size() > nRate )random_shuffle( vcBuckets[i].begin(), vcBuckets[i].end() );
			copy( vcBuckets[i].begin(), pos_end, back_inserter(vcSamples) );
		}
	}else{
		//this is random sampling from the whole set.

		//copy the ids. and random_shuffle it, then copy it.
		vector<int> vcTmpIds( vcDists.size() );
		for( int i=0 ; i<vcTmpIds.size(); i++ )vcTmpIds[i] = vcDists[i].first;

		if( false ){
			random_shuffle( vcTmpIds.begin(), vcTmpIds.end() );
			copy( vcTmpIds.begin(), vcTmpIds.begin()+nSamples, back_inserter(vcSamples) );
		}else{
			copy( vcTmpIds.begin(), vcTmpIds.end(), back_inserter(vcSamples) );
		}
	}
	return vcSamples;
}


//vcDists: sorted pair<id, dist>. 
void CRegionalMetaModel::SubdividTrainingData( dist_pair_vector& vcDists, vector< vector<int> >& vcIdSet, int min_pts )
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
		//will sample from the first half ids.
		dist_pair_vector vcTmpDists( vcDists.begin(), pos );

		//sample from the first half
		vector<int> vcIds = SampleIds( vcTmpDists, nSecndHalfPts );

		//then sample all the points in the second half
		vcIds.reserve( vcIds.size()+nSecndHalfPts );
		for( int i=0; i<nSecndHalfPts; i++ )vcIds.push_back( (*pos++).first );

		//put it into the vcIdSet and do the next subdivition
		vcIdSet.push_back( vcIds );

		//need a subdivition.
		SubdividTrainingData( vcTmpDists, vcIdSet, min_pts );
	}
}

//vcDists: sorted pair<id, dist>. 
void CRegionalMetaModel::SubdividTrainingData2( dist_pair_vector& vcDists, vector< vector<int> >& vcIdSet, int min_pts )
{
	//step 1. find the media's id.
	REAL rMaxDist = vcDists[ vcDists.size()-1 ].second;
	REAL rMinDist = vcDists[ 0 ].second;

	if( vcDists.size() <= 2*min_pts ){
		//there are not enough points to support the next subdivition, stop it.
		vector<int> vcIds;
		vcIds.resize( vcDists.size() );
		for( int i=0; i<vcDists.size(); i++ )vcIds[i] = vcDists[i].first;
		vcIdSet.push_back( vcIds );
	}else{
		//prepare the global sets.
		dist_pair_vector::iterator pos = vcDists.begin()+min_pts;
		REAL rMidDist = pos->second;
		int nSecndHalfPts = vcDists.size() - min_pts;

		//save the first half dist pairs (local region)
		//will sample from the first half ids.
		dist_pair_vector vcTmpDists( vcDists.begin(), pos );

		//compute the sample pts in the local region using the same density as the global one
		int nSamples = (vcDists.size()-min_pts) / (rMaxDist-rMidDist) * (rMidDist - rMinDist);
		if( nSamples > min_pts )nSamples = min_pts;

		//sample from the first half
		vector<int> vcIds = SampleIds( vcTmpDists, min_pts );

		//then sample all the points in the second half
		vcIds.reserve( vcIds.size()+nSecndHalfPts );
		for( int i=0; i<nSecndHalfPts; i++ )vcIds.push_back( (*pos++).first );

		//put it into the vcIdSet and do the next subdivition
		vcIdSet.push_back( vcIds );

		//prepare the local region.
		vector<int> vcLocalIds;
		vcLocalIds.reserve( min_pts );
		for( i=0; i<min_pts; i++ )vcLocalIds.push_back( vcTmpDists[i].first );

		vcIdSet.push_back( vcLocalIds );
	}
}

void CRegionalMetaModel::DoWriteTrainingFile( const char* strTrain, const char* strValid, vector< vector<REAL> >& vcInputs, vector< vector<REAL> >& vcOutputs, vector<int>& vcIds, REAL rValidRatio )
{
	//should ignore valid file, because the sub meta-model will create valid file if necessary
	std::ofstream ofgTrain( strTrain );
	std::ofstream ofgValid( strValid );

	size_t nsize = vcIds.size();
	int id;
	for( int i=0; i<nsize; i++ ){
//	for( int i=nsize-1; i>=0; i-- ){
		ofstream* pofg = NULL;
//		if( count % (int)(1.0/rValidRatio+0.5) ==0 )pofg=&ofgValid;
//		if( uni_rand()<rValidRatio )pofg = &ofgValid;
//		else pofg = &ofgTrain;
		pofg = &ofgTrain;
		
		id = vcIds[i];
		copy( vcInputs[id].begin(), vcInputs[id].end(), ostream_iterator<REAL>(*pofg, "\t") );
		copy( vcOutputs[id].begin(), vcOutputs[id].end(), ostream_iterator<REAL>(*pofg, "\t") );
		*pofg << endl;
	}
}


void CRegionalMetaModel::ComputeTrustRegion( CTrustRegion* pRegion, vector< vector<REAL> >& vcInputs, vector<int>& vcIds )
{
	int nDim = vcInputs[0].size();
	int nCount = vcIds.size();
	vector<REAL> vcPtMean(nDim, 0.0);

	//assume regions is normalized
	vector<REAL> vcMins( nDim, 1000.0 );
	vector<REAL> vcMaxs( nDim, -1000.0 );

	//compute the mean, min and max of each input variable
	for( int i=0; i<nCount; i++ ){
		for( int j=0; j<nDim; j++ ){
			vcPtMean[j] += vcInputs[vcIds[i]][j];
			vcMins[j] = min( vcMins[j], vcInputs[ vcIds[i] ][j] );
			vcMaxs[j] = max( vcMaxs[j], vcInputs[ vcIds[i] ][j] );
		}
	}
	transform( vcPtMean.begin(), vcPtMean.end(), vcPtMean.begin(), bind2nd( divides<REAL>(), nCount ) );

	//compute the standard deviation.
	vector<REAL> vcStds(nDim, 0.0);
	for( i=0; i<nCount; i++ ){
		for( int j=0; j<nDim; j++ ){
			vcStds[j] += diff_sqr<REAL>()(vcInputs[vcIds[i]][j], vcPtMean[j]);
		}
	}
	//modified on 02/09/05 from nCount to nCount-1
	transform( vcStds.begin(), vcStds.end(), vcStds.begin(), bind2nd( divides<REAL>(), nCount-1 ) );
	transform( vcStds.begin(), vcStds.end(), vcStds.begin(), sqrt );

	//modified on 02/09/05 from nCount to 2.0
	//compute the radius. std*2.5 ?
	double alpha = 2.5;
	vector<REAL> vcRadius = vcStds;
	transform( vcRadius.begin(), vcRadius.end(), vcRadius.begin(), bind2nd( multiplies<REAL>(), alpha ) );
	for( i=0; i<vcRadius.size(); i++ )
		if( vcRadius[i]<=0.0001 )vcRadius[i]=0.0001;

	cdump<<"Trust region created:"<<endl;
	cdump<<"Min, Max, Stds, Cens, Radius"<<endl;
	for( i=0; i<vcMins.size(); i++ )cdump<<vcMins[i]<<" "; cdump<<endl;
	for( i=0; i<vcMaxs.size(); i++ )cdump<<vcMaxs[i]<<" "; cdump<<endl;
	for( i=0; i<vcStds.size(); i++ )cdump<<vcStds[i]<<" "; cdump<<endl;
	for( i=0; i<vcPtMean.size(); i++ )cdump<<vcPtMean[i]<<" "; cdump<<endl;
	for( i=0; i<vcRadius.size(); i++ )cdump<<vcRadius[i]<<" "; cdump<<endl;
	pRegion->Create( vcMins, vcMaxs, vcStds, vcPtMean, vcPtMean, vcRadius );
}

void CRegionalMetaModel::Train( const string& strTrainData, const string& strValidData )
{
	int nInputs = GetInputs();
	int nOutputs = GetOutputs();

	vector< vector<REAL> > vcInputs;
	vector< vector<REAL> > vcOutputs;
	vector< REAL > vcPtCen = m_vcPtCen;
	dist_pair_vector vcDists;
	vector< vector<int> > vcIdSet;

	Release();

	//read all the data into vcInputs and vcOutputs.
	ReadTrainingData( strTrainData, nInputs, nOutputs, vcInputs, vcOutputs );

	//compute the distance to the center point.
	ComputeDistance( vcInputs, vcPtCen, vcDists );

	//subdivid the training data into clusters.
	SubdividTrainingData2( vcDists, vcIdSet, m_nMinCutPts );

	//create the training set for each hierarch.
	for( int i=0; i<vcIdSet.size(); i++ ){
		sort( vcIdSet[i].begin(), vcIdSet[i].end() );
		//write a training set to files and run the matlab trainer
		WriteTrainingFile( REG_TRAIN_FILE, REG_VALID_FILE, vcInputs, vcOutputs, vcIdSet[i] );
		CMetaModel* pModel = CreateMetaModel();
		pModel->Train( REG_TRAIN_FILE, REG_VALID_FILE );

		CTrustRegion* pRegion = new CTrustRegion();
		ComputeTrustRegion( pRegion, vcInputs, vcIdSet[i] );

		m_vcMetaModels.push_back( pModel );
		
		pRegion->SetModelId( m_vcMetaModels.size()-1 );
		m_vcRegions.push_back( pRegion );
	}

	//create the gating network
	m_pmxGating = new ZRealMatrix( GetInputs(), GetModelCount() );

	//create the training set and validation set for the gating network
	vector< vector<REAL> > vcValidInputs;
	vector< vector<REAL> > vcValidOutputs;
	vector< vector<REAL> > vcTrainInputs;
	vector< vector<REAL> > vcTrainOutputs;

	for( i=0; i<vcInputs.size(); i++ ){
		if( (i+1)%4==0 ){
			vcValidInputs.push_back( vcInputs[i] );
			vcValidOutputs.push_back( vcOutputs[i] );
		}else{
			vcTrainInputs.push_back( vcInputs[i] );
			vcTrainOutputs.push_back( vcOutputs[i] );
		}
	}
	TrainGatingNetwork( vcTrainInputs, vcTrainOutputs, vcValidInputs, vcValidOutputs );

	cdump<<"training finsihed:"<<vcIdSet.size()<<" nets were trained!"<<endl;
}

/*
	N - the number of training data. nPattern
	m - the number of inputs
	o - the number of outputs
*/

ZRealMatrix CRegionalMetaModel::ComputeDeta( const vector<REAL>& mxInputs, const vector<REAL>& mxOutputs )
{
	int nInputs = GetInputs();
	int nOutputs = GetOutputs();
	int nModels = GetModelCount();

	//use matrix operator to wrap the inputs
	ZRealMatrix mxTrain( nInputs, 1, &mxInputs[0] );
	ZRealMatrix mxTest( nOutputs, 1, &mxOutputs[0] );

//	cdump<<"Inputs:"<<endl<<mxTrain<<endl;
//	cdump<<"outputs:"<<endl<<mxTest<<endl;
//	cdump<<"gating"<<endl<<(*m_pmxGating)<<endl;
	//compute  mxU[nModels, 1]. mxGating[nInputs, nModels], mxTrain[nModels, 1];
	ZRealMatrix mxU = ElemExp( m_pmxGating->GetTransposeMx() * mxTrain );
//	cdump<<mxU<<endl;

	//get the trust region test matrix
	ZRealMatrix mxRegs(nModels, 1);
	GetTrustRegionVector( &mxInputs[0], &mxRegs(1,1) );
//	cdump<<"Regions:"<<endl<<mxRegs<<endl;

	//compute mxG[nModels,1]
	REAL rSum = 0;						//rSum is sum{ t(i)*exp[u(i)] }
	for( int i=1; i<=nModels; i++ ){
		mxU(i,1) *= mxRegs(i,1);
		rSum += mxU(i,1);
	}
	ZRealMatrix mxG = mxU / rSum;
//	cdump<<"G:"<<endl<<mxG<<endl;

	//compute internal model outputs.
	ZRealMatrix mxYin(nModels, nOutputs);
	for( i=0; i<nModels; i++ ){
		m_vcMetaModels[i]->Predict( &mxInputs[0], &mxYin(i+1,1) );
	}
	mxYin.Transpose();		//mxYin is [nOutputs, nModels]
//	cdump<<mxYin<<endl;

	//compute the final output mxY[nOutputs, 1]
	ZRealMatrix mxY(nOutputs, 1, 0.0);
	for( i=1; i<=nModels; i++ ){
		mxY += mxYin.GetColMx(i)*mxG(i,1);
	}
//	cdump<<mxY<<endl;

	//compute mxE[nOutputs, 1]
	ZRealMatrix mxE = mxTest - mxY;

	//compute deta, mxTemp is [1,nOutputs]*[nOutputs,nModels] = [1, nModels]
	ZRealMatrix mxTemp = mxE.GetTransposeMx() * ( mxY * ZRealMatrix(1, nModels, 1.0) - mxYin );
	for( i=1; i<=nModels; i++ )mxTemp(1,i) *= mxG(i,1);
	ZRealMatrix mxDeta = 2*mxTrain * mxTemp;

//	cdump<<"Deta:"<<endl<<mxDeta<<endl;
	//mxDeta is [nInputs, nModels], which is the correction matrix for the gating network
	return mxDeta;
}

REAL CRegionalMetaModel::TrainGating2( const vector< vector<REAL> >& mxInputs, const vector< vector<REAL> >& mxOutputs, const vector< vector<REAL> >& mxValidInputs, const vector< vector<REAL> >& mxValidOutputs, int nMaxEpoch )
{
	int nPattern = mxInputs.size();
	int nInputs = GetInputs();
	int nOutputs = GetOutputs();
	int nModels = GetModelCount();

	//alpha is learning rate, beta is momentum correction rate
	REAL alpha = 0.02;
	REAL beta = 0.1;

	m_pmxGating->SetRandom( 0.0, 1.0 );
	ZRealMatrix mxBeta(nInputs, nModels, 0.0);

	REAL rMinTestMSE = 1e100;
	ZRealMatrix mxBestGating = (*m_pmxGating);

	int nMaxDangleEpoch = 50;					//stop training if after nMaxDangleEpoch epochs without further decreasing validation mse
	int nLastDecreasingEpoch = 0;

/*
	//for debugging.
	cdump<<"All the inputs"<<endl;
	for( int i=0; i<nPattern; i++ ){
		for( int j=0; j<nInputs; j++ )cdump<<mxInputs[i][j]<<" ";
		cdump<<endl;
	}
	cdump<<"All the outputs"<<endl;
	for( i=0; i<nPattern; i++ ){
		for( int j=0; j<nOutputs; j++ )cdump<<mxOutputs[i][j]<<" ";
		cdump<<endl;
	}
	cdump<<"Internal predictions"<<endl;
	ZRealMatrix mxYin(nModels, nOutputs);
	for( i=0; i<nPattern; i++ ){
		for( int j=0; j<nModels; j++ ){
			m_vcMetaModels[j]->Predict( &mxInputs[i][0], &mxYin(j+1, 1) );
		}
		cdump<<mxYin<<endl;
	}

	cdump<<"Trust Regions:"<<endl;
	ZRealMatrix mxRegions(nPattern, nModels);
	for( i=0; i<nPattern; i++ ){
		GetTrustRegionVector( &mxInputs[i][0], &mxRegions(i+1,1) );
	}
	cdump<<mxRegions<<endl;
	
	//compute testing mse
	int nTestPattern = mxValidInputs.size();
	REAL rTestMSE = 0.0;
	for( i=0; i<nTestPattern; i++ ){
		vector<REAL> vcTestOut(nOutputs);
		m_vcMetaModels[0]->Predict( &mxValidInputs[i][0], &vcTestOut[0] );
		for( int k=0; k<nOutputs; k++ )rTestMSE += sqr(vcTestOut[k]-mxValidOutputs[i][k]);
	}
	rTestMSE = rTestMSE / nTestPattern / nOutputs;
	::cout<<"Original TESTMSE:"<<rTestMSE<<endl;
*/
	//the training input and output matrix
	ZRealMatrix mxI(nPattern, nInputs);
	ZRealMatrix mxO(nPattern, nOutputs);

	ZRealMatrix mxY(nPattern, nOutputs);
	ZRealMatrix mxYi(nPattern, nOutputs);
	ZRealMatrix mxE(nPattern, nOutputs);
	ZRealMatrix mxR(nPattern, nModels);

	//copy the output to matrix mxO and compute the trust regions.
	for( int i=0; i<nPattern; i++ ){
		copy( mxInputs[i].begin(), mxInputs[i].end(), &mxI(i+1,1) );
		copy( mxOutputs[i].begin(), mxOutputs[i].end(), &mxO(i+1,1) );
		GetTrustRegionVector( &mxInputs[i][0], &mxR(i+1,1) );
	}
//	cdump<<"Inputs:"<<endl<<mxI<<endl;
//	cdump<<"Outputs:"<<endl<<mxO<<endl;
//	cdump<<"Regions:"<<endl<<mxR<<endl;

	for( int epoch=0; epoch<=nMaxEpoch; epoch++ ){

		//compute testing mse
		int nTestPattern = mxValidInputs.size();
		REAL rTestMSE = 0.0;
		for( int i=0; i<nTestPattern; i++ ){
			vector<REAL> vcTestOut(nOutputs);
			PredictEx( &mxValidInputs[i][0], &vcTestOut[0] );
			for( int k=0; k<nOutputs; k++ )rTestMSE += sqr(vcTestOut[k]-mxValidOutputs[i][k]);
		}
		rTestMSE = rTestMSE / nTestPattern / nOutputs;
		if( rMinTestMSE > rTestMSE ){
			mxBestGating = (*m_pmxGating);
			rMinTestMSE = rTestMSE;
			nLastDecreasingEpoch = epoch;
		}

		//compute the current training mse
		REAL rMSE = 0.0;
		for( i=0; i<nPattern; i++ ){
			vector<REAL> vcTestOut(nOutputs);
			PredictEx( &mxInputs[i][0], &vcTestOut[0] );
			for( int k=0; k<nOutputs; k++ )rMSE += sqr(vcTestOut[k]-mxOutputs[i][k]);
		}
		rMSE = rMSE / nPattern / nOutputs;
		::cout<<"MSE:"<<rMSE<<"\t"<<"TESTMSE:"<<rTestMSE<<endl;

		//update the gating network using gradient descent.
		ZRealMatrix mxDeta(nModels, nInputs, 0.0);

		//compute the outputs.
		for( i=0; i<nPattern; i++ )PredictEx( &mxInputs[i][0], &mxY(i+1, 1) );
//		cdump<<"Y:"<<endl<<mxY<<endl;

		//compute the errors.
		ZRealMatrix mxE = mxO - mxY;
//		cdump<<"E:"<<endl<<mxE<<endl;

		//compute the U = R .* exp( I*A );
		ZRealMatrix mxU = mxI * (*m_pmxGating);
		mxU.ElemExp();
		mxU.ElemMul( mxR );
		ZRealMatrix mxSumU = mxU.GetSumColMx();
//		cdump<<"U:"<<endl<<mxU<<endl;

		//compute the gradient for each mxDeta(:,k)
		for( int k=0; k<nModels; k++ ){
			//compute the intermal model's ouputs
			for( int i=0; i<nPattern; i++ )m_vcMetaModels[k]->Predict( &mxInputs[i][0], &mxYi(i+1,1) );
//			cdump<<"mxYi:"<<endl<<mxYi<<endl;

			//compute mxGi
			ZRealMatrix mxGi = mxU.GetColMx(k+1);
			mxGi.ElemDiv( mxSumU );
//			cdump<<"mxGi:"<<endl<<mxGi<<endl;

			//compute (Y-Yi) .* E
			mxYi.Neg();
			mxYi += mxY;
			mxYi.ElemMul( mxE );
//			cdump<<"(Y-Yi).*E"<<endl<<mxYi<<endl;

			//compute L = Gi .* sum[(Y-Yi).*E]
			mxGi.ElemMul( mxYi.GetSumColMx() );
//			cdump<<"L:"<<endl<<mxGi<<endl;

			//compute ( L(T) * X )
			mxGi.Transpose();
			ZRealMatrix mxDti = mxGi * mxI;

//			cdump<<"mxDti"<<endl<<mxDti<<endl;

			copy( &mxDti(1,1), &mxDti(1,1)+nInputs, &mxDeta(k+1,1) );
		}
		//mxDeta is suppose to be [nInputs, nModels]
		mxDeta.Transpose();
		mxDeta *= 2;

//		cdump<<"Final Deta:"<<endl<<mxDeta<<endl;
		ZRealMatrix mxNew = (*m_pmxGating) - alpha*mxDeta + beta*mxBeta;
		mxBeta = mxNew - (*m_pmxGating);
		(*m_pmxGating) = mxNew;

		if( epoch - nLastDecreasingEpoch >=nMaxDangleEpoch ){
			break;
		}
	}

	(*m_pmxGating) = mxBestGating;
	return rMinTestMSE;
}

ZRealMatrix CRegionalMetaModel::ComputeGradient( ZRealMatrix& mxX, const ZRealMatrix& mxO, const ZRealMatrix& mxR, const ZRealMatrix& mxGate )
{
	int nPattern = mxX.GetRow();
	int nInputs = GetInputs();
	int nOutputs = GetOutputs();
	int nModels = GetModelCount();

	//assign the gating vector to the gating matrix.
	ZRealMatrix temp = mxGate.GetReshapeMx(nModels, nInputs);
	temp.Transpose();
	(*m_pmxGating) = temp;

	ZRealMatrix mxGd(nModels, nInputs, 0.0);

	ZRealMatrix mxY(nPattern, nOutputs);
	ZRealMatrix mxYi(nPattern, nOutputs);

	//compute the outputs.
	for( int i=1; i<=nPattern; i++ )PredictEx( &mxX(i,1), &mxY(i, 1) );

	//compute the errors.
	ZRealMatrix mxE = mxO - mxY;

	//compute the U = R .* exp( I*A );
	ZRealMatrix mxU = mxX * (*m_pmxGating);
	mxU.ElemExp();
	mxU.ElemMul( mxR );
	ZRealMatrix mxSumU = mxU.GetSumColMx();

	//compute the gradient for each mxDeta(:,k)
	for( int k=0; k<nModels; k++ ){
		//compute the intermal model's ouputs
		for( int i=1; i<=nPattern; i++ )m_vcMetaModels[k]->Predict( &mxX(i,1), &mxYi(i,1) );

		//compute mxGi
		ZRealMatrix mxGi = mxU.GetColMx(k+1);
		mxGi.ElemDiv( mxSumU );

		//compute (Y-Yi) .* E
		mxYi.Neg();
		mxYi += mxY;
		mxYi.ElemMul( mxE );

		//compute L = Gi .* sum[(Y-Yi).*E]
		mxGi.ElemMul( mxYi.GetSumColMx() );

		//compute ( L(T) * X )
		mxGi.Transpose();
		ZRealMatrix mxDti = mxGi * mxX;

		copy( &mxDti(1,1), &mxDti(1,1)+nInputs, &mxGd(k+1,1) );
	}
	mxGd *= 2;
	return mxGd.Reshape( mxGd.GetSize(), 1 );
}

REAL CRegionalMetaModel::ComputePerformance( ZRealMatrix& mxX, const ZRealMatrix& mxO,  const ZRealMatrix& mxGate )
{
	int nPattern = mxX.GetRow();
	int nInputs = GetInputs();
	int nOutputs = GetOutputs();
	int nModels = GetModelCount();

	//assign the gating vector to the gating matrix.
	ZRealMatrix temp = mxGate.GetReshapeMx(nModels, nInputs);
	temp.Transpose();
	(*m_pmxGating) = temp;

	REAL rMSE = 0.0;
	vector<REAL> vcTestOut(nOutputs);

	for( int i=0; i<nPattern; i++ ){
		PredictEx( &mxX(i+1,1), &vcTestOut[0] );
		for( int k=0; k<nOutputs; k++ )rMSE += sqr(vcTestOut[k]-mxO(i+1,k+1));
	}
	return rMSE / nOutputs / nPattern;
}

struct MODEL_COST_STRUCT
{
	CRegionalMetaModel	*pModel;
	ZRealMatrix* pmxX;				//the gating vectors
	ZRealMatrix* pmxD;				//the search direction
	ZRealMatrix* pmxI;				//the inputs
	ZRealMatrix* pmxO;				//the outputs
	int			count;
};

REAL ModelPerformance( void* pParam, REAL lambda )
{
	MODEL_COST_STRUCT* pStruct = (MODEL_COST_STRUCT*)pParam;
	pStruct->count++;
	
	return pStruct->pModel->ComputePerformance( *pStruct->pmxI, *pStruct->pmxO, (*pStruct->pmxX)+lambda*(*pStruct->pmxD) );
}

/*
	conjugate gradient training with Polak-Ribi¨¨re formul
	1. initalize D = -Gd, Gd_old = Gd, D_old = D
	2. min( f(X + lambda*D) ), using one dimensional minimization
	3. X = X + lambda*D
	4. Gd = gradient( f(X) )
	5. beta = Gd' * (Gd-Gd_old) / (Gd_old'*Gd_old)
	6. D = -Gd + beta*D_old
	7. go to step 2.
*/

REAL CRegionalMetaModel::TrainGating3( const vector< vector<REAL> >& mxInputs, const vector< vector<REAL> >& mxOutputs, const vector< vector<REAL> >& mxValidInputs, const vector< vector<REAL> >& mxValidOutputs, int nMaxEpoch )
{
	int nPattern = mxInputs.size();
	int nInputs = GetInputs();
	int nOutputs = GetOutputs();
	int nModels = GetModelCount();
	//the vector dimension of the optimizaing weight vector
	int nDim = m_pmxGating->GetSize();

	//the training input and output matrix
	ZRealMatrix mxI(nPattern, nInputs);
	ZRealMatrix mxO(nPattern, nOutputs);
	ZRealMatrix mxR(nPattern, nModels);

	//copy the output to matrix mxX, mxO and compute the trust regions.
	for( int i=0; i<nPattern; i++ ){
		copy( mxInputs[i].begin(), mxInputs[i].end(), &mxI(i+1,1) );
		copy( mxOutputs[i].begin(), mxOutputs[i].end(), &mxO(i+1,1) );
		GetTrustRegionVector( &mxInputs[i][0], &mxR(i+1,1) );
	}

	//validation controls
	REAL rMinTestMSE = 1e100;
	ZRealMatrix mxBestGating = (*m_pmxGating);
	int nMaxDangleEpoch = 8;					//stop training if after nMaxDangleEpoch epochs without further decreasing validation mse
	int nLastDecreasingEpoch = 0;

	//The variables.
	ZRealMatrix mxX(nDim, 1);
	ZRealMatrix mxGd(nDim, 1);		//the current gradient direction.
	ZRealMatrix mxGd_old(nDim, 1);	//last gradient direction.
	ZRealMatrix mxD(nDim, 1);		//the current search direction.
	ZRealMatrix mxD_old(nDim, 1);	//last search direction.
	REAL rGdDot, rGdDot_old;		//the dot product of gradient. Gd'*Gd

	mxX.SetRandom( 0, 1.0 );
	//initialize the search direction and gradient
	mxGd = ComputeGradient( mxI, mxO, mxR, mxX );
	mxGd_old = mxGd;
	rGdDot = mxGd.DotMul( mxGd );
	rGdDot_old = rGdDot;
	mxD = -mxGd / sqrt(rGdDot);
	mxD_old = mxD;

	for( int epoch=1; epoch <= nMaxEpoch; epoch++ ){
		//the following section do validate
		//compute testing mse
		int nTestPattern = mxValidInputs.size();
		REAL rTestMSE = 0.0;
		for( int i=0; i<nTestPattern; i++ ){
			vector<REAL> vcTestOut(nOutputs);
			PredictEx( &mxValidInputs[i][0], &vcTestOut[0] );
			for( int k=0; k<nOutputs; k++ )rTestMSE += sqr(vcTestOut[k]-mxValidOutputs[i][k]);
		}
		rTestMSE = rTestMSE / nTestPattern / nOutputs;
		if( rMinTestMSE > rTestMSE ){
			mxBestGating = (*m_pmxGating);
			rMinTestMSE = rTestMSE;
			nLastDecreasingEpoch = epoch;
		}

		//compute the current training mse
		REAL rMSE = 0.0;
		for( i=0; i<nPattern; i++ ){
			vector<REAL> vcTestOut(nOutputs);
			PredictEx( &mxInputs[i][0], &vcTestOut[0] );
			for( int k=0; k<nOutputs; k++ )rMSE += sqr(vcTestOut[k]-mxOutputs[i][k]);
		}
		rMSE = rMSE / nPattern / nOutputs;
		::cout<<"MSE:"<<rMSE<<"\t"<<"TESTMSE:"<<rTestMSE<<endl;


//		::cout<<"Gd'*D:"<<mxGd.DotMul( mxD )<<"\t";

		MODEL_COST_STRUCT param;
		param.pModel = this;
		param.pmxI = &mxI;
		param.pmxO = &mxO;
		param.pmxD = &mxD;
		param.pmxX = &mxX;
		param.count = 0;
		//do one dimensional search and update X
//		REAL lambda = GoldSearch( 0.0, 2.0, ModelPerformance, &param );
		REAL lambda = BrentSearch( 0.0, 2.0, ModelPerformance, &param, 10 );
		mxX += lambda * mxD;

//		::cout<<"lambda:"<<lambda<<"\t";

		//compute the new gradient.
		mxGd = ComputeGradient( mxI, mxO, mxR, mxX );

		//compute the gradient dot product.
		REAL rGdDot = mxGd.DotMul( mxGd );

		//compute the Polak-Ribi¨¨re formula
		REAL beta = 0;
		if( epoch%nDim == 0 )
			beta = 0;			//reset the conjugate direction to gradient
		else
			beta = mxGd.DotMul( mxGd-mxGd_old ) / rGdDot_old;

		//compute the new direction and normalize it
		mxD = -mxGd + beta*mxD_old;
		mxD /= sqrt( mxD.DotMul(mxD) );

		//save the current direction, gradient
		mxD_old = mxD;
		mxGd_old = mxGd;
		rGdDot_old = rGdDot;

		//check for a descent direction, see matlab traincgp
		//mxD is a normalized vector. mxGd'*mxD compute the projection of mxGd on direciton mxD
		//the projection should be less than 0. If it is too big, then it is not a descent direction.
		REAL rProj = mxGd.DotMul( mxD );
		if( rProj >= -0.001*sqrt(rGdDot) ){
			mxD = -mxGd / sqrt(rGdDot);
			mxD_old = -mxGd;
		}

		if( epoch - nLastDecreasingEpoch >=nMaxDangleEpoch ){
			break;
		}
	}

	::cout<<endl;

	(*m_pmxGating) = mxBestGating;
	return rMinTestMSE;
}

REAL CRegionalMetaModel::TrainGating( const vector< vector<REAL> >& mxInputs, const vector< vector<REAL> >& mxOutputs, const vector< vector<REAL> >& mxValidInputs, const vector< vector<REAL> >& mxValidOutputs, int nMaxEpoch )
{
	int nPattern = mxInputs.size();
	int nInputs = GetInputs();
	int nOutputs = GetOutputs();
	int nModels = GetModelCount();

	//alpha is learning rate, beta is momentum correction rate
	REAL alpha = 0.02;
	REAL beta = 0.1;

	m_pmxGating->SetRandom( 0.0, 1.0 );
	ZRealMatrix mxBeta(nInputs, nModels, 0.0);

	REAL rMinTestMSE = 1e100;
	ZRealMatrix mxBestGating = (*m_pmxGating);

	int nMaxDangleEpoch = 50;					//stop training if after nMaxDangleEpoch epochs without further decreasing validation mse
	int nLastDecreasingEpoch = 0;

/*
	//for debugging.
	cdump<<"All the inputs"<<endl;
	for( int i=0; i<nPattern; i++ ){
		for( int j=0; j<nInputs; j++ )cdump<<mxInputs[i][j]<<" ";
		cdump<<endl;
	}
	cdump<<"All the outputs"<<endl;
	for( i=0; i<nPattern; i++ ){
		for( int j=0; j<nOutputs; j++ )cdump<<mxOutputs[i][j]<<" ";
		cdump<<endl;
	}
	cdump<<"Internal predictions"<<endl;
	ZRealMatrix mxYin(nModels, nOutputs);
	for( i=0; i<nPattern; i++ ){
		for( int j=0; j<nModels; j++ ){
			m_vcMetaModels[j]->Predict( &mxInputs[i][0], &mxYin(j+1, 1) );
		}
		cdump<<mxYin<<endl;
	}

	cdump<<"Trust Regions:"<<endl;
	ZRealMatrix mxRegions(nPattern, nModels);
	for( i=0; i<nPattern; i++ ){
		GetTrustRegionVector( &mxInputs[i][0], &mxRegions(i+1,1) );
	}
	cdump<<mxRegions<<endl;
	
	//compute testing mse
	int nTestPattern = mxValidInputs.size();
	REAL rTestMSE = 0.0;
	for( i=0; i<nTestPattern; i++ ){
		vector<REAL> vcTestOut(nOutputs);
		m_vcMetaModels[0]->Predict( &mxValidInputs[i][0], &vcTestOut[0] );
		for( int k=0; k<nOutputs; k++ )rTestMSE += sqr(vcTestOut[k]-mxValidOutputs[i][k]);
	}
	rTestMSE = rTestMSE / nTestPattern / nOutputs;
	::cout<<"Original TESTMSE:"<<rTestMSE<<endl;
*/

	for( int epoch=0; epoch<=nMaxEpoch; epoch++ ){

		//compute testing mse
		int nTestPattern = mxValidInputs.size();
		REAL rTestMSE = 0.0;
		for( int i=0; i<nTestPattern; i++ ){
			vector<REAL> vcTestOut(nOutputs);
			PredictEx( &mxValidInputs[i][0], &vcTestOut[0] );
			for( int k=0; k<nOutputs; k++ )rTestMSE += sqr(vcTestOut[k]-mxValidOutputs[i][k]);
		}
		rTestMSE = rTestMSE / nTestPattern / nOutputs;
		if( rMinTestMSE > rTestMSE ){
			mxBestGating = (*m_pmxGating);
			rMinTestMSE = rTestMSE;
			nLastDecreasingEpoch = epoch;
		}

		//compute the current training mse
		REAL rMSE = 0.0;
		for( i=0; i<nPattern; i++ ){
			vector<REAL> vcTestOut(nOutputs);
			PredictEx( &mxInputs[i][0], &vcTestOut[0] );
			for( int k=0; k<nOutputs; k++ )rMSE += sqr(vcTestOut[k]-mxOutputs[i][k]);
		}
		rMSE = rMSE / nPattern / nOutputs;
		::cout<<"MSE:"<<rMSE<<"\t"<<"TESTMSE:"<<rTestMSE<<endl;


		//update the gating network using gradient descent.
		ZRealMatrix mxDeta(nInputs, nModels, 0.0);
		for( i=0; i<nPattern; i++ ){
			mxDeta += ComputeDeta( mxInputs[i], mxOutputs[i] );
		}

		cdump<<"Final Deta:"<<endl<<mxDeta<<endl;
		ZRealMatrix mxNew = (*m_pmxGating) - alpha*mxDeta + beta*mxBeta;
		mxBeta = mxNew - (*m_pmxGating);
		(*m_pmxGating) = mxNew;

		if( epoch - nLastDecreasingEpoch >=nMaxDangleEpoch ){
			break;
		}
	}

	(*m_pmxGating) = mxBestGating;
	return rMinTestMSE;
}

void CRegionalMetaModel::TrainGatingNetwork( const vector< vector<REAL> >& mxInputs, const vector< vector<REAL> >& mxOutputs, const vector< vector<REAL> >& mxValidInputs, const vector< vector<REAL> >& mxValidOutputs )
{
	REAL rMinTestMSE = 1e100;
	ZRealMatrix mxBestGating = (*m_pmxGating);

	if( GetModelCount()>1 ){
		for( int i=0; i<2; i++ ){
			REAL rMSE = TrainGating3(mxInputs, mxOutputs, mxValidInputs, mxValidOutputs, 20);
			if( rMinTestMSE > rMSE ){
				rMinTestMSE = rMSE;
				mxBestGating = (*m_pmxGating);
			}
		}
	}else{
		//only one model, no gating is requried.
		mxBestGating.SetUniform( 0 );
	}

	(*m_pmxGating) = mxBestGating;

/*	ZRealMatrix mxTrain( nInputs, nPattern );
	for( int i=0; i<mxTrain.GetRow(); i++ ){
		for( int j=0; j<mxTrain.GetCol(); j++ ){
			mxTrain(j,i) = mxInputs[i][j];
		}
	}
	ZRealMatrix mxTrustTest( nPattern, nModels );
	for( i=0; i<nPattern; i++ ){
		vector<REAL> regs;
		GetTrustRegionVector( &mxInputs[i][0], regs );
		for( int k=0; k<nModels; k++ )mxTrustTest(k, i) = regs[k];
	}
	ZRealMatrix mxG(nModels, nPattern);

	ZRealMatrix mxTemp = ElemExp( m_pmxGating->GetTransposeMx() * mxTrain );	//mxTemp is [nModels, nPattern];
	for( i=0; i<mxTemp.GetRow(); i++ )
		for( int j=0; j<mxTemp.GetCol(); j++ )mxTemp(i,j) *= mxTrustTest(i,j);

	for( 


	//compute the internal model outputs for each model. Each model output matrix is [nOutputs, nPattern]
	ZRealMatrix mxModelOutputs[nModels];
	for( i=0; i<nModels; i++ ){
		mxModelOutputs[i].Create( nPattern, nOutputs );
		for( int k=0; k<nPattern; k++ ){
			m_vcMetaModels[i]->Predict( &mxInputs[k][0], &mxModelOutputs[i](k,0) );
		}
		mxModelOutputs[i].Transpose();
	}

	//compute the final model output
	ZRealMatrix mxY(nOutputs, nPattern);*/
}

void CRegionalNeuralModel::SetLayerNodes( int* prLayerNodes, int nLayers )
{
	ASSERT( nLayers>=3 );
	m_vcLayerNodes.assign( prLayerNodes, prLayerNodes+nLayers );

	SetInputs( m_vcLayerNodes[0] );
	SetOutputs( m_vcLayerNodes[nLayers-1] );
}

void CRegionalNeuralModel::SetTrainBias( REAL* prTrainBias )
{
	ASSERT( m_vcLayerNodes.size() >=3 );
	m_vcTrainBias.assign( prTrainBias, prTrainBias+GetOutputs()+1 );
}

CMetaModel* CRegionalNeuralModel::CreateMetaModel()
{
	CNeuralModel* pModel = new CNeuralModel();
	pModel->SetLayerNodes( &m_vcLayerNodes[0], m_vcLayerNodes.size() );
	pModel->SetTrainBias( &m_vcTrainBias[0] );
	return pModel;
}

void CRegionalNeuralModel::WriteTrainingFile( const char* strTrain, const char* strValid, vector< vector<REAL> >& vcInputs, vector< vector<REAL> >& vcOutputs, vector<int>& vcIds )
{
	DoWriteTrainingFile( strTrain, strValid, vcInputs, vcOutputs, vcIds, 0.0 );
}

CMetaModel* CRegionalSvmModel::CreateMetaModel()
{
	CSvmModel* pModel = new CSvmModel();
	pModel->SetInputs( GetInputs() );
	pModel->SetOutputs( GetOutputs() );

	for( int i=0; i<GetOutputs(); i++ )pModel->SetOptions( m_vcOptions[i], i );
	return pModel;
}

void CRegionalSvmModel::SetOptions( const string& strOptions, int id )
{
	ASSERT( id>=0 && id<GetOutputs() );

	if( m_vcOptions.size()<GetOutputs() )m_vcOptions.resize( GetOutputs() );
	m_vcOptions[id] = strOptions;
}


void CRegionalSvmModel::WriteTrainingFile( const char* strTrain, const char* strValid, vector< vector<REAL> >& vcInputs, vector< vector<REAL> >& vcOutputs, vector<int>& vcIds )
{
	//support vector machine doesn't need the valid file
	DoWriteTrainingFile( strTrain, strValid, vcInputs, vcOutputs, vcIds, 0 );
}
