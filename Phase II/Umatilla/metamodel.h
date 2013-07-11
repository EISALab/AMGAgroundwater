#pragma once

#include "mininnet.h"

#include <list>
#include <algorithm>
#include <vector>
#include <iterator>
#include <numeric>
#include "ofdump.h"
#include "matlab.h"

#include <fstream>
#include <iostream>
#include <strstream>
#include <iomanip>
#include "svm.h"
#include "mathbase.h"
#include <matrixio.h>

using namespace std;

typedef pair<int, REAL> dist_pair;
typedef vector< dist_pair > dist_pair_vector;

#define _MAX_LINE 512

#define REG_TRAIN_FILE "reg_train.dat"
#define REG_VALID_FILE "reg_valid.dat"
#define TRAIN_FILE	"train_set.dat"
#define VALID_FILE	"valid_set.dat"
#define MODEL_FILE	"model_set.dat"
#define TEST_FILE	"test.dat"
#define NET_CMD		"univnet"
#define SVM_TRAIN_CMD	"svmtrain"
#define SVM_PREDICT_CMD	"svmpredict"
#define SVM_TRAIN_FILE	"svmtrain.dat"
#define SVM_PREDICT_FILE	"svmpred.dat"

inline REAL uni_rand()
{
	return (REAL)(rand())/RAND_MAX;
}

typedef struct svm_node		SVM_NODE;
typedef struct svm_model	SVM_MODEL;

inline void svm_predict( SVM_MODEL* model, const REAL* prInputs, int nInputs, REAL* prOutput )
{
	SVM_NODE *x = (SVM_NODE *)new SVM_NODE[nInputs+1];
	for( int i=0; i<nInputs; i++ ){
		x[i].index = i+1;
		x[i].value = prInputs[i];
	}
	x[i].index = -1;

	*prOutput = svm_predict(model,x);
	delete[] x;
}

class CTrustRegion
{
public:
	vector<REAL> m_vcMins;
	vector<REAL> m_vcMaxs;
	vector<REAL> m_vcStds;
	vector<REAL> m_vcMeans;
	vector<REAL> m_vcRadius;

	vector<REAL> m_ptCen;
	int			m_nModelId;
public:
	CTrustRegion(){};
	~CTrustRegion(){};

	int GetModelId()const { return m_nModelId; }
	void SetModelId( int nModelId ){ m_nModelId = nModelId; }

	//check if a point is within a max - min range at each dimension
	bool PtInRegion( const vector<REAL>& pt ) const
	{
		ASSERT( pt.size()==m_vcMins.size() && pt.size()==m_vcMaxs.size() );

		int ndim = pt.size();
		for( int i=0; i<ndim; i++ ){
			if( pt[i]<m_vcMins[i] || pt[i]>m_vcMaxs[i] )return false;
		}
		return true;
	}

	//this version assume this is a sphere, and check if the point is within radius to the center point.
	bool PtInRegion2( const vector<REAL>& pt ) const
	{
		ASSERT( pt.size()==m_ptCen.size() && pt.size()==m_vcRadius.size() );

		int ndim = pt.size();
		for( int i=0; i<ndim; i++ ){
			if( fabs(pt[i]-m_ptCen[i])>m_vcRadius[i] )return false;
		}
		return true;
	}

	REAL PtInRegionProb( const vector<REAL>& pt )const;

	void Create( const vector<REAL>& vcMins, const vector<REAL>& vcMaxs, const vector<REAL>& vcStds,
					const vector<REAL>& vcMeans, const vector<REAL>& ptCen, const vector<REAL>& vcRadius )
	{
		m_vcMins = vcMins;
		m_vcMaxs = vcMaxs;
		m_vcMeans = vcMeans;
		m_vcStds = vcStds;
		m_ptCen = ptCen;
		m_vcRadius = vcRadius;
	}

	REAL GetSphereRadius() const
	{
		return accumulate( m_vcRadius.begin(), m_vcRadius.end(), 0.0 ) / m_vcRadius.size();
	}
};


class CMetaModel
{
private:
	int m_nInputs;
	int m_nOutputs;
public:
	CMetaModel(){};
	CMetaModel( int nInputs, int nOutputs ):m_nInputs(nInputs),m_nOutputs(nOutputs){};
	virtual ~CMetaModel(){};

	int GetInputs(){ return m_nInputs; }
	void SetInputs( int nInputs ){ m_nInputs = nInputs; }
	int GetOutputs(){ return m_nOutputs; }
	void SetOutputs( int nOutputs ){ m_nOutputs = nOutputs; }

	virtual int GetModelCount(){ return 1; }	//the number of internal models
	virtual void Train( const string& strTrainData, const string& strValidData ){};
	virtual void Predict( const REAL* prInputs, REAL* prOutputs ){};
	virtual REAL Test( const REAL* prInputs, const REAL* prOutputs, int count );
};

class CNeuralModel : public CMetaModel
{
private:
	vector<int> m_vcLayerNodes;
	vector<REAL> m_vcTrainBias;

	CMiniNeuralNet* m_pNeuralNet;
public:
	CNeuralModel():m_pNeuralNet(NULL){}
	CNeuralModel( int* pLayerNodes, int nLayers );
	virtual ~CNeuralModel(){ if( m_pNeuralNet )delete m_pNeuralNet; }

	void SetLayerNodes( int* prNeuralNodes, int nLayers );
	void SetTrainBias( REAL* prTrainBias );
	virtual void Train( const string& strTrainData, const string& strValidData );
	virtual void Predict( const REAL* prInputs, REAL* prOutputs );
};

class CSvmModel : public CMetaModel
{
	static int m_nNextModelId;
	vector<string> m_vcModelFiles;
	vector<string> m_vcOptions;
	vector<SVM_MODEL*> m_vcModels;
public:

	CSvmModel();
	virtual ~CSvmModel(){ Release(); }

	void Release(){
		m_vcModelFiles.clear();
		for( int i=0; i<m_vcModels.size(); i++ ){
			svm_destroy_model( m_vcModels[i] );
		}
		m_vcModels.clear();
	}

	string CreateUniqueModelName();
	string GetOptions( int idOut );
	void SetOptions( const string& strOptions, int idOut );

	virtual void Train( const string& strTrainData, const string& strValidData );
	virtual void Predict( const REAL* prInputs, REAL* prOutputs );

	//transfrom the data file from (inputs columns + output columns) to svm acceptable format
	//nInputs is the number of input columns in strDataFile. nOutputs is the number of output columns in strDataFile
	//nPredId is the index of the output column that will appear in the first column of svm format file.
	//if nOutputs is 0, then no output columns will be written, nPredId is ignored in this case.
	//strOutput is the output file.
	void TransformToSvmData( const string& strDataFile, int nInputs, int nOutputs, int nPredId, const string& strOutput );
	void TransformSvmLine( ofstream& ofg, const vector<REAL>& vcInputs, const vector<REAL>& vcOutputs, int nPredId );

	void RunSvmTrain( const string& strTrainFile, const string& strOptions, const string& strModelFile );
	void RunSvmPredict( const string& strDataFile, const string& strModelFile, REAL& rOutput );
};

class CRegionalMetaModel : public CMetaModel
{
private:
	vector< CTrustRegion* > m_vcRegions;
	vector< CMetaModel* > m_vcMetaModels;
	vector< REAL >	m_vcPtCen;
	int				m_nMinCutPts;	//the cluster set is 2*m_nMinCutPts.
	ZRealMatrix*	m_pmxGating;	//the gating network

	void Release();

	void FindTrustRegions( const REAL* prInput, vector<CTrustRegion*>& hitRegions );
	void FindBestRegion( const REAL* prInput, vector<CTrustRegion*>& hitRegions );
	void GetTrustRegionVector( const REAL* prInput, REAL* prRegs );

public:
	CRegionalMetaModel():m_pmxGating(NULL){};
	CRegionalMetaModel( int nInputs, int nOutputs, REAL* prPtCen, int nMinCutPts ):m_pmxGating(NULL){
		SetInputs( nInputs );
		SetOutputs( nOutputs );
		SetMinCutPts( nMinCutPts );
		SetCenterPt( prPtCen );
	}
	virtual ~CRegionalMetaModel(){ Release(); }

	void SetMinCutPts( int nMinCutPts ){ m_nMinCutPts = nMinCutPts; }
	void SetCenterPt( REAL* prPtCen ){ m_vcPtCen.assign( prPtCen, prPtCen+GetInputs() ); }

	void DoWriteTrainingFile( const char* strTrain, const char* strValid, vector< vector<REAL> >& vcInputs, vector< vector<REAL> >& vcOutputs, vector<int>& vcIds, REAL rValidRatio );
	virtual CMetaModel* CreateMetaModel()=0;
	virtual void WriteTrainingFile( const char* strTrain, const char* strValid, vector< vector<REAL> >& vcInputs, vector< vector<REAL> >& vcOutputs, vector<int>& vcIds )=0;

	virtual int GetModelCount(){ return m_vcMetaModels.size(); }
	virtual void Train( const string& strTrainData, const string& strValidData );
	virtual void Predict( const REAL* prInputs, REAL* prOutput );

	void ComputeTrustRegion( CTrustRegion* pRegion, vector< vector<REAL> >& vcInputs, vector<int>& vcIds );
	void ComputeDistance( vector< vector<REAL> >& vcInputs, vector<REAL>& vcPtCen, dist_pair_vector& vcDists );
	vector<int> SampleIds( const dist_pair_vector& vcDists, int nSamples );
	void ReadTrainingData( const string& strDataFile, int nInputs, int nOutputs, vector< vector<REAL> >& vcInputs, vector< vector<REAL> >& vcOutputs );
	void SubdividTrainingData( dist_pair_vector& vcDists, vector< vector<int> >& vcIdSet, int min_pts );
	void SubdividTrainingData2( dist_pair_vector& vcDists, vector< vector<int> >& vcIdSet, int min_pts );

	void TrainGatingNetwork( const vector< vector<REAL> >& mxInputs, const vector< vector<REAL> >& mxOuputs, const vector< vector<REAL> >& mxValidIntputs, const vector< vector<REAL> >& mxValidOutputs );
	//initialize the gating network and train until nMaxEpoch or after 50 epochs without decreasing testing MSE.
	//return the final TEST MSE.
	REAL TrainGating( const vector< vector<REAL> >& mxInputs, const vector< vector<REAL> >& mxOuputs, const vector< vector<REAL> >& mxValidIntputs, const vector< vector<REAL> >& mxValidOutputs, int nMaxEpoch );
	//compute the gradient for one input-output pair.
	ZRealMatrix ComputeDeta( const vector<REAL>& mxInputs, const vector<REAL>& mxOutputs );

	//compute the gradient for the gating matrix
	ZRealMatrix ComputeGradient( ZRealMatrix& mxX, const ZRealMatrix& mxO, const ZRealMatrix& mxR, const ZRealMatrix& mxGate );
	REAL ComputePerformance( ZRealMatrix& mxX, const ZRealMatrix& mxO,  const ZRealMatrix& mxGate );

	//initialize the gating network and train until nMaxEpoch or after 50 epochs without decreasing testing MSE.
	//return the final TEST MSE.
	REAL TrainGating2( const vector< vector<REAL> >& mxInputs, const vector< vector<REAL> >& mxOutputs, const vector< vector<REAL> >& mxValidInputs, const vector< vector<REAL> >& mxValidOutputs, int nMaxEpoch );

	REAL TrainGating3( const vector< vector<REAL> >& mxInputs, const vector< vector<REAL> >& mxOutputs, const vector< vector<REAL> >& mxValidInputs, const vector< vector<REAL> >& mxValidOutputs, int nMaxEpoch );

	void PredictEx( const REAL* prInputs, REAL* prOutputs );

};

class CRegionalNeuralModel : public CRegionalMetaModel
{
	vector<int> m_vcLayerNodes;
	vector<REAL> m_vcTrainBias;
public:
	CRegionalNeuralModel(){};
	CRegionalNeuralModel( int* prLayerNodes, int nLayers ){ SetLayerNodes(prLayerNodes, nLayers); }
	virtual ~CRegionalNeuralModel(){}

	void SetLayerNodes( int* prLayerNodes, int nLayers );
	void SetTrainBias( REAL* prTrainBias );

	virtual CMetaModel* CreateMetaModel();
	virtual void WriteTrainingFile( const char* strTrain, const char* strValid, vector< vector<REAL> >& vcInputs, vector< vector<REAL> >& vcOutputs, vector<int>& vcIds );
};

class CRegionalSvmModel : public CRegionalMetaModel
{
private:
	vector<string> m_vcOptions;
	string	m_strOptions;
public:
	CRegionalSvmModel(){}

	void SetOptions( const string& strOptions, int id );
	virtual CMetaModel* CreateMetaModel();
	virtual void WriteTrainingFile( const char* strTrain, const char* strValid, vector< vector<REAL> >& vcInputs, vector< vector<REAL> >& vcOutputs, vector<int>& vcIds );
};