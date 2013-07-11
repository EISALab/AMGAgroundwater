#include "metamodel.h"
#include "nnsgaHelperC.h"

#define		MODEL_NEURAL			0
#define		MODEL_SVM				1
#define		MODEL_REGIONAL_NEURAL	2
#define		MODEL_REGIONAL_SVM		3

#define		MAXPATH		512

extern "C"{
	int _stdcall CREATE_META_MODEL( int& nModelType )
	{
		CMetaModel* pModel = NULL;
		switch( nModelType ){
		case MODEL_NEURAL:
			pModel = new CNeuralModel();
			break;
		case MODEL_SVM:
			pModel = new CSvmModel();
			break;
		case MODEL_REGIONAL_NEURAL:
			pModel = new CRegionalNeuralModel();
			break;
		case MODEL_REGIONAL_SVM:
			pModel = new CRegionalSvmModel();
			break;
		}
		return (int)pModel;
	}

	void _stdcall INIT_NEURAL_MODEL( CMetaModel*& pModel, int* pnNodes, int& nLayers, double* prNetBias )
	{
		ASSERT( NULL!=pModel );
		CNeuralModel* pNeuralModel = (CNeuralModel*)pModel;
		pNeuralModel->SetLayerNodes( pnNodes, nLayers );
		pNeuralModel->SetTrainBias( prNetBias );
	}

	void _stdcall INIT_SVM_MODEL( CMetaModel*& pModel, int& nInputs, int& nOutputs )//, char* pstrOptions, int nChars )
	{
		ASSERT( NULL!=pModel );
		CSvmModel* pSvmModel = (CSvmModel*)pModel;
		pSvmModel->SetInputs( nInputs );
		pSvmModel->SetOutputs( nOutputs );

//		char strOptions[_MAX_LINE];
//		strftoc( pstrOptions, nChars, strOptions );
//		pSvmModel->SetOptions( strOptions );
	}

	void _stdcall SET_SVM_OPTIONS( CMetaModel*& pModel, char* pstrOptions, int nChars, int& nId )
	{
		ASSERT( NULL!=pModel );
		CSvmModel* pSvmModel = (CSvmModel*)pModel;

		char strOptions[_MAX_LINE];
		strftoc( pstrOptions, nChars, strOptions );
		pSvmModel->SetOptions( strOptions, nId );
	}
			
	void _stdcall INIT_REGIONAL_NEURAL_MODEL( CMetaModel*& pModel, double* prPtCen, int& nMinCutPts, int* pnNodes, int& nLayers, double* prNetBias )
	{
		ASSERT( NULL!=pModel );
		CRegionalNeuralModel* pRegNeuralModel = (CRegionalNeuralModel*)pModel;
		//first set layer nodes, it automatically setup the inputs and outputs
		pRegNeuralModel->SetLayerNodes( pnNodes, nLayers );
		pRegNeuralModel->SetCenterPt( prPtCen );
		pRegNeuralModel->SetMinCutPts( nMinCutPts );
		pRegNeuralModel->SetTrainBias( prNetBias );
	}

	void _stdcall INIT_REGIONAL_SVM_MODEL( CMetaModel*& pModel, double* prPtCen, int& nMinCutPts, int& nInputs, int& nOutputs )//, char* pstrOptions, int nChars )
	{
		ASSERT( NULL!=pModel );
		CRegionalSvmModel* pRegSvmModel = (CRegionalSvmModel*)pModel;
		pRegSvmModel->SetInputs( nInputs );
		pRegSvmModel->SetOutputs( nOutputs );
		pRegSvmModel->SetMinCutPts( nMinCutPts );
		pRegSvmModel->SetCenterPt( prPtCen );

//		char strOptions[_MAX_LINE];
//		strftoc( pstrOptions, nChars, strOptions );
//		pRegSvmModel->SetOptions( strOptions );
	}
			
	void _stdcall SET_REGIONAL_SVM_OPTIONS( CMetaModel*& pModel, char* pstrOptions, int nChars, int& nId )
	{
		ASSERT( NULL!=pModel );
		CRegionalSvmModel* pRegSvmModel = (CRegionalSvmModel*)pModel;

		char strOptions[_MAX_LINE];
		strftoc( pstrOptions, nChars, strOptions );
		pRegSvmModel->SetOptions( strOptions, nId );
	}

	void _stdcall RELEASE_META_MODEL( CMetaModel*& pModel )
	{
		delete pModel;
	}

	void _stdcall TRAIN_META_MODEL( CMetaModel*& pModel, char* pstrDataFile, int nChars, char* pstrValidFile, int nValidChars )
	{
		char strTrainFile[MAXPATH];
		strftoc( pstrDataFile, nChars, strTrainFile );
		char strValidFile[MAXPATH];
		strftoc( pstrValidFile, nValidChars, strValidFile );

		pModel->Train( strTrainFile, strValidFile );
	}

	void _stdcall PREDICT_META_MODEL( CMetaModel*& pModel, REAL* prInput, REAL* prOutput )
	{
		pModel->Predict( prInput, prOutput );
	}

	double _stdcall TEST_META_MODEL( CMetaModel*& pModel, REAL* prInputs, REAL* prOutputs, int& count )
	{
		return pModel->Test( prInputs, prOutputs, count );
	}

	int _stdcall GET_MODEL_COUNT( CMetaModel*& pModel )
	{
		return pModel->GetModelCount();
	}


/*	void _stdcall TRAIN_META_MODEL( CMetaModel*& pModel, char* pstrDataFile, int nChars, double* prPtCen, int* pnNodes, int& nLayers, double* prNetBias, int& nMinPts )
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

	}*/
}

