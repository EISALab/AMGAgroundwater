// neuranet.h: interface for the ZNetLayer class.
//
//////////////////////////////////////////////////////////////////////

#if !defined(AFX_NEURANET_H__EAB2B050_4721_40B2_826B_778B826222AC__INCLUDED_)
#define AFX_NEURANET_H__EAB2B050_4721_40B2_826B_778B826222AC__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include <windows.h>
#include <mathbase.h>
#include <matrixio.h>
#include <list>

#if _MSC_VER<=1200		//for vc 6.0++, using the old iostream.h
	#include <fstream.h>
	#include <iostream.h>
#else					//for higher version, using the iostream
	#include <fstream>
	#include <iostream>
#endif

typedef ZRealMatrix ZMatrix;

typedef void (*NEUROFUN)(ZMatrix* pmx, LPVOID lpParam);
typedef REAL (*NEURODFUN)( REAL r );

class CMiniNeuralNet
{
public:
	class CNetLayer
	{
	public:
		int m_nNeuros;				//number of neuros in the layer
		ZMatrix* m_pmxWeight;		//weight of neural network
		ZMatrix* m_pmxOutput;		//output of every layer, using by trainning
	public:
		CNetLayer( int nInput );
		CNetLayer( int nInput, int nNeuros );
		virtual ~CNetLayer();

		void SetOutput( const REAL *prData );
		void GetOutput( REAL *prData );
	};
	typedef std::list<CNetLayer*> CMiniNetLayerList;

public:
	CMiniNetLayerList m_lstLayers;
	NEUROFUN m_pFunNeuro;

	//the following are used to scale input and output
	REAL m_rNeuroScaleMin;					//the minimum of neuro active function
	REAL m_rNeuroScaleMax;					//the maxmium of neuro active function
	REAL m_rOutScaleMin;					//the minimum of output
	REAL m_rOutScaleMax;					//the maxmium of output

//construction
public:
	CMiniNeuralNet();
	virtual ~CMiniNeuralNet();

//helper functions
public:
	void CopyWeightTo( ZMatrix& mxWeight );
	void CopyWeightFrom( const ZMatrix& mxWeight );
	int GetNetWeightSize();

public:
//neural network creation
	void Create( int *pnNeuros, int nLayers, NEUROFUN funNeuro=NULL );

//initialization: load neural network weights from mablab result
	void LoadMatlabNet( char* strWeightFile );

//property
	int GetLayerCount();
	int GetHiddenCount();
	CNetLayer* GetInputLayer();
	CNetLayer* GetOutputLayer();
	int GetInputCount();
	int GetOutputCount();
	void SetInput( const REAL *prInput );
	void GetInput( REAL *prInput );
	void GetOutput( REAL *prOutput );

	void SetNeuroScaler( REAL rMin, REAL rMax );
	void GetNeuroScaler( REAL& rMin, REAL& rMax );

public:
//train functions
	void ForwardPropagate( REAL* prInput=NULL );
//network simulate and test
	void Simulate(  const REAL* prInput, REAL* prOutput );
	REAL GetMeanSquareError( REAL* prInput, REAL* prOutput, int nPattern );
	REAL GetRootMeanSquareError( REAL* prInput, REAL* prOutput, int nPattern );

//helper functions for input and ouput data scale
	void ScaleIn( REAL* pInput, int nCount, REAL rMin, REAL rMax );
	void ScaleOut( REAL* pOutput, int nCount, REAL rMin, REAL rMax );

//debug only
	void TraceNetwork();

//io interface
	void SaveToFile( ofstream& ofg );
	void CreateFromFile( ifstream& ifg );
	void CreateFromFile( char* strNetFile );
	void SaveToFile( char* strNetFile );
};

void NeuroSigmoid( ZMatrix* pmx, LPVOID lpParam );
void NeuroTanSig( ZMatrix* pmx, LPVOID lpParam );

#endif // !defined(AFX_NEURANET_H__EAB2B050_4721_40B2_826B_778B826222AC__INCLUDED_)
