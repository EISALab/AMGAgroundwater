#include <stdlib.h>
#include <memory.h>
#include <treetmpl.h>
#include <typeinfo.h>
#include <algorithm>
#include <math.h>
#include <functional>
#include <vector>
#include <map>
#include <numeric>
#include <iostream>
#include <fstream>
#include "commdef.h"
using namespace std;

/* 
	Modified on Aug 22, 2005
	m_arrSams member variabled is added. The array records the sampling number for each fitness. The default value is always 1.
	GetSampleCount () returns the sum of this array.
*/

/*
	Two chromosomes caches can be created. The only difference is their comparing functions.
	CREATECHROMCACHE strictly compares the chromosomes bits by bits.
	CREATECHROMCACHEEX needs a customized function to compare the chromosomes.
*/

bool operator < ( const pair<int, int>& _left, const pair<int, int>& _right )
{
	return _left.first < _right.first;
}

class ChromSet
{
	vector< pair<int, int> >	m_arrIds;	//the <sampling id, index> array of the evaluated fitness
	vector< vector<double> >	m_mxFits;	//the fitnesses. each row is the fitness array evaluated by sampled a id
	vector< int >				m_arrSamps; //the sample number array.
public:

	//insert a new fitness arrray with samplie id nId
	//if nId is already existed, the function return false.
	bool InsertFit( int nId, double* prFits, int nFits )
	{
		vector< pair<int,int> >::iterator pos = lower_bound(m_arrIds.begin(), m_arrIds.end(), make_pair<int,int>(nId,0) );
		if( pos!=m_arrIds.end() && pos->first==nId )return false;

		m_arrIds.insert( pos, make_pair<int,int>(nId, m_mxFits.size()) );
		m_mxFits.push_back( vector<double>(prFits, prFits+nFits) );
		m_arrSamps.push_back( 1 );
		return true;
	}
	//update the fitness arrray sampled by nId
	bool ReplaceFit( int nId, double* prFits, int nFits )
	{
		vector< pair<int,int> >::iterator pos = lower_bound(m_arrIds.begin(), m_arrIds.end(), make_pair<int,int>(nId,0) );
		if( pos==m_arrIds.end() || pos->first!=nId )return false;

		copy( prFits, prFits+nFits, m_mxFits[pos->second].begin() );
		return true;
	}
	//check if the sample id is already existed
	bool SearchId( int nId )
	{
		return binary_search( m_arrIds.begin(), m_arrIds.end(), make_pair<int,int>(nId,0) );
	}
	//retrieve a fitness array sampled by nId
	bool GetFit( int nId, double* prFits )
	{
		vector< pair<int,int> >::iterator pos = lower_bound(m_arrIds.begin(), m_arrIds.end(), make_pair<int,int>(nId,0) );
		if( pos!=m_arrIds.end() && pos->first==nId ){
			int index = pos->second;
			copy( m_mxFits[index].begin(), m_mxFits[index].end(), prFits );
			return true;
		}
		return false;
	}
	//return the total sampling number
	int GetSampleCount()
	{
		return accumulate( m_arrSamps.begin(), m_arrSamps.end(), 0 );
	}

	void GetSampleIds( int* pSampleIds )
	{
		vector< pair<int,int> >::iterator pos = m_arrIds.begin();
		for( ; pos!=m_arrIds.end(); pos++ ){
			*pSampleIds = pos->first;
			pSampleIds++;
		}
	}

	bool AddSamples( int nId )
	{
		vector< pair<int,int> >::iterator pos = lower_bound(m_arrIds.begin(), m_arrIds.end(), make_pair<int,int>(nId,0) );
		if( pos==m_arrIds.end() || pos->first!=nId )return false;

		m_arrSamps[ pos->second ] ++;
		return true;
	}
	bool RemoveSamples( int nId )
	{
		vector< pair<int,int> >::iterator pos = lower_bound(m_arrIds.begin(), m_arrIds.end(), make_pair<int,int>(nId,0) );
		if( pos==m_arrIds.end() || pos->first!=nId )return false;

		int nSamples = --m_arrSamps[ pos->second ];
		if( nSamples==0 ){
			int index = pos->second;
			m_mxFits.erase( m_mxFits.begin()+index );
			m_arrSamps.erase( m_arrSamps.begin()+index );
			//remove this sampling id.
			m_arrIds.erase( pos );

			//adjust the index in the m_arrIds array.
			for( pos=m_arrIds.begin(); pos!=m_arrIds.end(); pos++ ){
				if( pos->second > index )pos->second--;
			}
		}
		return true;
	}
	//return the number of items
	int GetCount()
	{
		return m_arrIds.size();
	}
	//return the mean and standard deviation of each fitness value.
	//the function returns false if there are not enough fitness arraies to compute the means or the standard deviations
	bool GetStatFit( double *prMeanFits, double* prStdFits, double* prMedianFits )
	{
//		if( GetCount()<=0 )return false;
		if( GetCount()<=0 )return false;

		int count = GetSampleCount();

		//sum the rows into vector means
		vector<double> means( m_mxFits.front().size(), 0.0 );
//		for( int i=0; i<m_mxFits.size(); i++ )
//			for( int j=0; j<m_mxFits[i].size(); j++ )means[j] += m_arrSamps[i]*m_mxFits[i][j];

		vector< vector<double> >::iterator iter;
		for( iter=m_mxFits.begin(); iter!=m_mxFits.end(); iter++ ){
//			transform( means.begin(), means.end(), iter->begin(), means.begin(), plus<double>() );
			transform( means.begin(), means.end(), iter->begin(), means.begin(),
				compose_f_gx_hy( plus<double>(), bind2nd( plus<double>(), 0.0 ),
								bind2nd( multiplies<double>(), m_arrSamps[iter-m_mxFits.begin()] ) )
								);
		}

		//divide by the number of rows, and copy the means to prFits.
//		transform( means.begin(), means.end(), means.begin(), bind2nd(divides<double>(), GetCount()) );
		transform( means.begin(), means.end(), means.begin(), bind2nd(divides<double>(), count) );
		if( prMeanFits!=NULL )
			copy( means.begin(), means.end(), prMeanFits );


		//compute the standard deviation on request
		if( prStdFits!=NULL && GetCount()>1 ){
			vector<double> stds( m_mxFits.front().size(), 0.0 );
			vector< vector<double> >::iterator iter;
			//compute sum( (x-u)*(x-u) ) for each fit column
			for( iter=m_mxFits.begin(); iter!=m_mxFits.end(); iter++ ){
				for( int i=0; i<stds.size(); i++ ){
//					stds[i] += sqr(means[i]-iter->at(i));
					stds[i] += m_arrSamps[iter-m_mxFits.begin()] * sqr(means[i]-iter->at(i));
				}
			}
			//using compose function operator to do "sqrt( sum/N-1 )"
			transform( stds.begin(), stds.end(), stds.begin(), 
				compose_f_gx( 
					ptr_fun(sqrt), bind2nd(divides<double>(), count-1) 
//					ptr_fun(sqrt), bind2nd(divides<double>(), GetCount()-1) 
				)
			);
//			transform( stds.begin(), stds.end(), stds.begin(), bind2nd(divides<double>(), GetCount()-1) );
//			transform( stds.begin(), stds.end(), stds.begin(), sqrt );
			copy( stds.begin(), stds.end(), prStdFits );
		}

		//compute the medians on requests
		if( prMedianFits!=NULL ){
			int rows = m_mxFits.size();
			int cols = m_mxFits[0].size();
			vector<double> arrSorts(rows);

			for( int i=0; i<cols; i++ ){
				for( int j=0; j<rows; j++ )arrSorts[j] = m_mxFits[j][i];
				sort( arrSorts.begin(), arrSorts.end(), less<double>() );
				prMedianFits[i] = arrSorts[ rows / 2 ];
			}
		}

		return true;
	}

	void Dump( ostream& ofg )
	{
		ofg<<"There are "<<GetCount()<<" samples, they are:"<<endl;
		vector< pair<int,int> >::iterator iter;
		for( iter=m_arrIds.begin(); iter!=m_arrIds.end(); iter++ ){
			ofg<<iter->first;
			int index = iter->second;
			ofg<<"("<<m_arrSamps[index]<<")--\t";
			copy( m_mxFits[index].begin(), m_mxFits[index].end(), ostream_iterator<double>(ofg, " ") );
			ofg<<endl;
		}
	}
};

extern "C" int  _stdcall COMPARECHROMS( const int* pChrom1, const int* pChrom2 );
extern "C" int  _stdcall CHROM_NORMALIZE( int* pChrom );

typedef vector<int>		CHROMBITS;
typedef vector<double>	CHROMFITS;
typedef ChromSet		CHROMFITSET;

typedef void(_stdcall *CHROMFUNC)( const int* pChroms, int& count );
typedef void(_stdcall *CHROM_NORM_FUNC)( int* pChroms );
CHROM_NORM_FUNC g_pChromNormFunc = NULL;

bool operator < ( const CHROMBITS& _left, const CHROMBITS& _right )
{
	for( int i=0; i<_left.size() && i<_right.size(); i++ )
		if( _left[i]<_right[i] )return true;
		else if( _left[i]>_right[i] ) return false;

	//_right has more bits than _left and previous bits are equal.
	if( _left.size() < _right.size() )return true;

	return false;
}

class less_real_chrom
	: public binary_function< CHROMBITS, CHROMBITS, bool >
{
public:
	bool operator()( const CHROMBITS& _left, const CHROMBITS& _right ) const
	{
		//the first number is sampling number.
//		if( _left[0] != _right[0] ) return _left<_right;
		if( g_pChromNormFunc!=NULL ){
			CHROMBITS left_comp = _left;
			CHROMBITS right_comp = _right;

			(*g_pChromNormFunc)( &left_comp[0] );
			(*g_pChromNormFunc)( &right_comp[0] );

			return left_comp < right_comp;
		}else{
			return _left < _right;
		}

		//Note: COMPARECHROMS return 1 if the two chromosomes are the same. and 0 if they are different.
		if( COMPARECHROMS( &_left[0], &_right[0] )==0 ){
			return _left < _right;
		}else{
			return false;
		}
	}
};

//typedef map< CHROMBITS, CHROMFITSET, less<CHROMBITS> > CHROMCACHE;
typedef map< CHROMBITS, CHROMFITSET, less_real_chrom > CHROMCACHE;
typedef map< CHROMBITS, CHROMFITSET, less_real_chrom > CHROMCACHEEX;

class op_save_node
{
private:
	CHROMFUNC m_ChromFunc;
	int m_nCount;
public:
	op_save_node( CHROMFUNC pFunc ):m_ChromFunc(pFunc),m_nCount(0){}
	void operator()( const pair<CHROMBITS, CHROMFITSET>& item )
	{
		(*m_ChromFunc)( &item.first[0], ++m_nCount );
	}
};


extern "C"{

	void _stdcall SETCHROMNORMFUNC( CHROM_NORM_FUNC pFunc )
	{
		g_pChromNormFunc = pFunc;
	}

	int _stdcall CREATECHROMCACHE()
	{
		CHROMCACHE* pChromCache = new CHROMCACHE();
		return (int)pChromCache;
	}
	int _stdcall CREATECHROMCACHEEX( )
	{
		CHROMCACHEEX* pChromCache = new CHROMCACHEEX();
		cout<<typeid(pChromCache).name()<<endl;
		return (int)pChromCache;
	}

	void _stdcall INSERTCHROMCACHE( CHROMCACHE*& pChromCache, int* pChrom, int& nChromLen, int& nSampleId, double* prFits, int& nFits )
	{
		CHROMBITS bits( pChrom, pChrom+nChromLen );
		CHROMFITSET& chmset = (*pChromCache)[bits];
		chmset.InsertFit( nSampleId, prFits, nFits );

//		CHROMFITS fits( prFits, prFits+nFits );
//		pChromCache->insert( make_pair(bits, fits) );
	}
	void _stdcall INSERTCHROMCACHEEX( CHROMCACHEEX*& pChromCache, int* pChrom, int& nChromLen, int& nSampleId, double* prFits, int& nFits )
	{
		CHROMBITS bits( pChrom, pChrom+nChromLen );
		CHROMFITSET& chmset = (*pChromCache)[bits];
		chmset.InsertFit( nSampleId, prFits, nFits );

//		CHROMFITS fits( prFits, prFits+nFits );
//		pChromCache->insert( make_pair(bits, fits) );
/*		if( typeid(pChromCache)==typeid(CHROMCACHE) ){
		if( typeid(pAvlChroms)==typeid(CAvlChromTree*) ){
			CChromItem* pItem = new CChromItem( pChrom, *pnChromLen, prFits, *pnFits );
			pAvlChroms->Insert( pItem );
		}else{
			CCustomAvlChromTree* pAvl = dynamic_cast<CCustomAvlChromTree*>(pAvlChroms);
			CChromItem* pItem = new CChromItem( pChrom, *pnChromLen, prFits, *pnFits );
			pAvl->Insert( pItem );
		}*/
	}

	int _stdcall SEARCHCHROMCACHE( CHROMCACHE*& pChromCache, int* pChrom, int& nChromLen, int& nSampleId, double* prFits, int& nFits )
	{
		CHROMBITS bits( pChrom, pChrom+nChromLen );
//		cout<<typeid(pChromCache).name()<<endl;

		CHROMCACHE::iterator pos;
		pos = pChromCache->find( bits );
		if( pos!=pChromCache->end() ){
			if( nSampleId==-1 )return TRUE;
			else return pos->second.GetFit( nSampleId, prFits );
//			ASSERT( pos->second.size()==nFits );
//			copy( pos->second.begin(), pos->second.end(), prFits );
//			return TRUE;
		}
		return FALSE;
	}
	int _stdcall SEARCHCHROMCACHEEX( CHROMCACHEEX*& pChromCache, int* pChrom, int& nChromLen, int &nSampleId, double* prFits, int& nFits )
	{
		CHROMBITS bits( pChrom, pChrom+nChromLen );
//		cout<<typeid(pChromCache).name()<<endl;

		CHROMCACHEEX::iterator pos;
		pos = pChromCache->find( bits );
		if( pos!=pChromCache->end() ){
			if( nSampleId==-1 )return TRUE;
			else return pos->second.GetFit( nSampleId, prFits );
//			ASSERT( pos->second.size()==nFits );
//			copy( pos->second.begin(), pos->second.end(), prFits );
//			return TRUE;
		}
		return FALSE;
	}

	void _stdcall REMOVECHROMCACHE( CHROMCACHE*& pChromCache, int* pChrom, int& nChromLen, int& nSampleId )
	{
		CHROMBITS bits( pChrom, pChrom+nChromLen );

		CHROMCACHE::iterator pos;
		pos = pChromCache->find( bits );
		if( pos!=pChromCache->end() ){
			pos->second.RemoveSamples( nSampleId );
		}
	}

	void _stdcall ADDSAMPLECHROMCACHE( CHROMCACHE*& pChromCache, int* pChrom, int& nChromLen, int &nSampleId )
	{
		CHROMBITS bits( pChrom, pChrom+nChromLen );

		CHROMCACHE::iterator pos;
		pos = pChromCache->find( bits );
		if( pos!=pChromCache->end() ){
			pos->second.AddSamples( nSampleId );
		}
	}
	void _stdcall ADDSAMPLECHROMCACHEEX( CHROMCACHEEX*& pChromCache, int* pChrom, int& nChromLen, int &nSampleId )
	{
		CHROMBITS bits( pChrom, pChrom+nChromLen );

		CHROMCACHEEX::iterator pos;
		pos = pChromCache->find( bits );
		if( pos!=pChromCache->end() ){
			pos->second.AddSamples( nSampleId );
		}
	}

	int _stdcall GETSAMPLECOUNTCACHE( CHROMCACHE*& pChromCache, int* pChrom, int& nChromLen )
	{
		CHROMBITS bits( pChrom, pChrom+nChromLen );
		CHROMCACHE::iterator pos = pChromCache->find( bits );
		if( pos!=pChromCache->end() ){
			return pos->second.GetSampleCount();
		}
		return 0;
	}

	int _stdcall GETSAMPLECOUNTCACHEEX( CHROMCACHEEX*& pChromCache, int* pChrom, int& nChromLen )
	{
		CHROMBITS bits( pChrom, pChrom+nChromLen );
		CHROMCACHEEX::iterator pos = pChromCache->find( bits );
		if( pos!=pChromCache->end() ){
			return pos->second.GetSampleCount();
		}
		return 0;
	}

	void _stdcall GETSAMPLEIDSCACHE( CHROMCACHE*& pChromCache, int* pChrom, int& nChromLen, int* pSampleIds )
	{
		CHROMBITS bits( pChrom, pChrom+nChromLen );
		CHROMCACHE::iterator pos = pChromCache->find( bits );
		if( pos!=pChromCache->end() ){
			pos->second.GetSampleIds( pSampleIds );
		}
		return;
	}

	double _stdcall GETMEANSAMPLECACHE( CHROMCACHE*& pChromCache )
	{
		int nTotCount = 0;
		CHROMCACHE::iterator pos = pChromCache->begin();
		for( ; pos!=pChromCache->end(); pos++ ){
			nTotCount += pos->second.GetSampleCount();
		}
		if( pChromCache->size()>0 )return (double)nTotCount/pChromCache->size();
		return 0;
	}
	double _stdcall GETMEANSAMPLECACHEEX( CHROMCACHEEX*& pChromCache )
	{
		int nTotCount = 0;
		CHROMCACHEEX::iterator pos = pChromCache->begin();
		for( ; pos!=pChromCache->end(); pos++ ){
			nTotCount += pos->second.GetSampleCount();
		}
		if( pChromCache->size()>0 )return (double)nTotCount/pChromCache->size();
		return 0;
	}

	int _stdcall GETSTATCHROMCACHE( CHROMCACHE*& pChromCache, int* pChrom, int& nChromLen, double* prMeanFits, double* prStdFits )
	{
		CHROMBITS bits( pChrom, pChrom+nChromLen );
		CHROMCACHE::iterator pos = pChromCache->find( bits );
		if( pos!=pChromCache->end() ){
			bool bOk = pos->second.GetStatFit( prMeanFits, prStdFits, NULL );
			return bOk ? TRUE : FALSE;
		}
		return FALSE;
	}
	int _stdcall GETSTATCHROMCACHEEX( CHROMCACHEEX*& pChromCache, int* pChrom, int& nChromLen, double* prMeanFits, double* prStdFits )
	{
		CHROMBITS bits( pChrom, pChrom+nChromLen );
		CHROMCACHEEX::iterator pos = pChromCache->find( bits );
		if( pos!=pChromCache->end() ){
			bool bOk = pos->second.GetStatFit( prMeanFits, prStdFits, prMeanFits );
			return bOk ? TRUE : FALSE;
		}
		return FALSE;
	}

	void _stdcall REPLACECHROMCACHE( CHROMCACHE*& pChromCache, int* pChrom, int& nChromLen, int& nSampleId, double* prFits, int& nFits )
	{
		CHROMBITS bits( pChrom, pChrom+nChromLen );
//		cout<<typeid(pChromCache).name()<<endl;

		CHROMCACHE::iterator pos;
		pos = pChromCache->find( bits );
		ASSERT( pos!=pChromCache->end() );
		if( pos!=pChromCache->end() ){
			pos->second.ReplaceFit( nSampleId, prFits, nFits );
//			ASSERT( pos->second.size()==nFits );
//			copy( prFits, prFits+nFits, pos->second.begin() );
		}
	}
	void _stdcall REPLACECHROMCACHEEX( CHROMCACHEEX*& pChromCache, int* pChrom, int& nChromLen, int& nSampleId, double* prFits, int& nFits )
	{
		CHROMBITS bits( pChrom, pChrom+nChromLen );
//		cout<<typeid(pChromCache).name()<<endl;

		CHROMCACHEEX::iterator pos;
		pos = pChromCache->find( bits );
		ASSERT( pos!=pChromCache->end() );
		if( pos!=pChromCache->end() ){
			pos->second.ReplaceFit( nSampleId, prFits, nFits );
//			ASSERT( pos->second.size()==nFits );
//			copy( prFits, prFits+nFits, pos->second.begin() );
		}
	}

	void _stdcall FOREACHCHROMCACHE( CHROMCACHE*& pChromCache, CHROMFUNC func )
	{
		for_each( pChromCache->begin(), pChromCache->end(), op_save_node(func) );
	}

	void _stdcall FOREACHCHROMCACHEEX( CHROMCACHEEX*& pChromCache, CHROMFUNC func )
	{
		for_each( pChromCache->begin(), pChromCache->end(), op_save_node(func) );
	}

	void _stdcall RELEASECHROMCACHE( CHROMCACHE*& pChromCache )
	{
		delete pChromCache;
	}
	void _stdcall RELEASECHROMCACHEEX( CHROMCACHEEX*& pChromCache )
	{
		delete pChromCache;
	}
	void _stdcall DUMP( CHROMCACHE*& pChromCache )
	{
		CHROMCACHE::iterator iter;
		for( iter=pChromCache->begin(); iter!=pChromCache->end(); iter++ ){
			cout<<"Node has chromosomes: "<<endl;
			copy( iter->first.begin(), iter->first.end(), ostream_iterator<int>(cout, " ") );
			cout<<endl;
			iter->second.Dump( cout );
		}
	}
	void _stdcall DUMPCHROM( CHROMCACHE*& pChromCache, int* pChrom, int& nChromLen )
	{
		CHROMBITS bits( pChrom, pChrom+nChromLen );
		CHROMCACHE::iterator pos = pChromCache->find( bits );
		if( pos!=pChromCache->end() ){
			cout<<"Node has chromosomes: "<<endl;
			copy( pos->first.begin(), pos->first.end(), ostream_iterator<int>(cout, " ") );
			cout<<endl;
			ofstream ofg("chrom.gen", ios_base::app);
			pos->second.Dump( ofg );
		}
	}
	void _stdcall DUMPEX( CHROMCACHEEX*& pChromCache )
	{
		CHROMCACHEEX::iterator iter;
		for( iter=pChromCache->begin(); iter!=pChromCache->end(); iter++ ){
			cout<<"Node has chromosomes: "<<endl;
			copy( iter->first.begin(), iter->first.end(), ostream_iterator<int>(cout, " ") );
			cout<<endl;
			iter->second.Dump( cout );
		}
	}
}

