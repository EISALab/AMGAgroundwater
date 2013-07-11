#include <stdlib.h>
#include <memory.h>
#include <treetmpl.h>
#include <typeinfo.h>
#include <algorithm>
#include <math.h>
#include <functional>
#include <vector>
#include <map>
#include <iostream>
#include "commdef.h"
using namespace std;

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
public:

	//insert a new fitness arrray with samplie id nId
	//if nId is already existed, the function return false.
	bool InsertFit( int nId, double* prFits, int nFits )
	{
		vector< pair<int,int> >::iterator pos = lower_bound(m_arrIds.begin(), m_arrIds.end(), make_pair<int,int>(nId,0) );
		if( pos!=m_arrIds.end() && pos->first==nId )return false;

		m_arrIds.insert( pos, make_pair<int,int>(nId, m_mxFits.size()) );
		m_mxFits.push_back( vector<double>(prFits, prFits+nFits) );
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
	//return the number of samples
	int GetCount()
	{
		return m_arrIds.size();
	}
	//return the mean and standard deviation of each fitness value.
	//the function returns false if there are not enough fitness arraies to compute the means or the standard deviations
	bool GetStatFit( double *prMeanFits, double* prStdFits )
	{
		if( GetCount()<=0 )return false;

		//sum the rows into vector means
		vector<double> means( m_mxFits.front().size(), 0.0 );
		vector< vector<double> >::iterator iter;
		for( iter=m_mxFits.begin(); iter!=m_mxFits.end(); iter++ ){
			transform( means.begin(), means.end(), iter->begin(), means.begin(), plus<double>() );
		}

		//divide by the number of rows, and copy the means to prFits.
		transform( means.begin(), means.end(), means.begin(), bind2nd(divides<double>(), GetCount()) );
		if( prMeanFits!=NULL )
			copy( means.begin(), means.end(), prMeanFits );

		//compute the standard deviation on request
		if( prStdFits!=NULL && GetCount()>1 ){
			vector<double> stds( m_mxFits.front().size(), 0.0 );
			vector< vector<double> >::iterator iter;
			//compute sum( (x-u)*(x-u) ) for each fit column
			for( iter=m_mxFits.begin(); iter!=m_mxFits.end(); iter++ ){
				for( int i=0; i<stds.size(); i++ ){
					stds[i] += sqr(means[i]-iter->at(i));
				}
			}
			//using compose function operator to do "sqrt( sum/N-1 )"
			transform( stds.begin(), stds.end(), stds.begin(), 
				compose_f_gx( 
					ptr_fun(sqrt), bind2nd(divides<double>(), GetCount()-1) 
				)
			);
//			transform( stds.begin(), stds.end(), stds.begin(), bind2nd(divides<double>(), GetCount()-1) );
//			transform( stds.begin(), stds.end(), stds.begin(), sqrt );
			copy( stds.begin(), stds.end(), prStdFits );
		}
		return true;
	}

/*	void Dump()
	{
		cout<<"The Ids are:"<<endl;
		for( int i=0; i<m_arrIds.size(); i++ )cout<<m_arrIds[i].first<<" ";
		cout<<endl;

		cout<<"The Fits are:"<<endl;
		for( int i=0; i<m_mxFits.size(); i++ ){
			int index = m_arrIds[i].second;
			copy( m_mxFits[index].begin(), m_mxFits[index].end(), ostream_iterator<double>(cout, " ") );
			cout<<endl;
		}
	}*/
};


extern "C" int  _stdcall COMPARECHROMS( const int* pChrom1, const int* pChrom2 );

typedef vector<int>		CHROMBITS;
typedef vector<double>	CHROMFITS;

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
		if( _left[0] != _right[0] ) return _left<_right;

		//Note: COMPARECHROMS return 1 if the two chromosomes are the same. and 0 if they are different.
		if( COMPARECHROMS( &_left[1], &_right[1] )==0 ){
			return _left < _right;
		}else{
			return false;
		}
	}
};

typedef map< CHROMBITS, CHROMFITS, less<CHROMBITS> > CHROMCACHE;
typedef map< CHROMBITS, CHROMFITS, less_real_chrom > CHROMCACHEEX;

extern "C"{

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

	void _stdcall INSERTCHROMCACHE( CHROMCACHE*& pChromCache, int* pChrom, int& nChromLen, double* prFits, int& nFits )
	{
		CHROMBITS bits( pChrom, pChrom+nChromLen );
		CHROMFITS fits( prFits, prFits+nFits );
		pChromCache->insert( make_pair(bits, fits) );
	}
	void _stdcall INSERTCHROMCACHEEX( CHROMCACHEEX*& pChromCache, int* pChrom, int& nChromLen, double* prFits, int& nFits )
	{
		CHROMBITS bits( pChrom, pChrom+nChromLen );
		CHROMFITS fits( prFits, prFits+nFits );
		pChromCache->insert( make_pair(bits, fits) );
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

	int _stdcall SEARCHCHROMCACHE( CHROMCACHE*& pChromCache, int* pChrom, int& nChromLen, double* prFits, int& nFits )
	{
		CHROMBITS bits( pChrom, pChrom+nChromLen );
//		cout<<typeid(pChromCache).name()<<endl;

		CHROMCACHE::iterator pos;
		pos = pChromCache->find( bits );
		if( pos!=pChromCache->end() ){
			ASSERT( pos->second.size()==nFits );
			copy( pos->second.begin(), pos->second.end(), prFits );
			return TRUE;
		}
		return FALSE;
	}
	int _stdcall SEARCHCHROMCACHEEX( CHROMCACHEEX*& pChromCache, int* pChrom, int& nChromLen, double* prFits, int& nFits )
	{
		CHROMBITS bits( pChrom, pChrom+nChromLen );
//		cout<<typeid(pChromCache).name()<<endl;

		CHROMCACHEEX::iterator pos;
		pos = pChromCache->find( bits );
		if( pos!=pChromCache->end() ){
			ASSERT( pos->second.size()==nFits );
			copy( pos->second.begin(), pos->second.end(), prFits );
			return TRUE;
		}
		return FALSE;
	}

	void _stdcall REPLACECHROMCACHE( CHROMCACHE*& pChromCache, int* pChrom, int& nChromLen, double* prFits, int& nFits )
	{
		CHROMBITS bits( pChrom, pChrom+nChromLen );
//		cout<<typeid(pChromCache).name()<<endl;

		CHROMCACHE::iterator pos;
		pos = pChromCache->find( bits );
		ASSERT( pos!=pChromCache->end() );
		if( pos!=pChromCache->end() ){
			ASSERT( pos->second.size()==nFits );
			copy( prFits, prFits+nFits, pos->second.begin() );
		}
	}
	void _stdcall REPLACECHROMCACHEEX( CHROMCACHEEX*& pChromCache, int* pChrom, int& nChromLen, double* prFits, int& nFits )
	{
		CHROMBITS bits( pChrom, pChrom+nChromLen );
//		cout<<typeid(pChromCache).name()<<endl;

		CHROMCACHEEX::iterator pos;
		pos = pChromCache->find( bits );
		ASSERT( pos!=pChromCache->end() );
		if( pos!=pChromCache->end() ){
			ASSERT( pos->second.size()==nFits );
			copy( prFits, prFits+nFits, pos->second.begin() );
		}
	}

	void _stdcall RELEASECHROMCACHE( CHROMCACHE*& pChromCache )
	{
		delete pChromCache;
	}
	void _stdcall RELEASECHROMCACHEEX( CHROMCACHEEX*& pChromCache )
	{
		delete pChromCache;
	}
}

