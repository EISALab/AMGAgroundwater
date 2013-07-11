#include <stdlib.h>
#include <memory.h>
#include <stdio.h>
#include <direct.h>
#include <string.h>
#include <ctype.h>
#include <tchar.h>
#include <treetmpl.h>
#include <mathbase.h>
#include <typeinfo.h>

class CChromItem;
class CChromItemTraits;
typedef CAvlTree<CChromItem*, CChromItem*, int*, CChromItemTraits> CAvlChromTree;

extern "C" int  _stdcall COMPARECHROMS( int* pChrom1, int* pChrom2 );

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

class CCustomCompareTraits : public CElementTraits<CChromItem*, int*>
{
public:
	static int CompareElements( const CChromItem*& pItem1, const CChromItem*& pItem2 )
	{
		return pItem1->Compare( pItem2 );
	}
	static int CompareToKey( const CChromItem*& pItem, int* pChrom )
	{
		if( COMPARECHROMS( pItem->m_pChrom, pChrom )==0 ){
			return pItem->Compare( pChrom );
		}else{
			return 0;
		}
	}
};

typedef CAvlTree<CChromItem*, CChromItem*, int*, CCustomCompareTraits> CCustomAvlChromTree;

/*template< class AVL >
class CCustomCompareTraits : public CElementTraits<CChromItem*, int*>
{
public:
	static int CompareElements( const CChromItem*& pItem1, const CChromItem*& pItem2 )
	{
		return pItem1->Compare( pItem2 );
	}
	static int CompareToKey( const CChromItem*& pItem, int* pChrom )
	{
		return AVL::CompareKeys( pItem->m_pChrom, pChrom );
	}
};

template< typename TYPE, typename ARG_TYPE = const TYPE&, typename ARG_KEY = ARG_TYPE, class ETraits=CElementTraits< TYPE, ARG_KEY > >
class CCustomCompareAvlTree : public CAvlTree<TYPE, ARG_TYPE, ARG_KEY, ETraits>
{
public:
	typedef int (*FUNCOMPAREKEYS)( ARG_KEY key1, ARG_KEY key2 );
private:
	FUNCOMPAREKEYS	m_pfunCompareKeys;
public:
	CCustomCompareAvlTree( FUNCOMPAREKEYS pfunCompareKeys ){
		m_pfunCompareKeys = pfunCompareKeys;
	}
	int CompareKeys( ARG_KEY key1, ARG_KEY key2 ){
		return (*m_pfunCompareKeys)(key1, key2);
	}
};

typedef CCustomCompareAvlTree< CChromItem*, CChromItem*, int*, CChromItemTraits > CCustomChromTreeq;
//typedef CCustomCompareAvlTree< CChromItem*, CChromItem*, int*, CCustomCompareTraits< CCustomCompareAvlTree > > CCustomChromTree;
*/

//a helper function to free the AVL tree
BOOL FreeChromItem( CChromItem*& pItem, LPVOID)
{
	delete pItem;
	return TRUE;
}

extern "C"{
	int _stdcall CREATECUSTOMCHROMCACHE( )
	{
		CCustomAvlChromTree* pAvlChroms = new CCustomAvlChromTree();
		return (int)pAvlChroms;
	}
/*	void _stdcall INSERTCUSTOMCHROMCACHE( CCustomAvlChromTree*& pAvlChroms, int* pChrom, int* pnChromLen, double* prFits, int* pnFits )
	{
		CChromItem* pItem = new CChromItem( pChrom, *pnChromLen, prFits, *pnFits );
		pAvlChroms->Insert( pItem );
	}
	int _stdcall SEARCHCUSTOMCHROMCACHE( CCustomAvlChromTree*& pAvlChroms, int* pChrom, int* pnChromLen, double* prFits, int* pnFits )
	{
		POSITION pos = pAvlChroms->SearchByKey( pChrom );
		if( pos!=0 ){
			memcpy( prFits, pAvlChroms->GetAt(pos)->m_prVars, (*pnFits)*sizeof(double) );
			return TRUE;
		}
		return FALSE;
	}
	void _stdcall REPLACECUSTOMCHROMCACHE( CCustomAvlChromTree*& pAvlChroms, int* pChrom, int* pnChromLen, double* prFits, int* pnFits )
	{
		POSITION pos = pAvlChroms->SearchByKey( pChrom );
		ASSERT( pos!=0 );
		if( pos!=0 ){
			memcpy( pAvlChroms->GetAt(pos)->m_prVars, prFits, (*pnFits)*sizeof(double) );
		}
	}*/

	int _stdcall CREATECHROMCACHE()
	{
		CAvlChromTree* pAvlChroms = new CAvlChromTree();
		return (int)pAvlChroms;
	}
	void _stdcall INSERTCHROMCACHE( CAvlChromTree*& pAvlChroms, int* pChrom, int* pnChromLen, double* prFits, int* pnFits )
	{
		if( typeid(pAvlChroms)==typeid(CAvlChromTree*) ){
			CChromItem* pItem = new CChromItem( pChrom, *pnChromLen, prFits, *pnFits );
			pAvlChroms->Insert( pItem );
		}else{
			CCustomAvlChromTree* pAvl = dynamic_cast<CCustomAvlChromTree*>(pAvlChroms);
			CChromItem* pItem = new CChromItem( pChrom, *pnChromLen, prFits, *pnFits );
			pAvl->Insert( pItem );
		}
	}
	int _stdcall SEARCHCHROMCACHE( CAvlChromTree*& pAvlChroms, int* pChrom, int* pnChromLen, double* prFits, int* pnFits )
	{
		if( typeid(pAvlChroms)==typeid(CAvlChromTree*) ){
			POSITION pos = pAvlChroms->SearchByKey( pChrom );
			if( pos!=0 ){
				memcpy( prFits, pAvlChroms->GetAt(pos)->m_prVars, (*pnFits)*sizeof(double) );
				return TRUE;
			}
			return FALSE;
		}else{
			CCustomAvlChromTree* pAvl = dynamic_cast<CCustomAvlChromTree*>(pAvlChroms);
			POSITION pos = pAvl->SearchByKey( pChrom );
			if( pos!=0 ){
				memcpy( prFits, pAvl->GetAt(pos)->m_prVars, (*pnFits)*sizeof(double) );
				return TRUE;
			}
			return FALSE;
		}
	}
	void _stdcall REPLACECHROMCACHE( CAvlChromTree*& pAvlChroms, int* pChrom, int* pnChromLen, double* prFits, int* pnFits )
	{
		if( typeid(pAvlChroms)==typeid(CAvlChromTree*) ){
			POSITION pos = pAvlChroms->SearchByKey( pChrom );
			ASSERT( pos!=0 );
			if( pos!=0 ){
				memcpy( pAvlChroms->GetAt(pos)->m_prVars, prFits, (*pnFits)*sizeof(double) );
			}
		}else{
			CCustomAvlChromTree* pAvl = dynamic_cast<CCustomAvlChromTree*>(pAvlChroms);
			POSITION pos = pAvl->SearchByKey( pChrom );
			ASSERT( pos!=0 );
			if( pos!=0 ){
				memcpy( pAvl->GetAt(pos)->m_prVars, prFits, (*pnFits)*sizeof(double) );
			}
		}
	}
	void _stdcall RELEASECHROMCACHE( CAvlChromTree*& pAvlChroms )
	{
		if( typeid(pAvlChroms)==typeid(CAvlChromTree*) ){
			pAvlChroms->VisitTree( FreeChromItem );
			pAvlChroms->RemoveAll();
			delete pAvlChroms;
		}else{
			CCustomAvlChromTree* pAvl = dynamic_cast<CCustomAvlChromTree*>(pAvlChroms);
			pAvl->VisitTree( FreeChromItem );
			pAvl->RemoveAll();
			delete pAvl;
		}
	}
}
