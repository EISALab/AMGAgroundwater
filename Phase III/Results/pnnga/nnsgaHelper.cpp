#include <stdlib.h>
#include <memory.h>
#include <stdio.h>
#include <direct.h>
#include <string.h>
#include <ctype.h>
#include <tchar.h>
#include <treetmpl.h>

int g_nChromLen=0;

class CChromItem
{
public:
	int* m_pChrom;
	int m_nChromLen;
	double m_rFitness;
	CChromItem( int* pChrom, int nChromLen, double rFitness ){
		m_pChrom = new int[nChromLen];
		m_nChromLen = nChromLen;
		m_rFitness = rFitness;
		memcpy( m_pChrom, pChrom, nChromLen*sizeof(int) );
	}
	~CChromItem(){
		delete[] m_pChrom;
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

extern "C"{
	void _stdcall INITCHROMCACHE( int* pnChromLen )
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
	}
}