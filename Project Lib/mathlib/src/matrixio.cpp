#include "matrixio.h"

/*ostream& operator << ( ostream& os, const ZRealMatrix& mx )
{
	int nRowBase, nColBase;
	mx.GetBaseIndex( nRowBase, nColBase );

	for( int i=nRowBase; i<nRowBase+mx.GetRow(); i++ ){
		int j;
		for( j=nColBase; j<nColBase+mx.GetCol()-1; j++ ){
			os<<mx(i,j)<<" ";
		}
		os<<mx(i,j)<<endl;
	}
	return os;
}

istream& operator >> ( istream& is, ZRealMatrix& mx )
{
	int nRowBase, nColBase;
	mx.GetBaseIndex( nRowBase, nColBase );

	for( int i=nRowBase; i<nRowBase+mx.GetRow(); i++ ){
		for( int j=nColBase; j<nColBase+mx.GetCol(); j++ ){
			is>>mx(i,j);
		}
	}
	return is;
}*/

std::ostream& operator << ( std::ostream& os, const ZRealMatrix& mx )
{
	int nRowBase, nColBase;
	mx.GetBaseIndex( nRowBase, nColBase );

	for( int i=nRowBase; i<nRowBase+mx.GetRow(); i++ ){
		int j;
		for( j=nColBase; j<nColBase+mx.GetCol()-1; j++ ){
			os<<mx(i,j)<<" ";
		}
		os<<mx(i,j)<<endl;
	}
	return os;
}

std::istream& operator >> ( std::istream& is, ZRealMatrix& mx )
{
	int nRowBase, nColBase;
	mx.GetBaseIndex( nRowBase, nColBase );

	for( int i=nRowBase; i<nRowBase+mx.GetRow(); i++ ){
		for( int j=nColBase; j<nColBase+mx.GetCol(); j++ ){
			is>>mx(i,j);
		}
	}
	return is;
}

