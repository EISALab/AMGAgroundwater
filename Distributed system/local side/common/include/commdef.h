#ifndef _COMMDEF_H_
#define _COMMDEF_H_
#include <math.h>
#include <float.h>
#include <ctype.h>
#include <string.h>
#include <functional>

typedef double REAL;

//define data type
#ifndef BOOL
	#define BOOL int
#endif

#ifndef TRUE
	#define TRUE 1
#endif

#ifndef FALSE
	#define FALSE 0
#endif

#ifndef NULL
	#define NULL 0
#endif

#ifndef ELEMENTS
	#define ELEMENTS(array)		(sizeof(array)/sizeof((array)[0]))	//elements number of an arrray
#endif

#ifndef min
	#define min(x, y) ( (x)<=(y) ? (x) : (y) )
#endif
#ifndef max
	#define max(x, y) ( (x)>=(y) ? (x) : (y) )
#endif

#ifndef sqr
	#define sqr(x) (x)*(x)
#endif
#ifndef SQR
	#define SQR	sqr
#endif

#define setbit( a, i ) ( (a) |= (i) )
#define clrbit( a, i ) ( (a) &= ~(i) )
#define isset( a, i ) ( ((a) & (i)) != 0 )
#define isclr( a, i ) ( !isset( (a), (i) ) )

//swapping function
template<class T>
void swap( T& x, T& y )
{ T t = x; x = y; y = t; }

//double compare function
inline bool isflt_zero(float r)
{ return ((r+1.0)==1.0); }

inline bool isflt_equal(float r1, float r2, float eps=0)
{ return fabs(r1-r2)<=eps+FLT_EPSILON; }

inline bool isdbl_zero(double r)
{ return ((r+1.0)==1.0); }

inline bool isdbl_equal(double r1, double r2, double eps=0)
{ return fabs(r1-r2)<=eps+DBL_EPSILON; }

inline bool isrl_zero(REAL r)
{ return isdbl_zero(r); }

inline bool isrl_equal(REAL r1, REAL r2, REAL eps=0)
{ return isdbl_equal(r1,r2,eps); }

inline char* _tcsdec( char* start, char* current )
{
        return current>start ? current-1 : NULL;
}

inline char* _tcsinc( char* string )
{
        return string+1;
}

inline char* trimright( char* string )
{
        char *lpsz = string+strlen(string);

        lpsz = _tcsdec(string, lpsz);

        while( lpsz && isspace(*lpsz) )
                lpsz = _tcsdec(string, lpsz);

        if( lpsz==NULL )lpsz=string;
        else lpsz++;

        *lpsz = '\0';

        return string;
}

inline char* trimleft( char* string )
{
        char* lpsz = string;

        while (isspace(*lpsz))
                lpsz = _tcsinc(lpsz);

        if (lpsz != string)
        {
                // fix up data and length
                int nDataLength = (int)(strlen(string) - (lpsz - string));
                memmove(string, lpsz, (nDataLength+1)*sizeof(char));
        }
        return string;
}

inline char* trim( char* string )
{
	trimleft( string );
	return trimright( string );
}

/*
		Very useful compose function adaptors from <<The C++ Standard Library>>

? f (g( elem))
	This is the general form of a unary compose function. It allows nested calls of unary
	predicates such that the result of calling predicate g() for elem is used as input for
	predicate f(). The whole expression operates as a unary predicate.
? f (g( elem1,elem2))
	This is a form in which two elements, elem1 and elem2, are passed as arguments to a
	binary predicate g(). Again the result is used as input for the unary predicate f(). The
	whole expression operates as a binary predicate.
? f (g( elem),h(elem))
	This is a form in which elem is passed as an argument to two different unary predicates
	g() and h(), and the result of both is processed by the binary predicate f(). In a way,
	this form "injects" a single argument into a composed function. The whole expression
	operates as a unary predicate.
? f (g(elem1) ,h(elem2))
	This is a form in which two elements, elem1 and elem2, are passed as an argument to
	two different unary predicates g() and h(), and the result of both is processed by the
	binary predicate f(). In a way, this form "distributes" a composed function over two
	arguments. The whole expression operates as a binary predicate.

	Table 8.5. Possible Names of Compose Function Object Adapters
	The C++ Standard Library

	Functionality			This Book			SGI STL
	f (g(elem))				compose_f_gx		compose1
	f (g(elem1,elem2))		compose_f_gxy
	f (g(elem),h(elem))		compose_f_gx_hx		compose2
	f (g(elem1),h(elem2))	compose_f_gx_hy

	Look at the Boost repository for C++ libraries at http://www.boost.org/ for the names that

*/

/* class for the compose_f_gx adapter */
template <class OP1, class OP2>
class compose_f_gx_t
	: public std::unary_function<typename OP2::argument_type,
	typename OP1::result_type>
{
private:
	OP1 op1; //process: op1(op2(x))
	OP2 op2;
public:
	//constructor
	compose_f_gx_t(const OP1& o1, const OP2& o2)
		: op1(o1), op2(o2) {
	}
	//function call
	typename OP1::result_type
	operator() (const typename OP2::argument_type& x) const {
		return op1 (op2(x));
	}
};

/*	convenience functions for the compose _f_gx adapter   */
template <class OP1, class OP2>
inline compose_f_gx_t<OP1,OP2> compose_f_gx(const OP1& o1, const OP2& o2) {
	return compose_f_gx_t<OP1,OP2>(o1,o2);
}


/* class for the compose_f_gx adapter */
template <class OP1, class OP2>
class compose_f_gxy_t
	: public std::binary_function<typename OP2::first_argument_type, typename OP2::second_argument_type, 
	typename OP1::result_type>
{
private:
	OP1 op1; //process: op1(op2(x))
	OP2 op2;
public:
	//constructor
	compose_f_gxy_t(const OP1& o1, const OP2& o2)
		: op1(o1), op2(o2) {
	}
	//function call
	typename OP1::result_type
	operator() (const typename OP2::first_argument_type& x, const typename OP2::second_argument_type& y ) const {
		return op1 (op2(x, y));
	}
};

/*	convenience functions for the compose_f_gxy adapter   */
template <class OP1, class OP2>
inline compose_f_gxy_t<OP1,OP2> compose_f_gxy(const OP1& o1, const OP2& o2) {
	return compose_f_gxy_t<OP1,OP2>(o1,o2);
}

/*	class for the compose_f_gx_hx adapter	*/
template <class OP1, class OP2, class OP3>
class compose_f_gx_hx_t
	: public std::unary_function<typename OP2::argument_type,
	typename OP1::result_type>
{
private:
	OP1 op1; //process: op1 (op2(x), op3(x))
	OP2 op2;
	OP3 op3;
public:
	//constructor
	compose_f_gx_hx_t (const OP1& o1, const OP2& o2, const OP3& o3)
		: op1(o1), op2(o2), op3(o3) {
	}
	//function call
	typename OP1::result_type
	operator()(const typename OP2::argument_type& x) const {
		return op1(op2(x),op3(x));
	}
};

/*	convenience functions for the compose f_gx_hx adapter	*/
template <class OP1, class OP2, class OP3>
inline compose_f_gx_hx_t<OP1,OP2,OP3>
compose_f_gx_hx (const OP1& o1, const OP2& o2, const OP3& o3) {
	return compose_f_gx_hx_t<OPl,OP2,OP3>(ol,o2,o3);
}

/*	class for the compose_ f_gx_hy adapter	*/
template <class OP1, class OP2, class OP3>
class compose_f_gx_hy_t
	: public std::binary_function<typename OP2::argument_type,
	typename OP3::argument_type,
	typename OP1::result_type>
{
private:
	OP1 op1; //process: op1 (op2(x) ,op3(y))
	OP2 op2;
	OP3 op3;
public:
	//constructor
	compose_f_gx_hy_t (const OP1& o1, const OP2& o2, const OP3& o3)
		: op1(o1), op2(o2), op3(o3) {
	}
	//function call
	typename OP1::result_type
	operator()(const typename OP2::argument_type& x, const typename OP3::argument_type& y) const {
		return op1(op2(x),op3(y));
	}
};
/*	convenience function for the compose _f_gx_hy adapter	*/
template< typename OP1, typename OP2, class OP3 >
inline compose_f_gx_hy_t<OP1, OP2, OP3>
compose_f_gx_hy( const OP1& o1, const OP2& o2, const OP3& o3 ){
	return compose_f_gx_hy_t<OP1, OP2, OP3>(o1, o2, o3);
}

#endif //_COMMDEF_H_


