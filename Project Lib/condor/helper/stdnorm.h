#ifndef _STDNORM_H_
#define _STDNORM_H_
#define M_E			2.71828182845904523536
#define M_LOG2E		1.44269504088896340736
#define M_LOG10E	0.434294481903251827651
#define M_LN2		0.693147180559945309417
#define M_LN10		2.30258509299404568402
#define M_PI		3.14159265358979323846
#define M_PI_2		1.57079632679489661923
#define M_PI_4		0.785398163397448309116
#define M_1_PI		0.318309886183790671538
#define M_2_PI		0.636619772367581343076
#define M_1_SQRTPI	0.564189583547756286948
#define M_2_SQRTPI	1.12837916709551257390
#define M_SQRT2		1.41421356237309504880
#define M_SQRT_2	0.707106781186547524401
#define M_SQRT2PI	2.50662827463100050238

double random_normal();
double stdnormal_cdf(double u);
double stdnormal_inv(double p);

/*
* The standard normal PDF, for one random variable.
*/
inline double stdnormal_pdf(double u)
{
	return exp(-u*u/2)/M_SQRT2PI;
};

#endif