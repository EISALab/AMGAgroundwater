module statcomp
use std_helper
USE NUMERICAL_LIBRARIES

double precision, parameter :: g_rTestAlpha = dble(0.9)
contains

integer function CompareDouble( x1, x2 )
	implicit none
	double precision :: x1, x2
	if( (x1 - x2) == 0 )then
		CompareDouble = 0
	else if( x1>x2 )then
		CompareDouble = 1
	else
		CompareDouble = -1
	endif
end function

!alpha is the confidence level. eg. 0.90, 0.95
integer function CompareByOneSampleT( rMean1, rStd1, n, rMean2, alpha )
implicit none
	double precision, intent(in) :: rMean1, rMean2, rStd1, alpha
	integer :: n

	double precision :: R, b

	CompareByOneSampleT = ProbCompareByOneSampleT( rMean1, rStd1, n, rMean2 )
	return

	R = rStd1  / sqrt(dble(n))
	b = DTIN( alpha, dble(n-1) )
	if( b*R + (rMean1-rMean2) < 0 )then
		CompareByOneSampleT = -1
	else if( -b*R + (rMean1-rMean2) > 0 )then
		CompareByOneSampleT = 1
	else
		!the two sample means has no significant difference, choose the first one, since it is the sampled one.
		CompareByOneSampleT = -1
	endif
end function

integer function ProbCompareByOneSampleT( rMean1, rStd1, n, rMean2 )
implicit none
	double precision, intent(in) :: rMean1, rMean2, rStd1
	integer :: n

	double precision :: R, b, Prob
	R = rStd1  / sqrt(dble(n))
	Prob = DTDF( (rMean1-rMean2) / R, dble(n-1) )	!the probability m1>m2 is Prob
	if( Prob > UniRand() )then
		ProbCompareByOneSampleT = 1
	else
		ProbCompareByOneSampleT = -1
	endif
end function


integer function ProbCompareByTwoSampleT( rMean1, rStd1, n, rMean2, rStd2, m )
implicit none
	double precision, intent(in) :: rMean1, rMean2, rStd1, rStd2
	integer :: n, m

	double precision :: Sp, R, b, Prob
	Sp = sqrt( ((n-1)*rStd1*rStd1 + (m-1)*rStd2*rStd2) / (n+m-2) )
	R = Sp * sqrt( 1.d0/n + 1.d0/m )
	Prob = DTDF( (rMean1-rMean2) / R, dble(n+m-2) )

	!the probability m1>m2 is Prob
	if( Prob > UniRand() )then
		ProbCompareByTwoSampleT = 1
	else
		ProbCompareByTwoSampleT = -1
	endif
end function

!alpha is the confidence level. eg. 0.90, 0.95
integer function CompareByTwoSampleT( rMean1, rStd1, n, rMean2, rStd2, m, alpha )
implicit none
	double precision, intent(in) :: rMean1, rMean2, rStd1, rStd2, alpha
	integer :: n, m

	double precision :: Sp, R, b

	CompareByTwoSampleT = ProbCompareByTwoSampleT( rMean1, rStd1, n, rMean2, rStd2, m )
	return

	Sp = sqrt( ((n-1)*rStd1*rStd1 + (m-1)*rStd2*rStd2) / (n+m-2) )
	R = Sp * sqrt( 1.d0/n + 1.d0/m )
	b = DTIN( alpha, dble(n+m-2) )
	if( b*R + (rMean1-rMean2) < 0 )then
		CompareByTwoSampleT = -1
	else if( -b*R + (rMean1-rMean2) > 0 )then
		CompareByTwoSampleT = 1
	else
		!the two sample means has no significant difference, choose the one with larger sampling number.
		if( n > m )then
			CompareByTwoSampleT = -1
		else if( n<m )then
			CompareByTwoSampleT = 1
		else
			!the two samples has the same sampling #, choose the one with smaller std. more reliable
			if( rStd1 < rStd2 )then
				CompareByTwoSampleT = -1
			else if( rStd1 > rStd2 )then
				CompareByTwoSampleT = 1
			else
				!the two sample has the same standard deviation
				CompareByTwoSampleT = 0
			endif
		endif
	endif
end function

!alpha is the confidence level.
integer function CompareByNormal( rMean1, rStd1, rMean2, rStd2, alpha )
implicit none
	double precision, intent(in) :: rMean1, rMean2, rStd1, rStd2, alpha

	double precision :: std, mu, p
	std = sqrt( rStd1*rStd1 + rStd2*rStd2 )
	mu = rMean1 - rMean2
	
	!p is the probability that mu>0
	p = 1 - DNORDF( -mu/std )

	if( p>alpha )then
		CompareByNormal = 1
	else if( p<(1-alpha) )then
		CompareByNormal = -1
	else
		!the two sample means has no significant difference, choose the one with smaller std. more reliable
		if( rStd1 < rStd2 )then
			CompareByNormal = -1
		else if( rStd1 > rStd2 )then
			CompareByNormal = 1
		else
			!the two sample has the same standard deviation
			CompareByNormal = 0
		endif
	endif
end function


!###################################################################################################


!###################################################################################################
integer function CompareTwoTCostViol( arrCost1, arrViol1, n, arrCost2, arrViol2, m )
implicit none
	double precision, dimension(2), intent(in) :: arrCost1, arrCost2, arrViol1, arrViol2
	integer :: n, m

	if( arrViol1(1)<=0 )then
		if( arrViol2(1)<=0 )then
			!both has no violations, compare cost only
			CompareTwoTCostViol = CompareByTwoSampleT( arrCost1(1), arrCost1(2), n, arrCost2(1), arrCost2(2), m, g_rTestAlpha )
		else
			!the second has violation, return -1 since the first has no violation
			CompareTwoTCostViol = -1
		endif
	else 
		if( arrViol2(1)<=0 )then
			!the first has violation, return 1 since the second has no violation
			CompareTwoTCostViol = 1
		else
			!both has violations
			CompareTwoTCostViol = CompareByTwoSampleT( arrViol1(1), arrViol1(2), n, arrViol2(1), arrViol2(2), m, g_rTestAlpha )
		endif
	endif
end function

integer function CompareOneTCostViol( arrCost1, arrViol1, n, arrCost2, arrViol2 )
implicit none
	double precision, dimension(2), intent(in) :: arrCost1, arrCost2, arrViol1, arrViol2
	integer :: n

	if( arrViol1(1)<=0 )then
		if( arrViol2(1)<=0 )then
			!both has no violations, compare cost only
			CompareOneTCostViol = CompareByOneSampleT( arrCost1(1), arrCost1(2), n, arrCost2(1), g_rTestAlpha )
		else
			!the second has violation, return -1 since the first has no violation
			CompareOneTCostViol = -1
		endif
	else 
		if( arrViol2(1)<=0 )then
			!the first has violation, return 1 since the second has no violation
			CompareOneTCostViol = 1
		else
			!both has violations
			CompareOneTCostViol = CompareByOneSampleT( arrViol1(1), arrViol1(2), n, arrViol2(1), g_rTestAlpha )
		endif
	endif
end function

integer function CompareNormalCostViol( arrCost1, arrViol1, arrCost2, arrViol2 )
implicit none
	double precision, dimension(2), intent(in) :: arrCost1, arrCost2, arrViol1, arrViol2
	if( arrViol1(1)<=0 )then
		if( arrViol2(1)<=0 )then
			!both has no violations, compare cost only
			CompareNormalCostViol = CompareByNormal( arrCost1(1), arrCost1(2), arrCost2(1), arrCost2(2), g_rTestAlpha )
		else
			!the second has violation, return -1 since the first has no violation
			CompareNormalCostViol = -1
		endif
	else 
		if( arrViol2(1)<=0 )then
			!the first has violation, return 1 since the second has no violation
			CompareNormalCostViol = 1
		else
			!both has violations
			CompareNormalCostViol = CompareByNormal( arrViol1(1), arrViol1(2), arrViol2(1), arrViol2(2), g_rTestAlpha )
		endif
	endif
end function

integer function CompareCostViol( rCost1, rViol1, rCost2, rViol2 )
implicit none
	double precision, intent(in) :: rCost1, rViol1, rCost2, rViol2
	if( rViol1<=0 )then
		if( rViol2<=0 )then
			!both has no violations, compare cost only
			CompareCostViol = CompareDouble( rCost1, rCost2 )
		else
			!the second has violation, return -1 since the first has no violation
			CompareCostViol = -1
		endif
	else
		if( rViol1<=0 )then
			!the first has violation, return 1 since the second has no violation
			CompareCostViol = 1
		else
			!both has violations
			CompareCostViol = CompareDouble( rViol1, rViol2 )
		endif
	endif
end function
			

end module
