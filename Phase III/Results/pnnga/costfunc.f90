module costfunc
use casewell
implicit none

integer, parameter :: hPenRisk	= 844
integer, parameter :: hPenHead	= 843

character(*), parameter :: strPenRisk	= 'penriskinp.dat'                                                                                                                                                
character(*), parameter :: strPenHead	= 'penheadinp.dat'                                                                                                                                                

contains

! This marks the end of the penalty head subroutine                                                                                                                                                       
                                                                                                                                                                                                          
! #########################################################################################################################################                                                               
                                                                                                                                                                                                          
! This function is used to determine the penalty weights for violation of the risk constraint
! Written  by Gayathri Gopalakrishnan
! Will be called later by the function fitness or by cost function to create the correct objective function

! The function is modified by Shengquan Yan to return the normalized risk violations
! The variables declared here are
! rCurRisk		- the acutal risk value.
! rRiskLimit	- the standard risk limit

double precision function CalcViolateRisk( rCurRisk, rRiskLimit )
implicit none
	!arguments
	double precision, intent(in) :: rCurRisk, rRiskLimit

	!variables
	
	if( rCurRisk > rRiskLimit )then
		! divid by rStdRisk, this is to normalize the risk value.
		CalcViolateRisk = (rCurRisk - rRiskLimit) / rRiskLimit
	else
		CalcViolateRisk = 0
	endif
end function CalcViolateRisk                                                                                                                                                                              

!rRiskMean	- the mean of the risk value
!rRiskStd	- the standard deviation of the rRiskMean
!rRiskLimit	- the standard risk limit
double precision function CalcViolateRiskStd( rRiskMean, rRiskStd, rRiskLimit )
implicit none
	!arguments
	double precision, intent(in) :: rRiskMean, rRiskStd, rRiskLimit

	!variables
	if( rRiskMean > rRiskLimit )then
		CalcViolateRiskStd = (rRiskStd) / rRiskLimit
	else if( rRiskMean + rRiskStd > rRiskLimit ) then
		CalcViolateRiskStd = (rRiskMean+rRiskStd-rRiskLimit) / rRiskLimit
	else
		CalcViolateRiskStd = 0.0
	endif
	
end function

! This function is used to determine the penalty weights for violation of the risk constraint
! Written  by Gayathri Gopalakrishnan
! Will be called later by the function fitness or by cost function to create the correct objective function
! The function is modified by Shengquan Yan to return the normalized risk violations
                                                                                                                                                                                                          
! The variables declared here are
! rCurRisk	- the acutal risk value.
! rStdRisk	- the standard risk limit
! rCoeff	- the weight coefficient to compute the risk penalty
! strPenRisk - the file contains the predefined weight coefficients

double precision function CalcPenaltyRisk( rCurRisk, rStdRisk )                                                                                                                                           
implicit none                                                                                                                                                                                             
	!arguments                                                                                                                                                                                        
	double precision, intent(in) :: rCurRisk, rStdRisk                                                                                                                                                
	                                                                                                                                                                                                  
	!variables                                                                                                                                                                                        
	logical, save :: bDataLoaded = .false.                                                                                                                                                            
	double precision, save :: rCoeff1, rCoeff2, rStdRisk1                                                                                                                                             
	double precision :: rCoeff                                                                                                                                                                        
                                                                                                                                                                                                          
	if( .not.bDataLoaded )then                                                                                                                                                                        
		!load parameters only one time                                                                                                                                                            
		call OpenInputFile(hPenRisk, strPenRisk)                                                                                                                                                  
		rewind hPenRisk                                                                                                                                                                           
		read(hPenRisk, *)rStdRisk1, rCoeff1, rCoeff2                                                                                                                                              
		close(hPenRisk)                                                                                                                                                                           
		bDataLoaded = .true.                                                                                                                                                                      
	end if                                                                                                                                                                                            
                                                                                                                                                                                                          
	if(rStdRisk.eq.rStdRisk1) then                                                                                                                                                                    
		rCoeff = rCoeff1                                                                                                                                                                          
	else                                                                                                                                                                                              
		rCoeff = rCoeff2                                                                                                                                                                          
	endif                                                                                                                                                                                             
                                                                                                                                                                                                          
!	CalcViolateRisk = rCurRisk - rStdRisk                                                                                                                                                             
	if( rCurRisk > rStdRisk ) then                                                                                                                                                                    
		CalcPenaltyRisk = (rCurRisk - rStdRisk)*rCoeff / rStdRisk
	else                                                                                                                                                                                              
		CalcPenaltyRisk = 0                                                                                                                                                                       
	end if                                                                                                                                                                                            
end function CalcPenaltyRisk                                                                                                                                                                              
                                                                                                                                                                                                          
                                                                                                                                                                                                          
! #########################################################################################################################################                                                               
                                                                                                                                                                                                          
! This function is to determine the penalty for the head constraint violations                                                                                                                            
! This is to be used in the determination of the fitness function used later in the program                                                                                                               
! Written  by Gayathri Gopalakrishnan                                                                                                                                                                     

! This function is modified by Shengquan Yan to return normalized hydraulic head violations
! rCurH		- the current hydraulic head
! rOldH		- the original hydraulic head
double precision function CalcViolateH( rCurH, rOldH )                                                                                                                                                    
implicit none                                                                                                                                                                                             
	!arguments                                                                                                                                                                                        
	double precision, intent(in) :: rCurH, rOldH

	!variables
	logical, save :: bDataLoaded = .false.
	double precision, save :: rCoeff, rMargErr
	double precision :: rMargH

	if( .not.bDataLoaded )then
		!load parameters only one time
		call OpenInputFile(hPenHead, strPenHead)
		rewind hPenHead
		read(hPenHead, *)rCoeff, rMargErr
		close(hPenHead)
		bDataLoaded = .true.
	end if

	rMargH = rMargErr * rOldH

	if( abs(rOldH-rCurH) > rMargH )then
		CalcViolateH = (abs(rOldH-rCurH)-rMargH) / rMargH
	else
		CalcViolateH = 0
	endif
end function
                                                                                                                                                                                                          
! This function is modified by Shengquan Yan to return normalized hydraulic head violations
! rCurH		- the current hydraulic head
! rStdH		- the standard deviation of H
! rOldH		- the old hydraulic head.
double precision function CalcViolateStdH( rMeanH, rStdH, rOldH )
implicit none                                                                                                                                                                                             
	!arguments                                                                                                                                                                                        
	double precision, intent(in) :: rMeanH, rStdH, rOldH

	!variables
	logical, save :: bDataLoaded = .false.
	double precision, save :: rCoeff, rMargErr
	double precision :: rMargH

	if( .not.bDataLoaded )then
		!load parameters only one time
		call OpenInputFile(hPenHead, strPenHead)
		rewind hPenHead
		read(hPenHead, *)rCoeff, rMargErr
		close(hPenHead)
		bDataLoaded = .true.
	end if

	rMargH = rMargErr * rOldH

	if( abs(rOldH-rMeanH) > rMargH )then
		CalcViolateStdH = rStdH / rMargH
	else if( abs(rOldH-rMeanH)+rStdH > rMargH )then
		CalcViolateStdH = ( abs(rOldH-rMeanH)+rStdH - rMargH ) / rMargH
	else
		CalcViolateStdH = 0
	endif

end function
                                                                                                                                                                                                          
double precision function CalcPenaltyH( rCurH, rStdH )                                                                                                                                                    
implicit none                                                                                                                                                                                             
	!arguments                                                                                                                                                                                        
	double precision, intent(in) :: rCurH, rStdH                                                                                                                                                      
                                                                                                                                                                                                          
	!variables                                                                                                                                                                                        
	logical, save :: bDataLoaded = .false.                                                                                                                                                            
	double precision, save :: rCoeff, rMargErr                                                                                                                                                        
                                                                                                                                                                                                          
	if( .not.bDataLoaded )then                                                                                                                                                                        
		!load parameters only one time                                                                                                                                                            
		call OpenInputFile(hPenHead, strPenHead)                                                                                                                                                  
		rewind hPenHead                                                                                                                                                                           
		read(hPenHead, *)rCoeff, rMargErr                                                                                                                                                         
		close(hPenHead)                                                                                                                                                                           
		bDataLoaded = .true.                                                                                                                                                                      
	end if                                                                                                                                                                                            
                                                                                                                                                                                                          
	if( abs(rStdH-rCurH) > rMargErr*rStdH )then                                                                                                                                                       
		CalcPenaltyH = (abs(rStdH-rCurH)-rMargErr*rStdH)*rCoeff / rMargErr / rStdH                                                                                                                
	else                                                                                                                                                                                              
		CalcPenaltyH = 0                                                                                                                                                                          
	endif                                                                                                                                                                                             
end function CalcPenaltyH                                                                                                                                                                                 
                                                                                                                                                                                                          
!#########################################################################################################################################                                                                
!This function is to determine the penalty for repeated wells when pumping rate is excedeed                                                                                                               
!Felipe Espinoza 12/12/02                                                                                                                                                                                 
double precision function CalcPenaltyWell( Qw, arrWellLocs, nRemWells, nMaxId, Qmax )                                                                                                                     
implicit none                                                                                                                                                                                             
	!argument                                                                                                                                                                                         
	integer, intent(in) :: nRemWells, nMaxId	!nMaxId is the maximum id number of all possible location ids of a well                                                                           
	double precision, intent(in) :: Qw(nRemWells)                                                                                                                                                     
	integer, intent(in) :: arrWellLocs(nRemWells)                                                                                                                                                     
	double precision, intent(in) :: Qmax                                                                                                                                                              
                                                                                                                                                                                                          
	!variables                                                                                                                                                                                        
	integer :: i                                                                                                                                                                                      
	double precision  :: Qwt(nMaxId), dif                                                                                                                                                             
                                                                                                                                                                                                          
	!initialzie data                                                                                                                                                                                  
	CalcPenaltyWell = 0                                                                                                                                                                               
	Qwt = 0                                                                                                                                                                                           
                                                                                                                                                                                                          
	do i = 1, nRemWells                                                                                                                                                                               
		Qwt( arrWellLocs(i) ) = Qwt( arrWellLocs(i) ) + Qw(i)                                                                                                                                     
	enddo                                                                                                                                                                                             
                                                                                                                                                                                                          
	do i = 1, nMaxId                                                                                                                                                                                  
		dif = dabs(Qwt(i)) - Qmax                                                                                                                                                                 
		if( dif > 0 )then                                                                                                                                                                         
			CalcPenaltyWell = CalcPenaltyWell + dif                                                                                                                                           
		endif                                                                                                                                                                                     
	enddo                                                                                                                                                                                             
end function CalcPenaltyWell

!this function compute normalized pumping rate violations.
!Qw				- the pumping rate array of the remediation wells
!arrWellLocs	- the location id of each remediation well
!nRemWells		- the number of remediation wells
!nMaxIds		- the maximum number of possible well locations
!Qmax			- the limit of total pumping rate
double precision function CalcViolateWell( Qw, arrWellLocs, nRemWells, nMaxId, Qmax )                                                                                                                     
implicit none                                                                                                                                                                                             
	!argument                                                                                                                                                                                         
	integer, intent(in) :: nRemWells, nMaxId	!nMaxId is the maximum id number of all possible location ids of a well                                                                           
	double precision, intent(in) :: Qw(nRemWells)
	integer, intent(in) :: arrWellLocs(nRemWells)
	double precision, intent(in) :: Qmax

	!variables                                                                                                                                                                                        
	integer :: i
	double precision  :: Qwt(nMaxId), dif

	!initialzie data
	CalcViolateWell = 0
	Qwt = 0

	!compute the total pumping rates at each pumping locations
	do i = 1, nRemWells
		Qwt( arrWellLocs(i) ) = Qwt( arrWellLocs(i) ) + Qw(i)
	enddo                                                                                                                                                                                             

	!if the true total pmping rate is higher than Qmax, then compute the violation
	do i = 1, nMaxId
		dif = dabs(Qwt(i)) - Qmax
		if( dif > 0 )then
			CalcViolateWell = CalcViolateWell + dif
		endif
	enddo
	!normalize the violate.
	CalcViolateWell = CalcViolateWell / Qmax
end function


!this function compute the pure cost function. No constraints violations and penalties are computed
double precision function ComputeCost( arrHeads, arrOldHeads, arrPumpRates, nRemWells )
implicit none
	!arguments
	integer, intent(in) :: nRemWells
	double precision, dimension(nRemWells), intent(in) :: arrHeads, arrOldHeads, arrPumpRates

	!variables
	integer :: i, t
	double precision, dimension(g_nRemWells+g_nMonWells) :: Qw
	double precision :: rTotCost

	!wrap for compatible
	Qw(1:g_nRemWells) = arrPumpRates
	Qw(g_nRemWells+1:g_nRemWells+g_nMonWells) = 0

	t = g_rRemTime + 1
	call CalcRemCostEx( Qw, g_nRemWells+g_nMonWells, g_nRemWells, g_nMonWells, arrHeads, arrOldHeads, g_nFinGridX, g_nFinGridY, g_nFinGridZ, dble(g_nMonInt), g_rRemTime, t, g_arrFact, rTotCost )

	ComputeCost = rTotCost*g_rCostFactor
end function

!this function compute the pure cost standard deviation function. No constraints violations and penalties are computed
double precision function ComputeCostStd( arrHeadMeans, arrHeadStds, arrOldHeads, arrPumpRates, nRemWells )
implicit none
	!arguments
	integer, intent(in) :: nRemWells
	double precision, dimension(nRemWells), intent(in) :: arrHeadMeans, arrOldHeads, arrHeadStds, arrPumpRates

	!variables
	integer :: i
	double precision, dimension(nRemWells) :: Qw
	double precision :: rwcapstd, rwoptstd
	double precision :: ir, a3,a4,a5,b3,b4,b5,d,sacost,samp,anal,temp
	double precision :: monint, dt, o, fact(nRemWells)
	double precision :: mwcapcost, mwcapcostsite, mwcapcostwell, rwcapcost, rwopcost, potwcapcost                                                                                                     
	double precision :: blcapcost, rwlabcost, blopcost, municost, blmain, rwmain, muniwat, addopcost                                                                                                  
	double precision :: maincost, sumQ, factor2, factor3, pumpcapcost, pumpopcost, factor4, pwopcost                                                                                                  
	double precision :: par1, par2, potwoppar1, potwopcost, potwoppar2, analyopcost, pwcapcost, pumpcost                                                                                              
	!treatflag : the flag for deciding which treatment technology should be used.treatflag=1 implies bioremediation                                                                                   
	!						  while treatflag =0 implies pump and treat.                                                                                                      
	integer :: treatflag                                                                                                                                                                              

	!wrap for compatible
	Qw = arrPumpRates
	monint = dble(g_nMonInt)
	dt = g_rRemTime
	o = g_rRemTime+1
	fact = g_arrFact

!instructions begin here                                                                                                                                                                                  
	include 'costinp.dat'                                                                                                                                                                             

	!Note, only the remediation well capital cost and operating cost is influenced by hydraulic head variation
	rwcapstd = 0.0
	rwoptstd = 0.0

	!estimate the cost standard deviation caused by hydraulic heads variation
	do i=1,nRemWells                                                                                                                                                                                 
		if (Qw(i) < 0.d0) then
			temp = d - arrHeadMeans(i) - arrHeadStds(i)
			if (temp.lt.0) then
				temp = 0.d0
			else
				temp = arrHeadStds(i)
			endif
!			rwcapstd = rwcapstd + fact(i)*a3 *((d)**(b3)) + a4 * abs((Qw(i)))**b4 * temp ** (b5)                                                                                            
			rwcapstd = rwcapstd + a4 * abs((Qw(i)))**b4 * temp ** (b5)                                                                                            
			rwoptstd = rwoptstd + 365.0d0 * a5 * abs(Qw(i))  * &                                                                                                                              
				&((1.0d0+ir)**(monint *dt * o) - 1.0d0)/ (ir *(1.0d0+ir)**(monint *dt * o ))*temp                                                                                         
		else
			if( Qw(i) > 0.d0 )then
				temp = arrHeadMeans(i) - arrOldHeads(i) + arrHeadStds(i)
				if( temp < 0 )then
					temp = 0.d0
				else
					temp = arrHeadStds(i)
				endif
!				rwcapstd = rwcapstd + fact(i)*a3 * ((d)**(b3)) + a4 * Qw(i) ** (b4) * temp ** (b5)                                                                                      
				rwcapstd = rwcapstd + a4 * Qw(i) ** (b4) * temp ** (b5)                                                                                      
				rwoptstd = rwoptstd + 365.0d0 * a5 * Qw(i)*&                                                                                                                                      
					&((1.0d0+ir)**(monint *dt * o) -1.0d0) / (ir * (1.0d0+ir)**(monint *dt * o)) * temp                                                                                       
			endif                                                                                                                                                                             
		endif                                                                                                                                                                                     
	enddo                                                                                                                                                                                             

	ComputeCostStd = (rwcapstd+rwoptstd)*g_rCostFactor
!The cost std is always bigger than the true cost std, because of (b5) is 0.6
!The following function can approximate the true cost std.
	ComputeCostStd = 0.576*ComputeCostStd**1.2143
end function

double precision function ComputeViolation( arrHeads, arrOldHeads, arrPumpRates, nRemWells, rRiskB, rRiskW )
implicit none
	!arguments
	integer, intent(in) :: nRemWells
	double precision, dimension(nRemWells), intent(in) :: arrHeads, arrOldHeads, arrPumpRates
	double precision, intent(in) :: rRiskB, rRiskW

	double precision :: rViolHead, rViolRiskB, rViolRiskW
	integer :: i

	rViolHead = 0
	rViolRiskB = CalcViolateRisk( rRiskB, g_rRiskSt )
	rViolRiskW = CalcViolateRisk( rRiskW, g_rRiskSt )
	do i=1, nRemWells
		rViolHead = rViolHead + CalcViolateH( arrHeads(i), arrOldHeads(i) )
	end do
	rViolHead = rViolHead + CalcViolateWell( arrPumpRates, g_arrWellLocs, nRemWells, 58, g_parrWellInfos(1)%rMaxPumpRate )
	ComputeViolation = rViolHead + rViolRiskB + rViolRiskW
end function

double precision function ComputeViolationStd( arrHeads, arrHeadStds, arrOldHeads, arrPumpRates, nRemWells, rRiskB, rRiskW, rRiskBStd, rRiskWStd )
implicit none
	!arguments
	integer, intent(in) :: nRemWells
	double precision, dimension(nRemWells), intent(in) :: arrHeads, arrHeadStds, arrOldHeads, arrPumpRates
	double precision, intent(in) :: rRiskB, rRiskW, rRiskBStd, rRiskWStd

	double precision :: rViolHeadStd, rViolRiskBStd, rViolRiskWStd
	integer :: i

	rViolHeadStd = 0
	rViolRiskBStd = CalcViolateRiskStd( rRiskB, rRiskBStd, g_rRiskSt )
	rViolRiskWStd = CalcViolateRiskStd( rRiskW, rRiskWStd, g_rRiskSt )
	do i=1, nRemWells
		rViolHeadStd = rViolHeadStd + CalcViolateStdH( arrHeads(i), arrHeadStds(i), arrOldHeads(i) )
	end do
	ComputeViolationStd = rViolHeadStd + rViolRiskBStd + rViolRiskWStd
end function


subroutine CostByNN( arrHeads, arrOldHeads, nRemWells, rRiskB, rRiskW )
implicit none
	!arguments
	integer, intent(in) :: nRemWells
	double precision, dimension(nRemWells), intent(in) :: arrHeads, arrOldHeads
	double precision, intent(in) :: rRiskB, rRiskW

	!variables
	double precision :: rPenHead, rPenRisk, rPenRiskB, rPenRiskW, rTotCost, rFitness
	integer :: i, t, nLoc
	type(WellLoc), dimension(:), pointer :: parrLocs
	double precision, dimension(g_nRemWells+g_nMonWells) :: Qw

	!wrap for compatible
	do i=1,g_nRemWells
		!translate the current coordinate
		nLoc = g_arrWellLocs(i)
		parrLocs => g_parrWellInfos(i)%parrLocs
		Qw(i) = g_arrPumpRates(i)
	end do
	do i=g_nRemWells+1, g_nRemWells+g_nMonWells
		Qw(i) = 0
	end do

	rPenRiskB = CalcPenaltyRisk(rRiskB, g_rRiskSt)
	rPenRiskW = CalcPenaltyRisk(rRiskW, g_rRiskSt)
	rPenRisk = rPenRiskB + rPenRiskW
                                                                                                                                                                                                          
	rPenHead = 0
	do i=1, nRemWells
		rPenHead = rPenHead + CalcPenaltyH( arrHeads(i), arrOldHeads(i) )
	end do
	rPenHead = rPenHead + CalcPenaltyWell( Qw, g_arrWellLocs, nRemWells, 58, g_parrWellInfos(1)%rMaxPumpRate )

	t = g_rRemTime + 1
	call CalcRemCostEx( Qw, g_nRemWells+g_nMonWells, g_nRemWells, g_nMonWells, arrHeads, arrOldHeads, g_nFinGridX, g_nFinGridY, g_nFinGridZ, dble(g_nMonInt), g_rRemTime, t, g_arrFact, rTotCost )

	g_rFitness = rTotCost*g_rCostFactor + rPenRisk + rPenHead
	g_rTotCost = rTotCost
	g_rPenHead = rPenHead
	g_rPenRisk = rPenRisk
	g_rRiskB = rRiskB
	g_rRiskW = rRiskW
end subroutine CostByNN
                                                                                                                                                                                                          


! #########################################################################################################################################                                                               
                                                                                                                                                                                                          
! This function is to calculate the remediation cost for the installation of the wells                                                                                                                    
! Written by Gayathri Gopalakrishnan.                                                                                                                                                                     
! Rewritten by Shengquan Yan on July 2002                                                                                                                                                                 
!                                                                                                                                                                                                         
! The source code for this function was obtained from previous results by Bryan Smalley                                                                                                                   
! The function is used for the calculation of the cost alone and will be combined                                                                                                                         
! with the penalty weights and the risk violation to find the fitness function                                                                                                                            
!                                                                                                                                                                                                         
! Parameters:                                                                                                                                                                                             
! Qw(nowells), (in)		: the pumping rate of all the wells                                                                                                                                       
! nowells, (in)			: the number of all the wells                                                                                                                                             
! noremwells, (in)		: the number of remediation wells                                                                                                                                         
! nomonwells, (in)		: the number of monitoring wells  (nowells = noremwells+nomonwells)                                                                                                       
! ndrw-[x,y,z](noremwells), (in) : the three arrays of the locations of the remediation wells                                                                                                             
! H(z,x,y), (in)		: the array of hydraulic heads of all the wells                                                                                                                           
! Hun(z,x,y), (in)		: the array of hydraulic heads of undisturbed case with no pumping                                                                                                        
! [x,y,z]-grid, (in)	: the dimension of x, y, and z grid                                                                                                                                               
! monInt, (in)			: the monitoring interval time                                                                                                                                            
! dt, (in)				: time interval for remediation                                                                                                                                   
! o, (in)				: a parameter to check that the cost is depreciated enough                                                                                                        
! ir, (in)				: the interest of investment                                                                                                                                      
! cost, (out)			: the total cost of remediation which includes all components                                                                                                             
!                                                                                                                                                                                                         
!                                                                                                                                                                                                         
subroutine CalcRemCostEx( Qw, nowells, noremwells, nomonwells, H, Hun, xgrid, ygrid, zgrid, monInt, dt, o, fact, cost )                                                                                   
implicit none                                                                                                                                                                                             
                                                                                                                                                                                                          
!parameters definition                                                                                                                                                                                    
	integer, intent(in) :: nowells, noremwells, nomonwells                                                                                                                                            
	double precision, intent(in) :: Qw(nowells)                                                                                                                                                       
                                                                                                                                                                                                          
	integer, intent(in) :: xgrid, ygrid, zgrid                                                                                                                                                        
	double precision, dimension(noremwells), intent(in) :: H, Hun                                                                                                                                     
                                                                                                                                                                                                          
	double precision, intent(in) :: monInt, dt                                                                                                                                                        
	integer, intent(in) :: o                                                                                                                                                                          
	double precision, intent(in) :: fact(noremwells)                                                                                                                                                  
	double precision, intent(out) :: cost                                                                                                                                                             
                                                                                                                                                                                                          
!variables definition                                                                                                                                                                                     
	double precision :: ir
	double precision :: mwcapcost, mwcapcostsite, mwcapcostwell, rwcapcost, rwopcost, potwcapcost                                                                                                     
	double precision :: blcapcost, rwlabcost, blopcost, municost, blmain, rwmain, muniwat, addopcost                                                                                                  
	double precision :: maincost, sumQ, factor2, factor3, pumpcapcost, pumpopcost, factor4, pwopcost                                                                                                  
	double precision :: par1, par2, potwoppar1, potwopcost, potwoppar2, analyopcost, pwcapcost, pumpcost                                                                                              
                                                                                                                                                                                                          
	!treatflag : the flag for deciding which treatment technology should be used.treatflag=1 implies bioremediation                                                                                   
	!						  while treatflag =0 implies pump and treat.                                                                                                      
	integer :: treatflag                                                                                                                                                                              
                                                                                                                                                                                                          
	double precision :: a3,a4,a5,b3,b4,b5,d,sacost,samp,anal,temp                                                                                                                                     
	integer :: i,flagblow                                                                                                                                                                             
                                                                                                                                                                                                          
	!data file containing the calculation parameters                                                                                                                                                  
	integer, parameter:: hInput=41, hOutput = 66                                                                                                                                                      
	character(*), parameter::strInput='costinp.dat', strOutput='costs.out'                                                                                                                            
                                                                                                                                                                                                          
	!read the parameters                                                                                                                                                                              
	!call OpenInputFile( hInput, strInput )                                                                                                                                                           
	!rewind hInput                                                                                                                                                                                    
	!read(hInput,*)ir,a3,a4,a5,b3,b4,b5,d,mwcapcost,mwcapcostwell,samp,anal,blcapcost,rwlabcost,blopcost, &                                                                                           
	!		municost,blmain,rwmain,factor2,factor3,pumpcapcost,par1,par2,factor4,treatflag,potwcapcost, &                                                                                     
	!		potwoppar1, potwoppar2, analyopcost                                                                                                                                               
	!close( hInput )                                                                                                                                                                                  
                                                                                                                                                                                                          
!instructions begin here                                                                                                                                                                                  
	include 'costinp.dat'                                                                                                                                                                             
                                                                                                                                                                                                          
	!open the output file                                                                                                                                                                             
	call OpenOutputFile( hOutput, strOutput )                                                                                                                                                         
                                                                                                                                                                                                          
	!initialize the costs of the wells                                                                                                                                                                
	cost = 0.d0                                                                                                                                                                                       
	rwcapcost = 0.d0                                                                                                                                                                                  
	rwopcost = 0.d0                                                                                                                                                                                   
!	irm = monInt*dt/365*ir  ! In order to caculate the discount per monitoring period.                                                                                                                
                                                                                                                                                                                                          
	!Changed by Xiaolin Ren                                                                                                                                                                           
                                                                                                                                                                                                          
	!This is the end of the section where the cost coefficients are inputted                                                                                                                          
                                                                                                                                                                                                          
	!This is the remediation well capital cost                                                                                                                                                        
	do i=1,noremwells                                                                                                                                                                                 
		if (Qw(i).lt. 0.d0) then                                                                                                                                                                  
			temp = d - H(i)                                                                                                                                                                   
			if (temp.lt.0) then                                                                                                                                                               
				temp = 0.d0                                                                                                                                                               
			endif                                                                                                                                                                             
			rwcapcost = rwcapcost + fact(i)*a3 *((d)**(b3)) + a4 * abs((Qw(i)))**b4 * temp ** (b5)                                                                                            
		else                                                                                                                                                                                      
			if (Qw(i).eq. 0.d0) then                                                                                                                                                          
				rwcapcost = rwcapcost                                                                                                                                                     
			else                                                                                                                                                                              
				temp = H(i) - Hun(i)                                                                                                                                                      
				if (temp.lt.0.d0) then                                                                                                                                                    
					temp = 0.d0                                                                                                                                                       
				endif                                                                                                                                                                     
				rwcapcost = rwcapcost + fact(i)*a3 * ((d)**(b3)) + a4 * Qw(i) ** (b4) * temp ** (b5)                                                                                      
			endif                                                                                                                                                                             
		endif                                                                                                                                                                                     
	enddo                                                                                                                                                                                             
                                                                                                                                                                                                          
	!This is the remediation well operating cost                                                                                                                                                      
 	do i=1,noremwells                                                                                                                                                                                 
		if (Qw(i).le. 0.d0) then                                                                                                                                                                  
			temp = d - H(i)                                                                                                                                                                   
			if (temp.lt.0.d0) then                                                                                                                                                            
				temp = 0.d0                                                                                                                                                               
			endif                                                                                                                                                                             
			rwopcost = rwopcost + 365.0d0 * a5 * abs(Qw(i))  * &                                                                                                                              
				&((1.0d0+ir)**(monint *dt * o) - 1.0d0)/ (ir *(1.0d0+ir)**(monint *dt * o ))*temp                                                                                         
		else                                                                                                                                                                                      
			temp = H(i) - Hun(i)                                                                                                                                                              
			if (temp.lt.0.d0) then                                                                                                                                                            
				temp = 0.d0                                                                                                                                                               
			endif                                                                                                                                                                             
			rwopcost = rwopcost + 365.0d0 * a5 * Qw(i)*&                                                                                                                                      
				&((1.0d0+ir)**(monint *dt * o) -1.0d0) / (ir * (1.0d0+ir)**(monint *dt * o)) * temp                                                                                       
		endif                                                                                                                                                                                     
		write(hOutput,*)Qw(i),monint,dt,ir,o,temp                                                                                                                                                 
	enddo                                                                                                                                                                                             
                                                                                                                                                                                                          
                                                                                                                                                                                                          
	!This is the end of the section for determining if bioremediation or pump and treat costs are to be used                                                                                          
	!treatflag =1 implies bioremediation is to be used                                                                                                                                                
                                                                                                                                                                                                          
	!This is the monitoring well cost                                                                                                                                                                 
	mwcapcost = mwcapcost + mwcapcostwell * nomonwells                                                                                                                                                
                                                                                                                                                                                                          
	!This is the sampling/analysis cost                                                                                                                                                               
 	sacost = samp + anal * nomonwells                                                                                                                                                                 
	sacost = sacost / ( (1.0d0+ir) ** (monint * dt))                                                                                                                                                  
                                                                                                                                                                                                          
	!This is only used for this case(o=1 & monint>1yr)                                                                                                                                                
	!flagblow is the flag to tell if the wells are being pumped or not                                                                                                                                
                                                                                                                                                                                                          
	flagblow = 0                                                                                                                                                                                      
	if (flagblow.eq.0) then                                                                                                                                                                           
		do i=1,noremwells                                                                                                                                                                         
			if(Qw(i).gt.0.d0) then                                                                                                                                                            
				flagblow = 1                                                                                                                                                              
			endif                                                                                                                                                                             
		enddo                                                                                                                                                                                     
	endif                                                                                                                                                                                             
	if(flagblow.eq.0) then                                                                                                                                                                            
		do i=noremwells,nowells !This is for the monitoring wells                                                                                                                                 
			if(Qw(i).gt.0.d0) then                                                                                                                                                            
				flagblow = 1                                                                                                                                                              
			endif                                                                                                                                                                             
		enddo                                                                                                                                                                                     
	endif                                                                                                                                                                                             
                                                                                                                                                                                                          
	!This section sets the blower cost to 0 if the wells are not being pumped at all                                                                                                                  
	if (flagblow.eq.0) then                                                                                                                                                                           
		blcapcost = 0.d0                                                                                                                                                                          
	endif                                                                                                                                                                                             
                                                                                                                                                                                                          
	! This section is used to find the additional operating cost if present                                                                                                                           
	addopcost=0.d0                                                                                                                                                                                    
                                                                                                                                                                                                          
	if(treatflag.eq.1) then                                                                                                                                                                           
		!the variable sumQ is used to find the sum of all the pumping rates                                                                                                                       
		!the variable sumQ is used to find the sum of all the pumping rates, including remed wells and monitoring wells                                                                           
		!????????????, why just summation? Aren't some Qw(i)s negative if they are injection well? By Shegnquan Yan                                                                               
		sumQ = 0.d0                                                                                                                                                                               
		do i=1,nowells                                                                                                                                                                            
			sumQ = sumQ + Qw(i)                                                                                                                                                               
		end do                                                                                                                                                                                    
                                                                                                                                                                                                          
		muniwat = sumQ                                                                                                                                                                            
		if(muniwat.lt.0.d0) then                                                                                                                                                                  
			muniwat = 0.d0                                                                                                                                                                    
		endif                                                                                                                                                                                     
                                                                                                                                                                                                          
		if(flagblow.eq.0) then                                                                                                                                                                    
			blopcost = 0.d0                                                                                                                                                                   
		endif                                                                                                                                                                                     
                                                                                                                                                                                                          
		!This next part checks to see if all the pumping rates are zero                                                                                                                           
		if (sumQ.eq.0.d0) then                                                                                                                                                                    
			rwlabcost = 0.d0                                                                                                                                                                  
		endif                                                                                                                                                                                     
                                                                                                                                                                                                          
		addopcost = (rwlabcost + blopcost + municost + muniwat)* monint *dt&                                                                                                                      
             & *((1.0d0+ir)**(o) -1.0d0)/(ir * (1.0d0+ir)**(o))                                                                                                                                           
		addopcost = 0.0d0                                                                                                                                                                         
	endif                                                                                                                                                                                             
                                                                                                                                                                                                          
	!This section deals with the maintenance costs                                                                                                                                                    
	if(flagblow.eq.0) then                                                                                                                                                                            
		blmain = 0.d0                                                                                                                                                                             
	else if(sumQ.eq.0.d0) then                                                                                                                                                                        
		blmain = 0.d0                                                                                                                                                                             
		rwmain = 0.d0                                                                                                                                                                             
	endif                                                                                                                                                                                             
                                                                                                                                                                                                          
                                                                                                                                                                                                          
	!'==============================                                                                                                                                                                  
	!The following cost is for bioremdiation action, changed by Xiaolin Ren                                                                                                                           
	!temp = o*monint*dt/factor2                                                                                                                                                                       
	!if(temp.eq.ceiling(temp)) then                                                                                                                                                                   
	!	maincost =( blmain + rwmain)*((1+ir * factor2/factor3)**(temp-1))/(ir*factor2/factor3*(1+ir*factor2/factor3)**(temp))                                                                     
	!else                                                                                                                                                                                             
		maincost = 0.d0                                                                                                                                                                           
	!endif                                                                                                                                                                                            
	!=============================                                                                                                                                                                    
                                                                                                                                                                                                          
                                                                                                                                                                                                          
	!This is the section that deals with the costs for the pump and treatment of the groundwater.                                                                                                     
	!In this case, we are assuming that the groundwater is treated and then recirculated in the aquifer                                                                                               
	!Thus, only the costs of pumping are considered and not those of disposal of the treated water.                                                                                                   
	!Note that since the maximum pumping rate is only within a given range for this problem, that is                                                                                                  
	!the only range of capital and operating costs being considered here.                                                                                                                             
	!April 12th, 2001 - a change has been made where the cost of disposal is also added to the design                                                                                                 
	!The capital cost of disposing to the POTW is added to the capital cost pwcapcost as potwcapcost                                                                                                  
	!The operating cost is added as potwopcost                                                                                                                                                        
	!The discharge rate is assumed to be equal to the influent rate to the treatment system                                                                                                           
                                                                                                                                                                                                          
	pwcapcost = 0.d0                                                                                                                                                                                  
	pwopcost = 0.d0                                                                                                                                                                                   
	pumpcost = 0.d0                                                                                                                                                                                   
	potwopcost = 0.d0                                                                                                                                                                                 
                                                                                                                                                                                                          
	if (treatflag.eq.0) then                                                                                                                                                                          
                                                                                                                                                                                                          
		pwcapcost = pumpcapcost + potwcapcost                                                                                                                                                     
        do i=1,noremwells                                                                                                                                                                                 
!##########################################################################################                                                                                                               
			if(Qw(i)<0.d0)then                                                                                                                                                                
				pwopcost = par1 * abs(Qw(i)) * factor4 + par2                                                                                                                             
				potwopcost = potwoppar1 * abs( Qw(i) ) * factor4 + potwoppar2                                                                                                             
			end if                                                                                                                                                                            
        enddo                                                                                                                                                                                             
                                                                                                                                                                                                          
! the Injection water		                                                                                                                                                                          
		sumQ = 0.d0                                                                                                                                                                               
        do i=1,noremwells                                                                                                                                                                                 
           sumQ = sumQ + Qw(i)                                                                                                                                                                            
        enddo                                                                                                                                                                                             
                                                                                                                                                                                                          
        do i=noremwells,nowells                                                                                                                                                                           
           sumQ = sumQ + Qw(i)                                                                                                                                                                            
        enddo                                                                                                                                                                                             
                                                                                                                                                                                                          
        muniwat = sumQ                                                                                                                                                                                    
        if(muniwat.lt.0.d0) then                                                                                                                                                                          
           muniwat = 0.d0                                                                                                                                                                                 
        endif                                                                                                                                                                                             
                                                                                                                                                                                                          
        pumpcost = pwcapcost + ( pwopcost + potwopcost + analyopcost * dt ) * &                                                                                                                           
             & ((1.0d0+ir)**(monint *dt * o) -1.0d0) / (ir * (1.0d0+ir)**(monint *dt * o)) + muniwat*municost                                                                                             
!############################################################################################################                                                                                             
                                                                                                                                                                                                          
!           pwopcost = par1 * abs(Qw(i)) * factor4 + par2                                                                                                                                                 
 !          potwopcost = potwoppar1 * abs( Qw(i) ) * factor4 + potwoppar2                                                                                                                                 
  !      enddo                                                                                                                                                                                            
   !     pumpcost = pwcapcost + ( pwopcost + potwopcost + analyopcost * dt ) * &                                                                                                                          
    !         & ((1.0d0+ir)**(monint *dt * o) -1.0d0) / (ir * (1.0d0+ir)**(monint *dt * o))                                                                                                               
                                                                                                                                                                                                          
		! Writing individual costs to file 'cost.out'                                                                                                                                             
		!Writing individual costs to file 'cost.out'                                                                                                                                              
		write(hOutput,*) "pwcapcost", pwcapcost                                                                                                                                                   
		write(hOutput,*) "pwopcost", pwopcost                                                                                                                                                     
		write(hOutput,*) "potwopcost", potwopcost                                                                                                                                                 
     endif                                                                                                                                                                                                
                                                                                                                                                                                                          
     cost = rwcapcost + rwopcost + mwcapcost + sacost + blcapcost + addopcost + maincost + pumpcost                                                                                                       
                                                                                                                                                                                                          
	close( hOutput )                                                                                                                                                                                  
end subroutine CalcRemCostEx                                                                                                                                                                              

end module