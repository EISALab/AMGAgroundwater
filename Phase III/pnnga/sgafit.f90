module sgafit
use STD_HELPER
use costfunc
use casemeta
use sga
use chmcache
use casewell
use sampling
implicit none

integer, parameter :: hSsInp	= 845
integer, parameter :: hRiskInp	= 842
integer, parameter :: hCsgaOut	= 999
                                                                                                                                                                                                          

character(*), parameter :: strSsInp		= 'ssinp.dat'                                                                                                                                             
character(*), parameter :: strRiskInp	= 'riskinp.dat'                                                                                                                                                   
character(*), parameter :: strCsgaOut	= 'newsga.out'                                                                                                                                                    
                                                                                                                                                                                                          

integer, parameter :: g_nPoolSize	= 80
integer :: g_nPooledPdes
                                                                                                                                                                                                          
!DEC$ DEFINE COST_VERSION=1                                                                                                                                                                               
                                                                                                                                                                                                          
!DEC$ IF( COST_VERSION>=2 )                                                                                                                                                                               
	!Retardation factor for velocity.                                                                                                                                                                 
	double precision, parameter :: l_rRfs = 1.00                                                                                                                                                      
                                                                                                                                                                                                          
	!longitudinal dispersivity.                                                                                                                                                                       
	double precision, parameter :: l_rDaL = 3.78                                                                                                                                                      
	!transverse dispersivity.                                                                                                                                                                         
	double precision, parameter :: l_rDaT = 3.74                                                                                                                                                      
                                                                                                                                                                                                          
	!average linear velocity= Darcy's velocity/porosity. m/day                                                                                                                                        
	double precision, parameter :: l_rVelocity = 0.10487                                                                                                                                              
                                                                                                                                                                                                          
	!thickness of aquifer.                                                                                                                                                                            
	double precision, parameter :: l_rB = 9.0                                                                                                                                                         
                                                                                                                                                                                                          
	!time interval for remediation                                                                                                                                                                    
	double precision, parameter :: l_rDetaT = 1.0                                                                                                                                                     
!DEC$ ELSE                                                                                                                                                                                                
	double precision, parameter :: l_rRfs = 1.41                                                                                                                                                      
                                                                                                                                                                                                          
	!longitudinal dispersivity.                                                                                                                                                                       
	double precision, parameter :: l_rDaL = 15.0                                                                                                                                                      
	!transverse dispersivity.                                                                                                                                                                         
	double precision, parameter :: l_rDaT = 3.0                                                                                                                                                       
                                                                                                                                                                                                          
	!average linear velocity= Darcy's velocity/porosity. m/day                                                                                                                                        
	double precision, parameter :: l_rVelocity = 0.03                                                                                                                                                 
                                                                                                                                                                                                          
	!thickness of aquifer.                                                                                                                                                                            
	double precision, parameter :: l_rB = 20.0                                                                                                                                                        
                                                                                                                                                                                                          
	!time interval for remediation                                                                                                                                                                    
	double precision, parameter :: l_rDetaT = 1.0                                                                                                                                                     
!DEC$ ENDIF                                                                                                                                                                                               

type MODELHANDLESTRUCT
	integer :: hHeadMeanModel
	integer :: hRiskMeanModel
	integer :: hHeadMeanNorm
	integer :: hRiskMeanNorm
end type

contains

!this function selects one individual using tournament selection scheme.
!arrFits	- the fitness value array.
!arrIndex	- the id array of the candidate individuals
!nSelSize	- the size of arrIndex. 
!nTournSize	- the tournament size.
!the function returns the index of selected individual.
integer function TournSel( arrFits, arrIndex, nSelSize, nTournSize )
use Std_helper
implicit none
	!argument
	double precision, dimension(*), intent(in) :: arrFits
	integer, dimension(*), intent(in) :: arrIndex
	integer, intent(in) :: nSelSize, nTournSize
	
	!variable
	integer :: iPick, iMate, i

	iPick = 1+dint(dble(nSelSize-1)*UniRand())	!this is not correct, now for debug only
	do i=2, nTournSize
		iMate = 1+dint(dble(nSelSize-1)*UniRand())
		do while ((nSelSize>nTournSize) .and. (iMate==iPick) )
			iMate = 1+dint(dble(nSelSize-1)*UniRand())
		enddo
			
		if( CompareByCostViolTest( arrIndex(iMate), arrIndex(iPick) )<=0 )then
!		if( arrFits(arrIndex(iMate))<arrFits(arrIndex(iPick)) )then
			iPick = iMate
		endif
	enddo
	TournSel = iPick
end function TournSel

!the subroutine find the unique chromosomes and put their ids into arrIds.
!this is to avoid oversampling the pdes. (if there many repetitions, a pde individual maybe sampled too many times)
subroutine UniqueIds( sgaParam, arrIds, count )
implicit none
	type(SGAPARAMSTRUCT), intent(in) :: sgaParam
	integer, dimension(sgaParam%nPopSize), intent(out) :: arrIds
	integer, intent(out) :: count

	integer :: hCacheTemp, i
	double precision :: dums(1)

	hCacheTemp = CreateChromCache()
	count = 0

	do i=1, sgaParam%nPopSize
		if( SearchChromCache(hCacheTemp, sgaParam%arrPop(i,:), sgaParam%nChromBits, 0, dums, 1)==0 )then
			call InsertChromCache(hCacheTemp, sgaParam%arrPop(i,:), sgaParam%nChromBits, 0, dums, 1)
			
			count = count+1
			arrIds(count) = i
		endif
	enddo

	call ReleaseChromCache( hCacheTemp )
end subroutine

!select nExpectedPdes using tournament selection scheme.
!nTournSize		- the tournament size
!nExpectPdes	- the expected number of selected individuals
!arrPdeIds(out)	- the array contains the selected individuals
!nPdeCount(out)	- the number of selected individuals
subroutine TournSelPdes( sgaParam, nTournSize, nExpectPdes, arrPdeIds, nPdeCount )
implicit none
	type(SGAPARAMSTRUCT), intent(in) :: sgaParam
	integer, intent(in) :: nTournSize, nExpectPdes
	integer, intent(out) :: nPdeCount
	integer, dimension(*), intent(out) :: arrPdeIds

	!variables
	integer, dimension(sgaParam%nPopSize) :: arrIndex
	integer :: i, nSelCount, iPick

	!collect the ANN-evaluated ids in the population
	nSelCount = 0
	do i=1, sgaParam%nPopSize
		if( sgaParam%arrFlags(i)==NN_EVAL )then
			arrIndex( nSelCount+1 ) = i
			nSelCount = nSelCount + 1
		endif
	enddo

	!Modified by Shengquan Yan 08/22/05. The PDE evaluated ids is still in the selection set.
!	arrIndex = (/(i, i=1,sgaParam%nPopSize)/)
!	nSelCount = sgaParam%nPopSize
	call UniqueIds( sgaParam, arrIndex, nSelCount )

	nPdeCount = min( nExpectPdes, nSelCount )
	!do tournment selection without replacement.
	do i=1, nPdeCount
		iPick = TournSel( sgaParam%arrFit, arrIndex, nSelCount, nTournSize )
		arrPdeIds(i) = arrIndex(iPick)
		call SwapInt( arrIndex(iPick), arrIndex(nSelCount) )
		nSelCount = nSelCount-1
	enddo
end subroutine

!select nExpectedPdes individuals using best selection scheme.
!arrIndex		- the sorted array index 
!nExpectPdes	- the expected number of selected individuals
!arrPdeIds(out)	- the array contains the selected individuals
!nPdeCount(out)	- the number of selected individuals
subroutine BestSelPdes( sgaParam, arrIndex, nExpectedPdes, arrPdeIds, nPdeCount )
use std_helper
implicit none
	type(SGAPARAMSTRUCT), intent(in) :: sgaParam
	integer, dimension(:) :: arrIndex
	integer, intent(in) :: nExpectedPdes
	integer, intent(out) :: nPdeCount
	integer, dimension(*), intent(out) :: arrPdeIds

	!arguments
	integer :: i, id

	nPdeCount = 0
	do i=1, sgaParam%nPopSize
		id = arrIndex(i)
		if( nPdeCount<nExpectedPdes )then
			if( (sgaParam%arrFlags(id)==NN_EVAL) )then
				arrPdeIds(nPdeCount+1) = id
				nPdeCount = nPdeCount + 1
			endif
		else
			exit
		endif
	enddo
end subroutine

integer(2) function WellCompare(a1, a2)
	integer :: a1, a2
	WellCompare = g_arrWellLocs(a1)-g_arrWellLocs(a2)
end function

subroutine EvalByAnns(sgaParam, modelHandle)
implicit none
	!argument                                                                                                                                                                                         
	type(SGAPARAMSTRUCT), intent(inout) :: sgaParam
	type(MODELHANDLESTRUCT), intent(in) :: modelHandle

	!variables
	integer :: i, k
	integer, dimension( g_nRemWells ) :: arrWellIndex
	double precision, dimension(4*g_nRemWells+2) :: arrVars			!the vars for parsing and normalize the NN parameters.
	double precision, dimension( g_nRemWells ) :: arrHeads
	double precision :: rRiskB, rRiskW

	do i=1, sgaParam%nPopSize
		call DecodeOne( sgaParam, i )
		call wrapPrepFunc( i, sgaParam%arrVars, sgaParam%nPopSize, sgaParam%nVarCount )

		!order the wells by their number first
		arrWellIndex = (/(k, k=1, g_nRemWells)/)
		call qsort( arrWellIndex, g_nRemWells, sizeof(arrWellIndex(1)), WellCompare )

		!predict the heads                                                                                                                                                                
		call ParseVars( arrVars )
		call Normalize( modelHandle%hHeadMeanNorm, arrVars, 1 )
		call OrderVars( arrVars, arrWellIndex )
		call SimNet( modelHandle%hHeadMeanModel, arrVars, arrVars(3*g_nRemWells+1:4*g_nRemWells) )
		call UnOrderVars( arrVars, arrWellIndex )
		!parse the data back to global variables.
		call UnNormalize( modelHandle%hHeadMeanNorm, arrVars, 1 )
		arrHeads = arrVars(3*g_nRemWells+1 : 4*g_nRemWells)

		!predict the risks
		call ParseVars( arrVars, g_arrHeads )
		call Normalize( modelHandle%hRiskMeanNorm, arrVars, 1 )
		call OrderVars( arrVars, arrWellIndex )
		call SimNet( modelHandle%hRiskMeanModel, arrVars, arrVars(4*g_nRemWells+1:4*g_nRemWells+2) )
		call UnOrderVars( arrVars, arrWellIndex )
		!parse the data back to global variables.
		call UnNormalize( modelHandle%hRiskMeanNorm, arrVars, 1 )
		rRiskB = 10**( -arrVars(4*g_nRemWells+1) )                                                                                                                                        
		rRiskW = 10**( -arrVars(4*g_nRemWells+2) )

		!compute the cost and violations and their standard deviation
		sgaParam%arrCosts(i) = ComputeCost( arrHeads, g_arrOldHeads, g_arrPumpRates, g_nRemWells )
		sgaParam%arrCostStds(i) = 0.d0
		sgaParam%arrViols(i) = ComputeViolation( arrHeads, g_arrOldHeads, g_arrPumpRates, g_nRemWells, rRiskB, rRiskW )
		sgaParam%arrViolStds(i) = 0.d0
                                                                                                                                                                                                      
		!calculate the cost by the output of ANN                                                                                                                                           
		call CostByNN( arrHeads, g_arrOldHeads, g_nRemWells, rRiskB, rRiskW )                                                                                                           
                                                                                                                                                                                                      
		sgaParam%arrFit(i) = g_rFitness
		sgaParam%arrFitStds(i) = 0.d0
		sgaParam%arrFlags(i) = NN_EVAL		!0 means a NN results.
	enddo
end subroutine

!EvalByCache evaluates the specified individuals by cache.
!the function first searches the individual in the cache, if it finds the individual, it compute the cost and violations using cached information.
!sgaParam		- the sga parameter structure
!arrIndex		- the array of the individual ids
!nIds			- the size of the ids array
!nCachedFits	- the number of fitness stored in cache
!nFlag			- the flag that will be assigned to the invididual if it can be evaluated by the cache
subroutine EvalByCache( sgaParam, arrIds, nIds, nCachedFits, nFlag )
implicit none
	!argument                                                                                                                                                                                         
	type(SGAPARAMSTRUCT), intent(inout) :: sgaParam
	integer, dimension(:) :: arrIds
	integer, intent(in) :: nIds, nCachedFits, nFlag

	!variables
	double precision, dimension(nCachedFits) :: arrCachedFits, arrCachedFitStds
	integer :: i, id

	do i = 1, nIds
		id = arrIds(i)
		if( SearchChromCache(hChromCache, sgaParam%arrPop(id,:), sgaParam%nChromBits, -1, arrCachedFits, nCachedFits)/=0 )then
			call GetStatChromCache(hChromCache, sgaParam%arrPop(id,:), sgaParam%nChromBits, arrCachedFits, arrCachedFitStds )
			sgaParam%arrCosts(id) = arrCachedFits(1)
			sgaParam%arrViols(id) = arrCachedFits(2)
			sgaParam%arrCostStds(id) = arrCachedFitStds(1)
			sgaParam%arrViolStds(id) = arrCachedFitStds(2)
			sgaParam%arrFit(id) = arrCachedFits(3)
			sgaParam%arrFitStds(id) = arrCachedFitStds(3)
			sgaParam%arrFlags(id) = nFlag
		endif
	enddo
end subroutine

!UpdateInjPool evaluates the specified individuals by cache.
!the function first searches the individual in the cache, if it finds the individual, it compute the cost and violations using cached information.
!sgaParam		- the sga parameter structure
!nCachedFits	- the number of fitness stored in cache
subroutine UpdateInjPool( sgaParam, nCachedFits )
implicit none
	!argument                                                                                                                                                                                         
	type(SGAPARAMSTRUCT), intent(inout) :: sgaParam
	integer, intent(in) :: nCachedFits

	!variables
	double precision, dimension(nCachedFits) :: arrCachedFits, arrCachedFitStds
	integer :: i

	do i=lbound(sgaParam%arrFitInj, 1), ubound(sgaParam%arrFitInj,1)
		if( sgaParam%arrFitInj(i)>=1d80 )cycle

		if( SearchChromCache(hChromCache, sgaParam%arrPopInj(i,:), sgaParam%nChromBits, -1, arrCachedFits, nCachedFits)/=0 )then
			call GetStatChromCache(hChromCache, sgaParam%arrPopInj(i,:), sgaParam%nChromBits, arrCachedFits, arrCachedFitStds )

			sgaParam%arrFitInj(i) = arrCachedFits(3)
			sgaParam%arrFitStdsInj(i) = arrCachedFitStds(3)
			sgaParam%arrCostsInj(i) = arrCachedFits(1)
			sgaParam%arrCostStdsInj(i) = arrCachedFitStds(1)
			sgaParam%arrViolsInj(i) = arrCachedFits(2)
			sgaParam%arrViolStdsInj(i) = arrCachedFits(2)
		endif
	enddo
end subroutine

subroutine UpdateSSD( sgaParam, handles, rStdBase, rSmoothStd, rSmoothStdDev )
implicit none
	type(SGAPARAMSTRUCT), intent(inout) :: sgaParam
	type(MODELHANDLESTRUCT), intent(in) :: handles
	double precision, intent(inout) :: rStdBase, rSmoothStd, rSmoothStdDev

	integer :: i
	double precision, dimension(sgaParam%nPopSize, 4*g_nRemWells) :: arrVars			!the vars for parsing and normalize the NN parameters.
	double precision, dimension(3*g_nRemWells) :: arrMeans, arrStds
	double precision :: rAvgStd, rDiff

	do i=1, sgaParam%nPopSize
		call DecodeOne( sgaParam, i )
		call wrapPrepFunc( i, sgaParam%arrVars, sgaParam%nPopSize, sgaParam%nVarCount )

		call ParseVars( arrVars(i,:) )
		call Normalize( handles%hHeadMeanNorm, arrVars(i,:), 1 )
	enddo

	call CalcMeanStd( arrVars(:,1:3*g_nRemWells), arrMeans, arrStds )

	rAvgStd = sum( arrStds ) / ( ubound(arrStds,1)-lbound(arrStds,1)+1 )

	if( sgaParam%iCurGen==1 )then
		rStdBase = rAvgStd
		rSmoothStd = rAvgStd
		rSmoothStdDev = 0
	else
		!so jacon's algorithm smooth adding!
		rDiff = rAvgStd - rSmoothStd
		rSmoothStd = rSmoothStd + 0.5*(rDiff)
		rSmoothStdDev = rSmoothStdDev + 0.5*( abs(rDiff) - rSmoothStdDev )
	endif
end subroutine

subroutine UpdateAnns( sgaParam, handles, nCachedFits )
use neuronet
implicit none
	!arguments
	type(SGAPARAMSTRUCT), intent(inout) :: sgaParam
	type(MODELHANDLESTRUCT), intent(inout) :: handles
	integer :: nCachedFits

	!variables
	integer, dimension( g_nRemWells ) :: arrWellIndex
	double precision, dimension(4*g_nRemWells+2) :: arrVars			!the vars for parsing and normalize the NN parameters.
	double precision, dimension(nCachedFits) :: arrCachedFits, arrCachedFitStds
	double precision, dimension(g_nRemWells) :: arrHeads, arrHeadStds
	double precision :: rRiskB, rRiskW, rRiskBStd, rRiskWStd, rHeadMse, rRiskMse
	character(80) :: strHeadFile, strHeadStdFile, strRiskFile, strRiskStdFile
	integer :: i, k, index, start_gen

	index = 5
	start_gen = 3
	do i=1, sgaParam%nPopSize
		if( sgaParam%arrFlags(i)/=PDE_EVAL )cycle

		call DecodeOne( sgaParam, i )
		call wrapPrepFunc( i, sgaParam%arrVars, sgaParam%nPopSize, sgaParam%nVarCount )

		!point to the correct file
		g_nPooledPdes = g_nPooledPdes + 1
		if( mod(g_nPooledPdes,4)/=0 )then
			strHeadFile = strHeadTrain
			strRiskFile = strRiskTrain
		else
			strHeadFile = strHeadTest
			strRiskFile = strRiskTest
		endif

		!retrieve the neural network outputs
		call GetStatChromCache(hChromCache, sgaParam%arrPop(i,:), sgaParam%nChromBits, arrCachedFits, arrCachedFitStds )

		!parse the variables.
		arrHeads = arrCachedFits( index:index+g_nRemWells-1 )
		arrHeadStds = arrCachedFitStds( index : index+g_nRemWells-1 )
		rRiskB = arrCachedFits( index+2*g_nRemWells )
		rRiskW = arrCachedFits( index+2*g_nRemWells+1 )
		rRiskBStd = arrCachedFitStds( index+2*g_nRemWells )
		rRiskWStd = arrCachedFitStds( index+2*g_nRemWells+1 )

		!order the wells by their number first
		arrWellIndex = (/(k, k=1, g_nRemWells)/)
		call qsort( arrWellIndex, g_nRemWells, sizeof(arrWellIndex(1)), WellCompare )

		!write the head training data into file
		call ParseVars( arrVars, arrHeads )
		call Normalize( handles%hHeadMeanNorm, arrVars, 1 )
		call OrderVars( arrVars, arrWellIndex )
		call AppendToFile( strHeadFile, arrVars, 4*g_nRemWells )

		!write the risk training data into file
		call ParseVars( arrVars, arrHeads, rRiskB, rRiskW )
		call Normalize( handles%hRiskMeanNorm, arrVars, 1 )
		call OrderVars( arrVars, arrWellIndex )
		call AppendToFile( strRiskFile, arrVars, 4*g_nRemWells+2 )

	enddo

	if( (g_nPooledPdes>g_nPoolSize) .and. (sgaParam%iCurGen>=start_gen) .and. (sgaParam%iCurGen/=sgaParam%nMaxGens) )then
!	if( .false. )then

		rHeadMSE = TrainMatlabNetEx( strHeadM, arrHeadNet, 3, (/0.d0,0.d0,0.d0,0.0d0/), strHeadTrain, strHeadNN, strHeadTest )
		rRiskMSE = TrainMatlabNetEx( strRiskM, arrRiskNet, 3, (/-0.2454545d0,0.225d0,0.0d0/), strRiskTrain, strRiskNN, strRiskTest )

		call ReleaseAnns( handles%hHeadMeanModel, handles%hRiskMeanModel )

		call CreateAnns( handles%hHeadMeanModel, handles%hRiskMeanModel )
		call LoadMatlabAnns( handles%hHeadMeanModel, handles%hRiskMeanModel )

		!empty the pool
		g_nPooledPdes = 0
	endif
end subroutine

!instead of adding the training/testing records one by one, 
!the UpdateAnnsEx function add the records as a batch, these records can be retrieved from the Cache.
subroutine UpdateAnnsEx( sgaParam, handles, nCachedFits )
use neuronet
implicit none
	!arguments
	type(SGAPARAMSTRUCT), intent(inout) :: sgaParam
	type(MODELHANDLESTRUCT), intent(inout) :: handles
	integer :: nCachedFits

	!variables
	double precision :: rHeadMse, rRiskMse
	integer :: i, start_gen

	start_gen = 3

	do i=1, sgaParam%nPopSize
		if( sgaParam%arrFlags(i)/=PDE_EVAL )cycle
		!still update the pool, as it trigers the ANN retraining.
		g_nPooledPdes = g_nPooledPdes + 1
	enddo

	if( (g_nPooledPdes>g_nPoolSize) .and. (sgaParam%iCurGen>=start_gen) .and. (sgaParam%iCurGen/=sgaParam%nMaxGens) )then
!	if( .false. )then

		call CleanTrainingFiles
		call ForEachChromCache( hChromCache, SaveChroms )

		rHeadMSE = TrainMatlabNetEx( strHeadM, arrHeadNet, 3, (/0.d0,0.d0,0.d0,0.0d0/), strHeadTrain, strHeadNN, strHeadTest )
		rRiskMSE = TrainMatlabNetEx( strRiskM, arrRiskNet, 3, (/-0.2454545d0,0.225d0,0.0d0/), strRiskTrain, strRiskNN, strRiskTest )

		call ReleaseAnns( handles%hHeadMeanModel, handles%hRiskMeanModel )

		call CreateAnns( handles%hHeadMeanModel, handles%hRiskMeanModel )
		call LoadMatlabAnns( handles%hHeadMeanModel, handles%hRiskMeanModel )

		!empty the pool
		g_nPooledPdes = 0
	endif

contains

	!this is a function called by c++ for_each().
	!the function was passed the chromosomes and is expected to save the information to a file.
	subroutine SaveChroms( arrChroms, count )
	implicit none
		!arguments
		integer, dimension(*) :: arrChroms
		integer :: count

		!variables
		integer :: i, k, index
		integer, dimension(g_sgaParam%nChromBits) :: arrTempBits

		integer, dimension( g_nRemWells ) :: arrWellIndex
		double precision, dimension(4*g_nRemWells+2) :: arrVars			!the vars for parsing and normalize the NN parameters.
		double precision, dimension(nCachedFits) :: arrCachedFits, arrCachedFitStds
		double precision, dimension(g_nRemWells) :: arrHeads
		double precision :: rRiskB, rRiskW
		character(80) :: strHeadFile, strRiskFile

		!save the first individual
		arrTempBits = g_sgaParam%arrPop(1,:)
		!repleace the first individual with the passed chromosome
		do i=1, g_sgaParam%nChromBits
			g_sgaParam%arrPop(1,i) = arrChroms(i)
		enddo

		call DecodeOne( sgaParam, 1 )
		call wrapPrepFunc( 1, sgaParam%arrVars, sgaParam%nPopSize, sgaParam%nVarCount )

		!retrieve the neural network outputs
		call GetStatChromCache(hChromCache, sgaParam%arrPop(1,:), sgaParam%nChromBits, arrCachedFits, arrCachedFitStds )

		index = 5

		!parse the variables.
		arrHeads = arrCachedFits( index:index+g_nRemWells-1 )
		rRiskB = arrCachedFits( index+2*g_nRemWells )
		rRiskW = arrCachedFits( index+2*g_nRemWells+1 )

		!order the wells by their number first
		arrWellIndex = (/(k, k=1, g_nRemWells)/)
		call qsort( arrWellIndex, g_nRemWells, sizeof(arrWellIndex(1)), WellCompare )

		if( mod(count,4)/=0 )then
			strHeadFile = strHeadTrain
			strRiskFile = strRiskTrain
		else
			strHeadFile = strHeadTest
			strRiskFile = strRiskTest
		endif

		!write the head training data into file
		call ParseVars( arrVars, arrHeads )
		call Normalize( handles%hHeadMeanNorm, arrVars, 1 )
		call OrderVars( arrVars, arrWellIndex )
		call AppendToFile( strHeadFile, arrVars, 4*g_nRemWells )

		!write the risk training data into file
		call ParseVars( arrVars, arrHeads, rRiskB, rRiskW )
		call Normalize( handles%hRiskMeanNorm, arrVars, 1 )
		call OrderVars( arrVars, arrWellIndex )
		call AppendToFile( strRiskFile, arrVars, 4*g_nRemWells+2 )

		!save the first individual
		g_sgaParam%arrPop(1,:) = arrTempBits
	end subroutine

end subroutine

subroutine CalcStat( arrVars, arrMeans, arrStds, arrMins, arrMaxs )                                                                                                                                              
implicit none                                                                                                                                                                                     
	!arguments                                                                                                                                                                                
	double precision, dimension(:,:) :: arrVars                                                                                                                                               
	double precision, dimension(:), intent(out) :: arrMeans, arrStds, arrMins, arrMaxs                                                                                                                         
                                                                                                                                                                                                      
	!variables                                                                                                                                                                                
	integer :: nRows, nCols, i, j                                                                                                                                                             
                                                                                                                                                                                                      
	nRows = ubound(arrVars,1)-lbound(arrVars,1)+1                                                                                                                                             
	nCols = ubound(arrVars,2)-lbound(arrVars,2)+1                                                                                                                                             
                                                                                                                                                                                                      
	arrMeans = sum( arrVars, 1 )                                                                                                                                                              
	arrMeans = arrMeans / (ubound(arrVars,1)-lbound(arrVars,1)+1)
	arrMins = arrVars(1,:)
	arrMaxs = arrVars(1,:)
                                                                                                                                                                                                      
	arrStds = 0                                                                                                                                                                               
	do i=1, nRows                                                                                                                                                                             
		do j=1, nCols                                                                                                                                                                     
			arrStds(j) = arrStds(j) + (arrVars(i,j)-arrMeans(j))**2                                                                                                                   
			arrMins(j) = min( arrMins(j), arrVars(i,j) )
			arrMaxs(j) = max( arrMaxs(j), arrVars(i,j) )
		enddo                                                                                                                                                                             
	enddo
                                                                                                                                                                                                      
	do i=1, nCols                                                                                                                                                                             
		arrStds(i) = sqrt( arrStds(i)/(nRows-1) )
	enddo
end subroutine

subroutine SavePopStat(hFile, sgaParam)
implicit none
	!arguments
	type(SGAPARAMSTRUCT), intent(inout) :: sgaParam
	integer, intent(in) :: hFile
	double precision :: arrMeans(1), arrStds(1), arrMins(1), arrMaxs(1)
	double precision :: arrFit(sgaParam%nPopSize, 1)

	!variables
	integer :: j, id

	if( sgaParam%iCurGen==1 )then
		write(hFile, 1000)
	endif

	arrFit(:,1) = sgaParam%arrFit
	call CalcStat( arrFit, arrMeans, arrStds, arrMins, arrMaxs )

	write( hFile, fmt=1001 ) sgaParam%iCurGen, arrMins(1), arrMaxs(1), arrMeans(1), arrStds(1)
                   
	1000 format('GEN', 8x, 8x, 'min', 4x, 8x, 'max', 4x, 8x, 'mean', 4x, 8x, 'std' )
	1001 format(i4, 8x, f12.4, 4x, f12.4, 4x, f12.4, 4x, f12.4)
end subroutine SavePopStat

subroutine SaveGenInfo( sgaParam, hGaOut, nCachedFits )
implicit none
	type(SGAPARAMSTRUCT), intent(inout) :: sgaParam
	integer, intent(in) :: hGaOut, nCachedFits

	!variables
	integer :: i, id, index, samples
	double precision, dimension(2) :: arFit, arCost, arViol, arTotCost, arRiskB, arRiskW
	double precision, dimension(nCachedFits) :: arrCachedFits, arrCachedFitStds
	double precision, dimension(g_nRemWells) :: arrHeads, arrHeadStds
	integer, dimension(sgaParam%nPopSize) :: arrIds

	arrIds = (/ (i, i=1,sgaParam%nPopSize) /)
	call qsort( arrIds, sgaParam%nPopSize, sizeof(arrIds(1)), CompareByCostViolTest )
!	call qsort( arrIds, sgaParam%nPopSize, sizeof(arrIds(1)), CompareByFitness )

	index = 5

	open(unit=hGaOut, file=strGaOut, status='unknown', position='append' )
	write( hGaOut, 1000 ) sgaParam%iCurGen

	do i=1, sgaParam%nPopSize
		id = arrIds(i)
		call DecodeOne( sgaParam, id )
		call wrapPrepFunc( id, sgaParam%arrVars, sgaParam%nPopSize, sgaParam%nVarCount )

		arFit = (/ sgaParam%arrFit(id), sgaParam%arrFitStds(id) /)
		arCost = (/ sgaParam%arrCosts(id), sgaParam%arrCostStds(id) /)
		arViol = (/ sgaParam%arrViols(id), sgaParam%arrViolStds(id) /)
		arrHeads = 0
		arrHeadStds = 0
		arRiskB = 0
		arRiskW = 0
		samples = 0
		if( SearchChromCache(hChromCache, sgaParam%arrPop(id,:), sgaParam%nChromBits, -1, arrCachedFits, nCachedFits)/=0 )then
			call GetStatChromCache(hChromCache, sgaParam%arrPop(id,:), sgaParam%nChromBits, arrCachedFits, arrCachedFitStds )
			samples = GetSampleCountCache( hChromCache, sgaParam%arrPop(id,:), sgaParam%nChromBits )
			arFit = (/ arrCachedFits(3), arrCachedFitStds(3) /)
			arTotCost = (/ arrCachedFits(4), arrCachedFitStds(4) /)
			arRiskB = (/ arrCachedFits(index+2*g_nRemWells), arrCachedFitStds(index+2*g_nRemWells) /)
			arRiskW = (/ arrCachedFits(index+2*g_nRemWells+1), arrCachedFitStds(index+2*g_nRemWells+1) /)
			arrHeads = (/ arrCachedFits(index:index+g_nRemWells-1) /)
			arrHeadStds = (/ arrCachedFitStds(index:index+g_nRemWells-1) /)
		endif

		call SaveIndInfo( id, sgaParam%arrFlags(id), samples, arFit, arCost, arViol, arTotCost, arRiskB, arRiskW, &
			& g_arrWellLocs, g_arrPumpRates, arrHeads, arrHeadStds, sgaParam%arrPop(id,:) )

		arCost(1) = ComputeCost(arrHeads, g_arrOldHeads, g_arrPumpRates, g_nRemWells)
		arCost(2) = ComputeCostStd( arrHeads, arrHeadStds, g_arrOldHeads, g_arrPumpRates, g_nRemWells )
		arViol(1) = ComputeViolation(arrHeads, g_arrOldHeads, g_arrPumpRates, g_nRemWells, arRiskB(1), arRiskW(1) )
		arViol(2) = ComputeViolationStd( arrHeads, arrHeadStds, g_arrOldHeads, g_arrPumpRates, g_nRemWells, arRiskB(1), arRiskW(1), arRiskB(2), arRiskW(2) )

!		call SaveIndInfo( i, sgaParam%arrFlags(i), arFit, arCost, arViol, arTotCost, arRiskB, arRiskW, &
!			& g_arrWellLocs, g_arrPumpRates, arrHeads, arrHeadStds, sgaParam%arrPop(i,:) )

	enddo
	close( hGaOut )

	open(unit=hPopStat, file=strPopStat, status='unknown', position='append' )
	call SavePopStat( hPopStat, sgaParam )
	close( hPopStat )


	1000 format( 3('#'), 'Generation ', i4, 2x, 3('#'), 'fitness cost violations, totcost, risb, riskw, loc[], Q[], H[]', 10('#') )

contains
	subroutine SaveIndInfo( id, nFlag, samples, arFit, arCost, arViol, arTotCost, arRiskB, arRiskW, &
		& arrWellLocs, arrPumpRates, arrHeads, arrHeadStds, arrChroms )
	implicit none

	!arguments
	integer, intent(in) :: id, nFlag, samples
	double precision, dimension(2) :: arFit, arCost, arViol, arTotCost, arRiskB, arRiskW
	integer, dimension(:) :: arrWellLocs
	double precision, dimension(:) :: arrPumpRates, arrHeads, arrHeadStds
	integer, dimension(:) :: arrChroms

	!variables
	integer :: j

	if( nFlag==NN_EVAL )then
		write(hGaOut,fmt=1001,advance="NO")id, samples
	else if( nFlag==CACHE_EVAL )then
		write(hGaOut,fmt=1002,advance="NO")id, samples
	else if( nFlag==PDE_EVAL )then
		write(hGaOut,fmt=1003,advance="NO")id, samples
	else
		call assert(.false.)
	endif

	write(hGaOut,fmt=1100,advance="NO")arFit, arCost, arViol, arTotCost, arRiskB, arRiskW

	!well locations
	do j=1, g_nRemWells                                                                                                                                                                       
		write( hGaOut, fmt=1200, advance="NO" )arrWellLocs(j)
	enddo
	!well pumping rates
	do j=1, g_nRemWells
		write( hGaOut, fmt=1300, advance="NO" )arrPumpRates(j)
	enddo
	!well hydraulic head
	do j=1, g_nRemWells
		write( hGaOut, fmt=1400, advance="NO" )arrHeads(j), arrHeadStds(j)
	enddo
	do j=lbound(arrChroms,1), ubound(arrChroms,1)
		write( hGaOut, fmt=1500, advance="NO" )arrChroms(j)
	enddo
	!just print the catridge
	write(hGaOut, fmt=1600, advance="YES")

	1001 format( i4, '(A-', i3, ')' )
	1002 format( i4, '(C-', i3, ')' )
	1003 format( i4, '(P-', i3, ')' )

	1100 format( 2x, 4(f17.6, '(', f14.6, ')'), 2(f15.12, '(', f15.12, ')') )
	1200 format( 2x, i2)
	1300 format(2x, f9.4)
	1400 format(2x, 2(f9.4, '(', f9.4, ')') )
 	1500 format(1x,i1)
	1600 format(1x)
	end subroutine

end subroutine

subroutine SaveCountInfo( sgaParam, nPdeIds, nJobCount )
implicit none
	type(SGAPARAMSTRUCT), intent(inout) :: sgaParam
	integer, intent(in) :: nPdeIds, nJobCount

	!variables
	integer, save :: nTotalPdeIds, nTotalSimulations
	integer :: i, nPdeCount, nAnnCount, nCacheCount

	nPdeCount = 0
	nAnnCount = 0
	nCacheCount = 0
	do i=1, sgaParam%nPopSize
		if( sgaParam%arrFlags(i)==NN_EVAL )then
			nAnnCount = nAnnCount + 1
		else if( sgaParam%arrFlags(i)==PDE_EVAL )then
			nPdeCount = nPdeCount + 1
		else
			nCacheCount = nCacheCount + 1
		endif
	enddo

	nTotalPdeIds = nTotalPdeIds + nPdeIds
	nTotalSimulations = nTotalSimulations + nJobCount

	if( sgaParam%iCurGen==1 )then
		write(hGaCount, 1000)
	endif

	open(unit=hGaCount, file=strGaCount, status='unknown', position='append')
	write( hGaCount, 1001 )sgaParam%iCurGen, nTotalPdeIds, nTotalSimulations, nPdeCount, nCacheCount, nAnnCount
	close( hGaCount )

	1000 format('GEN', 2x, 'TOTPDE', 2x, 'TOTSIM', 2x, 'PDE#', 2x, 'CACHE#', 2x, 'ANN#' )
	1001 format( i3, 2x, 5(i5, 2x) )

end subroutine
		

!using matlab to create neural network                                                                                                                                                                    
subroutine FitEvalNet(sgaParam)
use condorio
USE DFPORT                                                                                                                                                                                                
implicit none                                                                                                                                                                                             
	!argument                                                                                                                                                                                         
	type(SGAPARAMSTRUCT), intent(inout) :: sgaParam

	!variable                                                                                                                                                                                         
	integer :: i, j, k, id
	logical :: bAnnReady                                                                                                                                                        

	!variables for caching
	double precision, dimension(:), allocatable :: arrCachedFits
	integer :: nCachedFits

	!variables for adaptive sampling control
	integer :: nInitSampling = 20
	double precision, save :: rStdBase, rSmoothStd, rSmoothStdDev

	!variables for sorting
	integer, dimension( sgaParam%nPopSize ) :: arrIndex
	
	!variables for pde evaluation	
	integer :: arrPdeIds(sgaParam%nPopSize)
	integer :: nExpectedPdes, nPdeIds

	!variables for distributed computation
	integer, dimension(:), allocatable :: arrJobIds, arrJobIds2
	integer :: nJobCount, nSamples

	double precision :: rCost, rViols

	!handles 
	type(MODELHANDLESTRUCT) :: handles

	nJobCount = 0
	if( sgaParam%iCurGen==1 )then
		!prepare ncp static folder
		call MakeStaticFolder
		!prepare output files
		call PrepareOutputFiles
		!no pde is in pool yet.
		g_nPooledPdes = 0
	endif

	nCachedFits = 4 + 2*g_nRemWells + 2		!cost, viols, fitness, totcost, heads(:), oldheads(:), riskb, riskw
	allocate( arrCachedFits(nCachedFits) )

	!choose the realizations using latin tube
	call ChooseRealizations( sgaParam, sgaParam%arrRealizations )

	!create scaling package to normalize inputs and outputs for both head and risk models
	call CreateScalings( handles%hHeadMeanNorm, handles%hRiskMeanNorm )
	!create ANNs
	call CreateANNs( handles%hHeadMeanModel, handles%hRiskMeanModel )

	!check the availablity of the ANNs
	inquire( FILE=strHeadNN, EXIST=bAnnReady )
	if( sgaParam%iCurGen==sgaParam%iStartGen+sgaParam%nMaxGens-1 )bAnnReady = .false.
!	bAnnReady = .false.

	if( bAnnReady )then
		!load neural net weights.
		call LoadMatlabAnns( handles%hHeadMeanModel, handles%hRiskMeanModel )
	endif

!	write( hGaOut, 1111 ) sgaParam%iCurGen
!	write( hGaNN, 1111 ) sgaParam%iCurGen

	!######################### Pass 1 - evaluate the fitness by the ANNs #########################
	sgaParam%arrFlags = -1
	if( bAnnReady )then
		call EvalByAnns( sgaParam, handles )
	endif

	!######################### Pass 2 - evaluate the fitness by the CACHE ###########################3
	arrIndex = (/(i, i=1,sgaParam%nPopSize)/)
	call EvalByCache( sgaParam, arrIndex, sgaParam%nPopSize, nCachedFits, CACHE_EVAL )

	!######################### Pass 3 - evaluate the fitness by the PDEs ###########################3
	if( bAnnReady )then
		!compute the expected sampling number
		nExpectedPdes = nInitSampling * (rSmoothStd-rSmoothStdDev)/(rStdBase - rSmoothStdDev)
		
		!sample by best sampling strategy or tournament sampling strategy
		if( sgaParam%iCurGen>100 )then
!			call qsort( arrIndex, sgaParam%nPopSize, sizeof(arrIndex(1)), FitCompare )
			call qsort( arrIndex, sgaParam%nPopSize, sizeof(arrIndex(1)), CompareByCostViolTest )
			call BestSelPdes( sgaParam, arrIndex, nExpectedPdes, arrPdeIds, nPdeIds)
		else
			call TournSelPdes( sgaParam, 5, nExpectedPdes, arrPdeIds, nPdeIds )
		endif
	else
		nPdeIds = 0
		do i = 1, sgaParam%nPopSize
			if( sgaParam%arrFlags(i)/=CACHE_EVAL )then
				nPdeIds = nPdeIds + 1
				arrPdeIds( nPdeIds ) = i
			endif
		enddo
	endif

	!broadcast the jobs to the distributed environment
	nSamples = ubound(sgaParam%arrRealizations,2) - lbound(sgaParam%arrRealizations,2) + 1
	nJobCount = 0
	allocate( arrJobIds(sgaParam%nPopSize*nSamples), arrJobIds2(sgaParam%nPopSize*nSamples) )

	do i = 1, nPdeIds
		id = arrPdeIds(i)
		call DecodeOne( sgaParam, id )
		call wrapPrepFunc( id, sgaParam%arrVars, sgaParam%nPopSize, sgaParam%nVarCount )

		do k=1, nSamples
			if( SearchChromCache( hChromCache, sgaParam%arrPop(id,:), sgaParam%nChromBits, sgaParam%arrRealizations(id,k), arrCachedFits, nCachedFits )==0 )then
				arrJobIds( nJobCount+1 ) = id*1000+k
				arrJobIds2( nJobCount+1 ) = id*1000 + sgaParam%arrRealizations(id,k)
				call BroadcastSlave( arrJobIds2(nJobCount+1), sgaParam%iCurGen )
				nJobCount = nJobCount + 1

				call InsertChromCache( hChromCache, sgaParam%arrPop(id,:), sgaParam%nChromBits, sgaParam%arrRealizations(id,k), arrCachedFits, nCachedFits )
			else
				!this sample is cached already, increase the sample count
				call AddSampleChromCache( hChromCache, sgaParam%arrPop(id,:), sgaParam%nChromBits, sgaParam%arrRealizations(id,k) )
			endif
		enddo
	end do

	!route the jobs
	if( nJobCount>0 )then 
		call WaitForSlaves( arrJobIds2, nJobCount )
	endif

	!collect the job results
	do i=1, nJobCount
		id = arrJobIds(i) / 1000
		k = mod(arrJobIds(i), 1000)
		call DecodeOne( sgaParam, id )
		call wrapPrepFunc( id, sgaParam%arrVars, sgaParam%nPopSize, sgaParam%nVarCount )

		call CollectSlave( arrJobIds2(i), sgaParam%iCurGen, g_rFitness, g_rTotCost, g_rPenHead, g_rPenRisk, &
					& g_rRiskB, g_rRiskW, g_arrHeads, g_arrOldHeads, g_nRemWells )

		!save the  data into the cache
		rCost = ComputeCost( g_arrHeads, g_arrOldHeads, g_arrPumpRates, g_nRemWells )
		rViols = ComputeViolation( g_arrHeads, g_arrOldHeads, g_arrPumpRates, g_nRemWells, g_rRiskB, g_rRiskW )
		arrCachedFits = (/rCost, rViols, g_rFitness, g_rTotCost, g_arrHeads, g_arrOldHeads, g_rRiskB, g_rRiskW /)

		call ReplaceChromCache( hChromCache, sgaParam%arrPop(id,:), sgaParam%nChromBits, sgaParam%arrRealizations(id,k), arrCachedFits, nCachedFits )
	enddo
	deallocate( arrJobIds, arrJobIds2 )

	!let the cache do the dirty job for me, evaluate the pde results.
	call EvalByCache( sgaParam, arrPdeIds, nPdeIds, nCachedFits, PDE_EVAL )
	call UpdateInjPool( sgaParam, nCachedFits )

	!this is the right place to save the output files
	call SaveGenInfo( sgaParam, hGaOut, nCachedFits )
	call SaveCountInfo( sgaParam, nPdeIds, nJobCount )

	!######################### Update SSD and ANNs ###########################
	call UpdateSSD( sgaParam, handles, rStdBase, rSmoothStd, rSmoothStdDev )

	write( hGaCount, * )nInitSampling * (rSmoothStd-rSmoothStdDev)/(rStdBase - rSmoothStdDev)

	!do injection???????????????

	!update the ANNs
	call UpdateAnnsEx( sgaParam, handles, nCachedFits )

	!######################### Cleanup ####################################
	deallocate( arrCachedFits )
	call ReleaseAnns( handles%hHeadMeanModel, handles%hRiskMeanModel )
	call ReleaseScalings( handles%hHeadMeanNorm, handles%hRiskMeanNorm )

	call RefreshHosts
!	call Dump( hChromCache )

contains
	integer(2) function FitCompare(a1, a2)
		integer :: a1, a2

		double precision :: r1, r2
		r1 = sgaParam%arrFit(a1)
		r2 = sgaParam%arrFit(a2)

		if( (r1 - r2) == 0 )then
			FitCompare = 0
		else if( r1>r2 )then
			FitCompare = 1
		else
			FitCompare = -1
		endif
	end function FitCompare
end subroutine

subroutine FitEval(sgaParam)
use stdfor
use fmt_helper
use ncpfor
use condorio
USE DFPORT
implicit none
	!argument
	type(SGAPARAMSTRUCT), intent(inout) :: sgaParam

	!variable
	integer :: i, id, k

	double precision, dimension(sgaParam%nPopSize, g_nRemWells+2+1+2+1) :: arrFits	!save all the heads, two risk values, total cost, head and risk penalties and fitnesses into the chromosome cache

!	integer :: arrJobIds( sgaParam%nPopSize * (ubound(sgaParam%arrRealizations,2)-lbound(sgaParam%arrRealizations,2)+1) )
!	integer :: arrJobIds2( sgaParam%nPopSize * (ubound(sgaParam%arrRealizations,2)-lbound(sgaParam%arrRealizations,2)+1) )
!	integer :: arrCachedIds( sgaParam%nPopSize * (ubound(sgaParam%arrRealizations,2)-lbound(sgaParam%arrRealizations,2)+1) )
	integer :: arrJobIds( sgaParam%nPopSize )
	integer :: nJobCount, nFits

	!assume dynaflag is always .false.
	if( sgaParam%iCurGen==1 )then
		i = RemoveFileC( strGaOut )
	endif

	open(unit=hGaOut, file=strGaOut, status='unknown', position='append' )

	write( hGaOut, 1111 ) sgaParam%iCurGen

	nFits = g_nRemWells + 2 + 1 + 2 + 1				!heads(nRemwells), 2 risks, 1 total cost, 2 penalties(head,risk), 1 fitness
	nJobCount = 0
	do i = 1, sgaParam%nPopSize
		call DecodeOne( sgaParam, i )
		call wrapPrepFunc( i, sgaParam%arrVars, sgaParam%nPopSize, sgaParam%nVarCount )

		if( SearchChromCache( hChromCache, sgaParam%arrPop(i,:), sgaParam%nChromBits, 0, arrFits(i,:), nFits )==0 )then
			!call CostWrapper
			nJobCount = nJobCount+1
			arrJobIds( nJobCount ) = i
			call BroadcastSlave( i, sgaParam%iCurGen )
			call InsertChromCache( hChromCache, sgaParam%arrPop(i,:), sgaParam%nChromBits, 0, arrFits(i,:), nFits )
		else
			id = i
			g_arrHeads = arrFits(id, 1:g_nRemWells)
			g_rRiskB = arrFits(id, g_nRemWells+1)
			g_rRiskW = arrFits(id, g_nRemWells+2)
			g_rFitness = arrFits(id, g_nRemWells+6)

			sgaParam%arrFit(id) = g_rFitness
			sgaParam%arrFlags(id) = CACHE_EVAL
			call SaveIndInfo(id)
		endif
	end do

	!route the jobs
	if( nJobCount>=1 )then
		call WaitForSlaves( arrJobIds, nJobCount )
	endif

	!load the job results
	do id=1, nJobCount
		i = arrJobIds(id)
		call DecodeOne( sgaParam, i )
		call wrapPrepFunc( i, sgaParam%arrVars, sgaParam%nPopSize, sgaParam%nVarCount )
		call CollectSlave( i, sgaParam%iCurGen, g_rFitness, g_rTotCost, g_rPenHead, g_rPenRisk, &
					& g_rRiskB, g_rRiskW, g_arrHeads, g_arrOldHeads, g_nRemWells )

		!save data for analysis and ouputs.
		arrFits(i, 1:g_nRemWells) = g_arrHeads
		arrFits(i, g_nRemWells+1) = g_rRiskB
		arrFits(i, g_nRemWells+2) = g_rRiskW
		arrFits(i, g_nRemWells+3) = g_rTotCost
		arrFits(i, g_nRemWells+4) = g_rPenHead
		arrFits(i, g_nRemWells+5) = g_rPenRisk
		arrFits(i, g_nRemWells+6) = g_rFitness

		sgaParam%arrFit(id) = g_rFitness

		call ReplaceChromCache( hChromCache, sgaParam%arrPop(i,:), sgaParam%nChromBits, 0, arrFits(i,:), nFits )

		call SaveIndInfo( i )
	enddo

	close( hGaOut )

	1111 format(/'#################  Generation',i5,'  #################')
contains
	subroutine SaveIndInfo(id)
	use sga                                                                                                                                                                                           
	use fmt_helper                                                                                                                                                                                    
	use casewell                                                                                                                                                                                      
	implicit none                                                                                                                                                                                     
		!arguments                                                                                                                                                                                
		integer, intent(in) :: id                                                                                                                                                                 
                                                                                                                                                                                                          
		integer :: j                                                                                                                                                                              
                                                                                                                                                                                                          
		if( .true. )then                                                                                                                                                                        
			write( hGaOut, fmt=1075, advance="NO" ) i, sgaParam%arrFit(i), g_rTotCost, g_rPenHead, g_rPenRisk, g_rRiskB, g_rRiskW                                                             
		else                                                                                                                                                                                      
			write( hGaOut, fmt=1076, advance="NO" ) i, sgaParam%arrFit(i), g_rTotCost, g_rPenHead, g_rPenRisk, g_rRiskB, g_rRiskW                                                             
		endif                                                                                                                                                                                     
		!well locations                                                                                                                                                                           
		do j=1, g_nRemWells                                                                                                                                                                       
			write( hGaOut, fmt=1078, advance="NO" )g_arrWellLocs(j)
		enddo
		!well pumping rates
		do j=1, g_nRemWells
			write( hGaOut, fmt=1077, advance="NO" )g_arrPumpRates(j)
		enddo
		!well hydraulic head
		do j=1, g_nRemWells
			write( hGaOut, fmt=1077, advance="NO" )g_arrHeads(j)
		enddo
		!well pumping flags and installation flags
!		do j=2*g_nRemWells+1, sgaParam%nVarCount
!			write( hGaOut, fmt=1079, advance="NO" )anint(sgaParam%arrVars(id,j))
!		enddo
		do j=1, sgaParam%nChromBits
			write( hGaOut, fmt=1082, advance="NO" )sgaParam%arrPop(id,j)
		enddo
		!just print the catridge
		write(hGaOut, fmt=1081, advance="YES")

		1075 format('*',i4,2x,2(f17.4,2x),4(f20.10,2x))
		1076 format(i4,2x,2(f17.4,2x),4(f20.10,2x))
		1077 format(2x,f6.1)
		1078 format(2x,i4)
		1079 format(2x,f2.0)
		1080 format(2x,f7.3)
		1081 format(1x)
		1082 format(2x,i2)
	end subroutine SaveIndInfo

end subroutine FitEval

subroutine FitEvalWithUncertainties(sgaParam)
use stdfor
use sga
use chmcache
use fmt_helper
use casewell
use neuroio
use ncpfor
use condorio
USE DFPORT
implicit none
	!argument
	type(SGAPARAMSTRUCT), intent(inout) :: sgaParam

	!variable
	integer :: i, id, k, ret

	!fitness and chached fitness
	double precision, allocatable, dimension(:,:) :: arrFits
	double precision, allocatable, dimension(:,:,:) :: arrCachedFits
	double precision, allocatable, dimension(:) :: arrCachedMeans, arrCachedStds
	double precision, dimension(sgaParam%nPopSize) :: arrFitMeans, arrFitStds
	!cached chromosomes
	integer, dimension( sgaParam%nChromBits+1 ) :: arrChrom
	!arrJobIds the virtual id of the job. arrJobIds2 the true job id
	integer, allocatable, dimension(:) :: arrJobIds, arrJobIds2, arrCachedIds

	integer :: nJobCount, nCachedCount, nCachedFits, nSamples
	double precision, dimension(g_nRemWells) :: arrHeads, arrHeadStds

	!assume dynaflag is always .false.
	if( sgaParam%iCurGen==1 )then
		i = RemoveFileC( strGaOut )
	endif

	nSamples = ubound(sgaParam%arrRealizations,2) - lbound(sgaParam%arrRealizations,2) + 1
	nCachedFits = g_nRemWells + 2 + 1 + 2 + 1				!heads(nRemwells), 2 risks, 1 total cost, 2 penalties(head,risk), 1 fitness
	nJobCount = 0
	nCachedCount = 0
	allocate( arrJobIds(sgaParam%nPopSize*nSamples), arrJobIds2(sgaParam%nPopSize*nSamples), arrCachedIds(sgaParam%nPopSize*nSamples) )
	allocate( arrCachedFits(sgaParam%nPopSize, nSamples, nCachedFits) )
	allocate( arrFits(sgaParam%nPopSize, nSamples) )
	allocate( arrCachedMeans(nCachedFits), arrCachedStds(nCachedFits) )

	open(unit=hGaOut, file=strGaOut, status='unknown', position='append' )
	write( hGaOut, 1111 ) sgaParam%iCurGen

	do i = 1, sgaParam%nPopSize
		call DecodeOne( sgaParam, i )
		call wrapPrepFunc( i, sgaParam%arrVars, sgaParam%nPopSize, sgaParam%nVarCount )

		do k=1, nSamples
!			arrChrom(1) = sgaParam%arrRealizations(i,k)
!			arrChrom(2:sgaParam%nChromBits+1 ) = sgaParam%arrPop(i,:)
			if( SearchChromCache( hChromCache, sgaParam%arrPop(i,:), sgaParam%nChromBits, sgaParam%arrRealizations(i,k), arrCachedFits(i,k,:), nCachedFits )==0 )then
				arrJobIds( nJobCount+1 ) = i*1000+k
				arrJobIds2( nJobCount+1 ) = i*1000 + sgaParam%arrRealizations(i,k)
				call BroadcastSlave( arrJobIds2(nJobCount+1), sgaParam%iCurGen )
				nJobCount = nJobCount + 1

				call InsertChromCache( hChromCache, sgaParam%arrPop(i,:), sgaParam%nChromBits, sgaParam%arrRealizations(i,k), arrCachedFits(i,k,:), nCachedFits )
			else
				arrCachedIds( nCachedCount+1 )=i*1000+k
				nCachedCount = nCachedCount + 1
			endif
		enddo
	end do

	!route the jobs
	if( nJobCount>0 )then 
		call WaitForSlaves( arrJobIds2, nJobCount )
	endif

	!load the job results
	do id=1, nJobCount
		i = arrJobIds(id) / 1000
		k = mod(arrJobIds(id), 1000)
		call DecodeOne( sgaParam, i )
		call wrapPrepFunc( i, sgaParam%arrVars, sgaParam%nPopSize, sgaParam%nVarCount )

		call CollectSlave( arrJobIds2(id), sgaParam%iCurGen, g_rFitness, g_rTotCost, g_rPenHead, g_rPenRisk, &
					& g_rRiskB, g_rRiskW, g_arrHeads, g_arrOldHeads, g_nRemWells )

		!save data for analysis and ouputs.
		arrFits( i, k ) = g_rFitness
 
		!Replace the chromosome cache.
		arrCachedFits(i, k, 1:g_nRemWells) = g_arrHeads
		arrCachedFits(i, k, g_nRemWells+1) = g_rRiskB
		arrCachedFits(i, k, g_nRemWells+2) = g_rRiskW
		arrCachedFits(i, k, g_nRemWells+3) = g_rTotCost
		arrCachedFits(i, k, g_nRemWells+4) = g_rPenHead
		arrCachedFits(i, k, g_nRemWells+5) = g_rPenRisk
		arrCachedFits(i, k, g_nRemWells+6) = g_rFitness
!		arrChrom(1) = sgaParam%arrRealizations(i,k)
!		arrChrom(2:sgaParam%nChromBits+1 ) = sgaParam%arrPop(i,:)
		call ReplaceChromCache( hChromCache, sgaParam%arrPop(i,:), sgaParam%nChromBits, sgaParam%arrRealizations(i,k), arrCachedFits(i,k,:), nCachedFits )
	enddo

	!for cached indivisuals, retrieve the fitness from the cache.
	do id=1, nCachedCount
		i = arrCachedIds(id) / 1000
		k = mod(arrCachedIds(id), 1000)
		call DecodeOne( sgaParam, i )
		call wrapPrepFunc( i, sgaParam%arrVars, sgaParam%nPopSize, sgaParam%nVarCount )

!		arrChrom(1) = sgaParam%arrRealizations(i,k)
!		arrChrom(2:sgaParam%nChromBits+1 ) = sgaParam%arrPop(i,:)
		if( SearchChromCache( hChromCache, sgaParam%arrPop(i,:), sgaParam%nChromBits, sgaParam%arrRealizations(i,k), arrCachedFits(i,k,:), nCachedFits )==0 )then
			print*, 'error caching'
			stop
		endif
		arrFits(i,k) = arrCachedFits(i,k,g_nRemWells+6)
		write(hGaOut,*)'cached id:', arrCachedIds(id)
	enddo

!	call CalcMeanStd( transpose(arrFits), arrFitMeans, arrFitStds )

	do i = 1, sgaParam%nPopSize
		ret = GetStatChromCache( hChromCache, sgaParam%arrPop(i,:), sgaParam%nChromBits, arrCachedMeans, arrCachedStds )
		sgaParam%arrFit(i) = arrCachedMeans( g_nRemWells+6 )
		arrHeads = arrCachedMeans(1:g_nRemWells)
		arrHeadStds = arrCachedStds(1:g_nRemWells)

		sgaParam%arrFlags(i) = PDE_EVAL
		call SaveIndInfo(sgaParam, i, arrCachedMeans( g_nRemWells+6 ), arrCachedStds( g_nRemWells+6 ), &
				& arrCachedMeans(g_nRemWells+3), arrCachedStds(g_nRemWells+3), arrHeads, arrHeadStds )
	end do

	close( hGaOut )

	call dump( hChromCache )

	1111 format(/'#################  Generation',i5,'  #################')
end subroutine

subroutine CalcMeanStd( arrVars, arrMeans, arrStds )                                                                                                                                              
implicit none                                                                                                                                                                                     
	!arguments                                                                                                                                                                                
	double precision, dimension(:,:) :: arrVars                                                                                                                                               
	double precision, dimension(:), intent(out) :: arrMeans, arrStds                                                                                                                          
                                                                                                                                                                                                      
	!variables                                                                                                                                                                                
	integer :: nRows, nCols, i, j                                                                                                                                                             
                                                                                                                                                                                                      
	nRows = ubound(arrVars,1)-lbound(arrVars,1)+1                                                                                                                                             
	nCols = ubound(arrVars,2)-lbound(arrVars,2)+1                                                                                                                                             
                                                                                                                                                                                                      
	arrMeans = sum( arrVars, 1 )                                                                                                                                                              
	arrMeans = arrMeans / (ubound(arrVars,1)-lbound(arrVars,1)+1)                                                                                                                             
                                                                                                                                                                                                      
	arrStds = 0                                                                                                                                                                               
	do i=1, nRows                                                                                                                                                                             
		do j=1, nCols                                                                                                                                                                     
			arrStds(j) = arrStds(j) + (arrVars(i,j)-arrMeans(j))**2                                                                                                                   
		enddo                                                                                                                                                                             
	enddo                                                                                                                                                                                     
                                                                                                                                                                                                      
	do i=1, nCols                                                                                                                                                                             
		arrStds(i) = sqrt( arrStds(i)/(nRows-1) )                                                                                                                                         
	enddo
end subroutine

subroutine SaveIndInfo(sgaParam, id, rFitness, rFitnessStd, rTotCost, rTotCostStd, arrHeads, arrHeadStds )
use sga
use casewell
implicit none
	!arguments
	type(SGAPARAMSTRUCT), intent(inout) :: sgaParam
	integer, intent(in) :: id
	double precision :: rFitness, rFitnessStd, rTotCost, rTotCostStd
	double precision, dimension(:) :: arrHeads, arrHeadStds
!	double precision, dimension(:) :: arrFitMeans, arrFitStds

	!variables
	integer :: j
                  
	write(hGaOut,fmt=1076,advance="NO" )id, rFitness, rFitnessStd, rTotCost, rTotCostStd

	!well locations                                                                                                                                                                           
	do j=1, g_nRemWells                                                                                                                                                                       
		write( hGaOut, fmt=1078, advance="NO" )g_arrWellLocs(j)
	enddo
	!well pumping rates
	do j=1, g_nRemWells
		write( hGaOut, fmt=1077, advance="NO" )g_arrPumpRates(j)
	enddo
	!well hydraulic head
	do j=1, g_nRemWells
		write( hGaOut, fmt=1085, advance="NO" )arrHeads(j), arrHeadStds(j)
	enddo
	!well pumping flags and installation flags
!	do j=2*g_nRemWells+1, sgaParam%nVarCount
!		write( hGaOut, fmt=1079, advance="NO" )anint(sgaParam%arrVars(id,j))
!	enddo
	do j=1, sgaParam%nChromBits
		write( hGaOut, fmt=1078, advance="NO" )sgaParam%arrPop(id,j)
	enddo
	!just print the catridge
	write(hGaOut, fmt=1081, advance="YES")
                                                                                                                                                                                                   
	1075 format('*',i4,2x,2(f17.6,2x),5(f20.10,2x))                                                                                                                                           
	1076 format(i4,2x, 2(f17.6, '(', f15.6, ')') ) 
	1077 format(2x, f9.4)

	1085 format(2x, 2(f9.4, '(', f9.4, ')') )
	1078 format(2x,i2)
	1079 format(2x,f2.0)
	1081 format(1x)
end subroutine SaveIndInfo                                                                                                                                                                        


end module