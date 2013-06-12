module meta_fit
use sga
use casemeta
use chmcache
use umacase
use costfun
use scaling

use metamodel
implicit none

!integer :: hChromCache

!two neural networks for prediction
integer :: hFourYCModel = 0
integer :: hFiveYCModel = 0
!integer :: nMaxNewPoints = 120

!integer, parameter :: g_nPoolSize	= 120
integer :: g_nPooledPdes

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


double precision function ComputeModelFitness( sgaParam, hNormMean, index, arrCachedFits )
implicit none
	!argments
	type(SGAPARAMSTRUCT), intent(inout) :: sgaParam
	integer, intent(in) :: hNormMean
	integer, intent(in):: index
	double precision, dimension(:), optional :: arrCachedFits

	double precision, dimension(3*g_nNewWells + g_nOldWells + 5) :: arrVars				!the vars for parsing and normalize the NN parameters.
	double precision :: rFitness, rTotalCost, rPenTnt, rPenRdx, rMaxTntC, rMaxRdxC, rDummyVCGCost, MB1, MB2, rViol
	integer :: iDryCellC, iDryCellH, nRemYear
	double precision :: rFourMaxRdxC, rFourMaxTntC
	double precision :: rViolRdx, rViolTnt, rViolWell
	integer :: nInputs, nOutputs

	nInputs = 3*g_nNewWells + g_nOldWells
	nOutputs = 5

	call DecodeOne( sgaParam, index )
	call PrepareFitness( index, sgaParam, MB1, MB2, rViolWell, arrVars )

	!do the prediction using neural network
	call Normalize( hNormMean, arrVars, 1 )
	!call neural network
	call predict_meta_model( hFourYCModel, arrVars, arrVars(nInputs+1:nInputs+3) )
	call predict_meta_model( hFiveYCModel, arrVars, arrVars(nInputs+4:nInputs+5) )
	!parse the data back to global variables.
	call UnNormalize( hNormMean, arrVars, 1 )

	rDummyVCGCost = 10**arrVars(nInputs+3)
	rFourMaxRdxC = 10**arrVars(nInputs+4)
	rFourMaxTntC = 10**arrVars(nInputs+5)
	rMaxRdxC = arrVars(nInputs+1)*rFourMaxRdxC
	rMaxTntC = arrVars(nInputs+2)*rFourMaxTntC

	!compute the mean cost and violations.
	call CostByNN( rMaxRdxC, rMaxTntC, rDummyVCGCost, rFourMaxRdxC, rFourMaxTntC, rFitness, rTotalCost, rPenRdx, rPenTnt, g_nGridFlagX, g_nGridFlagY, iDryCellC, iDryCellH )
	call ComputeRdxTntViols( rPenRdx, rPenTnt, rViolRdx, rViolTnt )
	sgaParam%arrCosts(index) = rTotalCost
	sgaParam%arrViols(index) = rViolRdx + rViolTnt + rViolWell

	!compute the cost and violations standard deviations.
	!violation caused by well pumping rates doesn't change over different realizaitons.
	!cost doesn't change over different realizations unless the remediation year changes. 
	!however, remediation year is almost always 5 under uncertainty.
	sgaParam%arrCostStds(index) = 0.d0
	sgaParam%arrViolStds(index) = 0.d0
	sgaParam%arrFitStds(index) = 0.d0

	rFitness = rFitness + MB1 + MB2
!	rFitness = log(rFitness + MB1 + MB2)

	!the cached array is (rTotalCost, rViols, rFitness, rMaxRdxC, rMaxTntC, rFourMaxRdxC, rFourMaxTntC, rVCGCost, nRemYear )
	if( present(arrCachedFits) )arrCachedFits = (/rTotalCost, sgaParam%arrViols(index), rFitness, rMaxRdxC, rMaxTntC, rFourMaxRdxC, rFourMaxTntC, rDummyVCGCost, 5.d0/)
!	if( present(arrCachedStds) )arrCachedStds = (/rDummyVCGStdCost, sgaParam%arrViolStds(index), -1.d0, rDummyVCGStdCost, rMaxRdxStdC, rMaxTntStdC, arrVars(nInputs+1), arrVars(nInputs+2), rDummyVCGStdCost, 0.d0/)

	ComputeModelFitness = rFitness
end function

subroutine EvalByAnns( sgaParam, hNormMean )
implicit none
	!argument                                                                                                                                                                                         
	type(SGAPARAMSTRUCT), intent(inout) :: sgaParam
	integer, intent(in) :: hNormMean
	integer :: i

	do i=1, sgaParam%nPopSize
		sgaParam%arrFit(i) = ComputeModelFitness( sgaParam, hNormMean, i )
		sgaParam%arrFlags(i) = NN_EVAL		!0 means a NN results.
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
			sgaParam%arrCostsInj(i) = arrCachedFits(1)
			sgaParam%arrCostStdsInj(i) = arrCachedFitStds(1)
			sgaParam%arrViolsInj(i) = arrCachedFits(2)
			sgaParam%arrViolStdsInj(i) = arrCachedFits(2)
		endif
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

	!the cached array is (rTotalCost, rViols, rFitness, rMaxRdxC, rMaxTntC, rFourMaxRdxC, rFourMaxTntC, rVCGCost, nRemYear )
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

subroutine EvalByPdes( sgaParam, arrPdeIds, nPdeCount, nCachedFits )
implicit none
	!argument                                                                                                                                                                                         
	type(SGAPARAMSTRUCT), intent(inout) :: sgaParam
	integer, dimension(:) :: arrPdeIds
	integer, intent(in) :: nPdeCount, nCachedFits

	!variables
	integer :: i, k, id, nSamples, nJobCount
	integer, dimension(:), allocatable :: arrJobIds, arrJobIds2
	double precision, dimension(nCachedFits) :: arrCachedFits

	double precision :: rFitness, rTotalCost, rViolTnt, rViolRdx, rViolWell, rMaxTntC, rMaxRdxC, rDummyVCGCost, MB1, MB2
	double precision :: rCost, rViols
	integer :: iDryCellC, iDryCellH, nRemYear
	double precision :: rFourMaxRdxC, rFourMaxTntC

	!broadcast the jobs to the distributed environment
	nSamples = ubound(sgaParam%arrRealizations,2) - lbound(sgaParam%arrRealizations,2) + 1
	nJobCount = 0
	allocate( arrJobIds(sgaParam%nPopSize*nSamples), arrJobIds2(sgaParam%nPopSize*nSamples) )

	do i = 1, nPdeCount
		id = arrPdeIds(i)
		call DecodeOne( sgaParam, id )
		call PrepareFitness( id, sgaParam, MB1, MB2, rViolWell )

		do k=1, nSamples
			if( SearchChromCache( hChromCache, sgaParam%arrPop(id,:), sgaParam%nChromBits, sgaParam%arrRealizations(id,k), arrCachedFits, nCachedFits )==0 )then
				arrJobIds( nJobCount+1 ) = id*1000+k
				arrJobIds2( nJobCount+1 ) = id*1000 + sgaParam%arrRealizations(id,k)
				call BroadcastSlave( arrJobIds2(nJobCount+1), sgaParam%iCurGen, g_nGridFlagX, g_nGridFlagY )
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
		call PrepareFitness( id, sgaParam, MB1, MB2, rViolWell )

		call CollectSlaveEx( arrJobIds2(i), sgaParam%iCurGen, rFitness, rTotalCost, rViolRdx, rViolTnt, rMaxRdxC, rMaxTntC, rDummyVCGCost, nRemYear, rFourMaxRdxC, rFourMaxTntC, iDryCellC, iDryCellH )

		rFitness = rFitness + MB1 + MB2
!		rFitness = log(rFitness + MB1 + MB2)

		if( rMaxRdxC>900 .or. rMaxTntC>900 )then
			!the solver didn't converge, skip this solution.
			call RemoveChromCache( hChromCache, sgaParam%arrPop(id,:), sgaParam%nChromBits, sgaParam%arrRealizations(id,k) )
		else
			!save the  data into the cache
			rViols = rViolWell + rViolRdx + rViolTnt

			!the cached array is (rTotalCost, rViols, rFitness, rMaxRdxC, rMaxTntC, rFourMaxRdxC, rFourMaxTntC, rVCGCost, nRemYear )
			arrCachedFits = (/rTotalCost, rViols, rFitness, rMaxRdxC, rMaxTntC, rFourMaxRdxC, rFourMaxTntC, rDummyVCGCost, dble(nRemYear)/)

			call ReplaceChromCache( hChromCache, sgaParam%arrPop(id,:), sgaParam%nChromBits, sgaParam%arrRealizations(id,k), arrCachedFits, nCachedFits )
		endif
	enddo
	deallocate( arrJobIds, arrJobIds2 )

	!let the cache do the dirty job for me, evaluating the pde results.
	call EvalByCache( sgaParam, arrPdeIds, nPdeCount, nCachedFits, PDE_EVAL )

end subroutine

subroutine UpdateSSD( sgaParam, hNormParam, rStdBase, rSmoothStd, rSmoothStdDev )
implicit none
	type(SGAPARAMSTRUCT), intent(inout) :: sgaParam
	integer, intent(in) :: hNormParam
	double precision, intent(inout) :: rStdBase, rSmoothStd, rSmoothStdDev

	integer :: i, nInputs
	double precision, dimension(sgaParam%nPopSize, 3*g_nNewWells + g_nOldWells + 5) :: arrVars			!the vars for parsing and normalize the NN parameters.
	double precision :: MB1, MB2, rWellViol
	double precision, dimension(3*g_nNewWells + g_nOldWells) :: arrMeans, arrStds
	double precision :: rAvgStd, rDiff

	nInputs = 3*g_nNewWells + g_nOldWells

	do i=1, sgaParam%nPopSize
		call DecodeOne( sgaParam, i )
		call PrepareFitness( i, sgaParam, MB1, MB2, rWellViol, arrVars(i,:) )

		call Normalize( hNormParam, arrVars(i,:), 1 )
	enddo

	call CalcMeanStd( arrVars(:,1:3*g_nNewWells + g_nOldWells), arrMeans, arrStds )
!	arrMeanPt = arrMeans

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

!instead of adding the training/testing records one by one, 
!the UpdateAnnsEx function add the records as a batch, these records can be retrieved from the Cache.
subroutine UpdateAnnsEx( sgaParam, hMeanNorm, nCachedFits )
use metamodel
use neural_model
use casemeta
implicit none
	!arguments
	type(SGAPARAMSTRUCT), intent(inout) :: sgaParam
	integer, intent(in) :: hMeanNorm
	integer :: nCachedFits

	!variables
	integer :: i, start_gen

	start_gen = 2

	do i=1, sgaParam%nPopSize
		if( sgaParam%arrFlags(i)/=PDE_EVAL )cycle
		!still update the # of pool pdes, as it trigers the ANN retraining.
		g_nPooledPdes = g_nPooledPdes + 1
	enddo

!	if( .true. )then
	if( (g_nPooledPdes>sgaParam%nRetrainingPoolSize) .and. (sgaParam%iCurGen>=start_gen) .and. (sgaParam%iCurGen/=sgaParam%nMaxGens) )then
!	if( .false. )then

		call CleanTrainingFiles
		call ForEachChromCache( hChromCache, SaveChroms )

		if( hFourYCModel==0 )then
			hFourYCModel = create_model()
			hFiveYCModel = create_model()
		else
			sgaParam%bPendingModels = .true.
		endif

		if( sgaParam%iCurGen<5 )then
			arrFourYCNet(2) = 8
			arrFiveYCNet(2) = 8
		else if( sgaParam%iCurGen<25 ) then
			arrFourYCNet(2)=12
			arrFiveYCNet(2) = 12
		else 
			arrFourYCNet(2) = 12
			arrFiveYCNet(2) = 12
		endif


		!initialize the neural networks.
		call initialize( hFourYCModel, arrFourYCNet, 3, (/0.d0,0.d0,0.d0,0.0d0/) )
		call initialize( hFiveYCModel,  arrFiveYCNet, 3, (/dble( (log10(2.1*1.05)+0.3)/1.7*1.8-0.9 ), dble( (log10(2.8*1.05)+0.3)/2.2*1.8-0.9 ), 1.2d0/) )

		!train the neural networks.
		call train_meta_model( hFourYCModel, strFourYCTrain, "umavalid" )
		call train_meta_model( hFiveYCModel, strFiveYCTrain, "umavalid" )

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
		integer :: i, index, nInputs
		integer, dimension(sgaParam%nChromBits) :: arrTempBits

		double precision :: MB1, MB2, rWellViol
		double precision, dimension(3*g_nNewWells + g_nOldWells + 5) :: arrVars		!the vars for parsing and normalize the NN parameters.

		double precision, dimension(nCachedFits) :: arrCachedFits, arrCachedFitStds
		double precision :: rMaxRdxC, rMaxTntC, rFourMaxRdxC, rFourMaxTntC, rDummyVCGCost

		!save the first individual
		arrTempBits = sgaParam%arrPop(1,:)
		!repleace the first individual with the passed chromosome
		do i=1, sgaParam%nChromBits
			sgaParam%arrPop(1,i) = arrChroms(i)
		enddo

		call DecodeOne( sgaParam, 1 )
		call PrepareFitness( 1, sgaParam, MB1, MB2, rWellViol, arrVars )

		!retrieve the neural network outputs
		call GetStatChromCache(hChromCache, sgaParam%arrPop(1,:), sgaParam%nChromBits, arrCachedFits, arrCachedFitStds )

		index = 3
		nInputs = 3*g_nNewWells + g_nOldWells

		!the cached array is (rTotalCost, rViols, rFitness, rMaxRdxC, rMaxTntC, rFourMaxRdxC, rFourMaxTntC, rVCGCost, nRemYear )

		!parse the variables.
		rMaxRdxC = arrCachedFits( index+1 )
		rMaxTntC = arrCachedFits( index+2 )
		rFourMaxRdxC = arrCachedFits( index+3 )
		rFourMaxTntC = arrCachedFits( index+4 )
		rDummyVCGCost = arrCachedFits( index+5 )

		!the values are out of range, don't put them into the training or testing files
		if( (rFourMaxTntC>100) .or. (rMaxTntC >100) )return

		!parse the variables into arrVars 
		rMaxRdxC = max( 0.1, min( rMaxRdxC, 25.d0 ) )
		rMaxTntC = max( 0.1, min( rMaxTntC, 75.d0 ) )
		rDummyVCGCost = max( 0.1, min( 1000.d0, rDummyVCGCost ) )
		rFourMaxRdxC = max(0.1, min(rFourMaxRdxC, 25.d0) )
		rFourMaxTntC = max(0.1, min(rFourMaxTntC, 75.d0) )

		!put the ANN training outputs into arrVars and normalize the input-outputs.
		arrVars(nInputs+1) = rMaxRdxC / rFourMaxRdxC
		arrVars(nInputs+2) = rMaxTntC / rFourMaxTntC
		arrVars(nInputs+3) = log10(rDummyVCGCost)
		arrVars(nInputs+4) = log10(rFourMaxRdxC)
		arrVars(nInputs+5) = log10(rFourMaxTntC)

		call Normalize( hMeanNorm, arrVars, 1 )

		!writting it to the training file or testing file
		call AppendToFile( strFourYCTrain, arrVars, nInputs+3 )
		call AppendToFile( strFiveYCTrain, (/arrVars(1:nInputs), arrVars(nInputs+4), arrVars(nInputs+5)/), nInputs+2 )

		!save the first individual
		sgaParam%arrPop(1,:) = arrTempBits
	end subroutine

end subroutine


subroutine SaveGenInfo( sgaParam, hGaOut, hNormMean, nCachedFits )
implicit none
	type(SGAPARAMSTRUCT), intent(inout) :: sgaParam
	integer, intent(in) :: hNormMean
	integer, intent(in) :: hGaOut, nCachedFits

	!variables
	integer :: i, id, index, samples
	double precision, dimension(2) :: arFit, arCost, arViol, arRdx5, arTnt5, arVCG, arCoeff
	double precision, dimension(nCachedFits) :: arrCachedFits, arrCachedFitStds
	double precision :: MB1, MB2, rWellViol
	integer, dimension(sgaParam%nPopSize) :: arrIds

	arrIds = (/ (i, i=1,sgaParam%nPopSize) /)
	call qsort( arrIds, sgaParam%nPopSize, sizeof(arrIds(1)), CompareByFitness )

	index = 3

	open(unit=hGaOut, file=strGaOut, status='unknown', position='append' )
	write( hGaOut, 1000 ) sgaParam%iCurGen

	do i=1, sgaParam%nPopSize
		id = arrIds(i)
		call DecodeOne( sgaParam, id )
		call PrepareFitness( id, sgaParam, MB1, MB2, rWellViol )

		arFit = (/ sgaParam%arrFit(id), sgaParam%arrFitStds(id) /)
		arCost = (/ sgaParam%arrCosts(id), sgaParam%arrCostStds(id) /)
		arViol = (/ sgaParam%arrViols(id), sgaParam%arrViolStds(id) /)
		arRdx5 = 0
		arTnt5 = 0
		arVCG = 0
		samples = 0
		!the cached array is (rTotalCost, rViols, rFitness, rMaxRdxC, rMaxTntC, rFourMaxRdxC, rFourMaxTntC, rVCGCost, nRemYear )
		if( SearchChromCache(hChromCache, sgaParam%arrPop(id,:), sgaParam%nChromBits, -1, arrCachedFits, nCachedFits)/=0 )then
			call GetStatChromCache(hChromCache, sgaParam%arrPop(id,:), sgaParam%nChromBits, arrCachedFits, arrCachedFitStds )
			samples = GetSampleCountCache( hChromCache, sgaParam%arrPop(id,:), sgaParam%nChromBits )
			arFit = (/ arrCachedFits(3), arrCachedFitStds(3) /)
			arRdx5 = (/ arrCachedFits(index+1), arrCachedFitStds(index+1) /)
			arTnt5 = (/ arrCachedFits(index+2), arrCachedFitStds(index+2) /)
			arVCG = (/ arrCachedFits(index+5), arrCachedFitStds(index+5) /)
			arCoeff = (/ arrCachedFits(index+1)/arrCachedFits(index+3), arrCachedFits(index+2)/arrCachedFits(index+4) /)
		else
			arFit(1) = ComputeModelFitness( sgaParam, hNormMean, id, arrCachedFits )
			arRdx5 = (/ arrCachedFits(index+1), 0.d0 /)
			arTnt5 = (/ arrCachedFits(index+2), 0.d0 /)
			arVCG = (/ arrCachedFits(index+5), 0.d0 /)
			arCoeff = (/ arrCachedFits(index+1)/arrCachedFits(index+3), arrCachedFits(index+2)/arrCachedFits(index+4) /)
		endif

		call SaveIndInfo( id, sgaParam%arrFlags(id), samples, arFit, arCost, arViol, arRdx5, arTnt5, arVCG, arCoeff)

!		arCost(1) = ComputeCost(arrHeads, g_arrOldHeads, g_arrPumpRates, g_nRemWells)
!		arCost(2) = ComputeCostStd( arrHeads, arrHeadStds, g_arrOldHeads, g_arrPumpRates, g_nRemWells )
!		arViol(1) = ComputeViolation(arrHeads, g_arrOldHeads, g_arrPumpRates, g_nRemWells, arRiskB(1), arRiskW(1) )
!		arViol(2) = ComputeViolationStd( arrHeads, arrHeadStds, g_arrOldHeads, g_arrPumpRates, g_nRemWells, arRiskB(1), arRiskW(1), arRiskB(2), arRiskW(2) )

!		call SaveIndInfo( i, sgaParam%arrFlags(i), arFit, arCost, arViol, arTotCost, arRiskB, arRiskW, &
!			& g_arrWellLocs, g_arrPumpRates, arrHeads, arrHeadStds, sgaParam%arrPop(i,:) )

	enddo
	close( hGaOut )

	open(unit=hPopStat, file=strPopStat, status='unknown', position='append' )
	call SavePopStat( hPopStat, sgaParam )
	close( hPopStat )


	1000 format( 3('#'), 'Generation ', i4, 2x, 3('#'), 'fitness cost violations, totcost, rdx5, tnt5, VCG, loc[], Q[], H[]', 10('#') )

contains

	subroutine SaveIndInfo( id, nFlag, samples, arFit, arCost, arViol, arRdx5, arTnt5, arVCG, arCoeff )
	implicit none

	!arguments
	integer, intent(in) :: id, nFlag, samples
	double precision, dimension(2) :: arFit, arCost, arViol, arRdx5, arTnt5, arVCG, arCoeff

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

	write(hGaOut,fmt=1100,advance="NO")arFit, arCost, arViol, arRdx5, arTnt5, arVCG, arCoeff

	!well locations
	do j=1, g_nNewWells
		write( hGaOut, fmt=1079, advance="NO" )nint(sgaParam%arrVars(id, j))
	enddo
	!well pumping rates
	do j=j, g_nNewWells+g_nNewExtWells
		write( hGaOut, fmt=1077, advance="NO" )sgaParam%arrVars(id, j)
	enddo
	!new well flags
	do j=j, 2*g_nNewWells+g_nNewExtWells
		write( hGaOut, fmt=1078, advance="NO" )sgaParam%arrVars(id, j)
	enddo

	!old well pumping rates
	do j=j, 2*g_nNewWells+g_nNewExtWells+g_nOldExtWells
		write( hGaOut, fmt=1077, advance="NO" )sgaParam%arrVars(id, j)
	enddo
	!old well pumping flags
	do j=j, 2*g_nNewWells+g_nNewExtWells+g_nOldExtWells+g_nOldWells
		write( hGaOut, fmt=1078, advance="NO" )sgaParam%arrVars(id, j)
	enddo

	do j=1, sgaParam%nChromBits
		write( hGaOut, fmt=1079, advance="NO" )sgaParam%arrPop(id, j)
	enddo

	!just print the catridge
	write(hGaOut, fmt=1600, advance="YES")

	1001 format( i4, '(A-', i2, ')' )
	1002 format( i4, '(C-', i2, ')' )
	1003 format( i4, '(P-', i2, ')' )

	1100 format( 2x, (f12.4, '(', f12.4, ')'), 2(f12.4, '(', f10.6, ')'), 4(f12.8, '(', f12.8, ')') )
	1200 format( 2x, i2)
	1300 format(2x, f9.4)
	1400 format(2x, 2(f9.4, '(', f9.4, ')') )
 	1500 format(1x,i1)

	1077 format(2x,f7.4)
	1078 format(2x,f2.0)
	1079 format(1x,i1)

	1600 format(1x)
	end subroutine

end subroutine


subroutine SaveCountInfo( sgaParam, nPdeIds )
implicit none
	type(SGAPARAMSTRUCT), intent(inout) :: sgaParam
	integer, intent(in) :: nPdeIds

	!variables
	integer, save :: nTotalPdeIds
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

	if( sgaParam%iCurGen==1 )then
		write(hGaCount, 1000)
	endif

	open(unit=hGaCount, file=strGaCount, status='unknown', position='append')
	write( hGaCount, 1001 )sgaParam%iCurGen, nTotalPdeIds, nPdeCount, nCacheCount, nAnnCount
	close( hGaCount )

	1000 format('GEN', 2x, 'TOTPDE', 2x, 'PDE#', 2x, 'CACHE#', 2x, 'ANN#' )
	1001 format( i3, 2x, 4(i5, 2x) )

end subroutine
		

!the logic of FitEvalNet is changed on 10/08/03 
! first the subroutine evaluate all fitnesses by Neural network
! then the subroutine randomly pick individuals to evaluate them by PDE.
! All the PDE results are saved into the cache.
! The statiscal file is ga.stat.
                                                                                                                                                                                                          
!using matlab to create neural network                                                                                                                                                                    
subroutine FitEval(sgaParam)                                                                                                                                                                       
use fmt_helper
use sampling
use condorio
use STD_HELPER
USE DFPORT

implicit none                                                                                                                                                                                             
	!argument                                                                                                                                                                                         
	type(SGAPARAMSTRUCT), intent(inout) :: sgaParam

	!variable
	integer :: i, j, k, id
	logical :: bAnnReady

	!variables for adaptive sampling control
	double precision, save :: rStdBase, rSmoothStd, rSmoothStdDev

	!variables for sorting
	integer, dimension( sgaParam%nPopSize ) :: arrIndex
	
	!variables for pde evaluation	
	integer :: arrPdeIds(sgaParam%nPopSize)
	integer :: nExpectedPdes, nPdeIds

	!variables for caching
	integer :: nCachedFits

	!variables for normalizing
	integer :: hMeanNorm			!the normalize parameters


	bAnnReady = .false.

	if( sgaParam%iCurGen==1 )then
		!prepare ncp static folder
		call MakeStaticFolder
		!prepare output files
		call PrepareOutputFiles
		!no pde is in pool yet.
		g_nPooledPdes = 0

		rSmoothStdDev = 0
	endif

	!the cached array is (rTotalCost, rViols, rFitness, rMaxRdxC, rMaxTntC, rFourMaxRdxC, rFourMaxTntC, rVCGCost, nRemYear )
	nCachedFits = 9		!(cost, violations, fitnes, rdx(5), tnt(5), rdx(4), tnt(4), VCG, nRemYear )

	!choose the realizations using latin tube
	call ChooseRealizations( sgaParam, sgaParam%arrRealizations )

	call CreateScalings( hMeanNorm )

	!prepare neural networks.
	call assert( ((hFourYCModel==0).and.(hFiveYCModel==0)).or.((hFourYCModel/=0).and.(hFiveYCModel/=0)) )

	bAnnReady = hFourYCModel/=0;
	if( sgaParam%iCurGen==sgaParam%iStartGen+sgaParam%nMaxGens-1 )bAnnReady = .false.

	!update the fitness only
	if( sgaParam%bUpdateFits )then
		call assert( bAnnReady )
		do i=1, sgaParam%nPopSize
			if( sgaParam%arrFlags(i)==NN_EVAL )then
				sgaParam%arrFit(i) = ComputeModelFitness( sgaParam, hMeanNorm, i )
			endif
		enddo
		sgaParam%bPendingModels = .false.

		call ReleaseNormalize( hMeanNorm )
		return
	endif

	arrIndex = (/(i, i=1,sgaParam%nPopSize)/)

	!######################### Pass 1 - evaluate the fitness by the ANNs #########################
	sgaParam%arrFlags = -1
	if( bAnnReady )then
		call EvalByAnns( sgaParam, hMeanNorm )
	endif


	!######################### Pass 2 - evaluate the fitness by the CACHE ###########################3
	call EvalByCache( sgaParam, arrIndex, sgaParam%nPopSize, nCachedFits, CACHE_EVAL )

	!######################### Pass 3 - evaluate the fitness by the PDEs ###########################3
	if( bAnnReady )then
		!compute the expected sampling number
		nExpectedPdes = sgaParam%nInitSampling * (rSmoothStd-rSmoothStdDev)/(rStdBase - rSmoothStdDev)
		
		!sample by best sampling strategy or tournament sampling strategy
		if( sgaParam%iCurGen>35 )then
!			call qsort( arrIndex, sgaParam%nPopSize, sizeof(arrIndex(1)), FitCompare )
			call qsort( arrIndex, sgaParam%nPopSize, sizeof(arrIndex(1)), CompareByCostViolTest )
			call BestSelPdes( sgaParam, arrIndex, nExpectedPdes, arrPdeIds, nPdeIds)
		else
			call TournSelPdes( sgaParam, 8, nExpectedPdes, arrPdeIds, nPdeIds )
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
	call EvalByPdes( sgaParam, arrPdeIds, nPdeIds, nCachedFits )

	call UpdateInjPool( sgaParam, nCachedFits )

	call SaveGenInfo( sgaParam, hGaOut, hMeanNorm, nCachedFits )
	call SaveCountInfo( sgaParam, nPdeIds )

	!######################### Update SSD and ANNs ###########################
	call UpdateSSD( sgaParam, hMeanNorm, rStdBase, rSmoothStd, rSmoothStdDev )

	!update the ANNs
	call UpdateAnnsEx( sgaParam, hMeanNorm, nCachedFits )

	call ReleaseScalings( hMeanNorm )

	!reload the host file                                                                                                                                                                                                       
	call RefreshHosts
contains
	subroutine SaveTrainInfo( rUmaMSE, rCostMSE )
	implicit none
		double precision :: rUmaMSE, rCostMSE
                                                                                                                                                                                                          
		write( hGaOut, * )'Neural network is trained, information'                                                                                                                                                
		write( hGaOut, 1001 ) rUmaMSE, rCostMSE

		1001 format(2(f10.8))
	end subroutine                                                                                                                                                                                    
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

end module