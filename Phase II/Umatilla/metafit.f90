module meta_fit
use sga
use casemeta
use chmcache
use umacase
use costfun
use scaling

use metamodel
implicit none

integer :: hChromCache

!two neural networks for prediction
integer :: hUmaModel = 0
integer :: hCostModel = 0
!integer :: nMaxNewPoints = 120

integer :: g_nPooledPdes

double precision, dimension( 29 ) :: arrBestPt
double precision, dimension(28) :: arrMeanPt
!double precision, dimension(3*g_nNewWells+g_nOldWells) :: arrMeanPt

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
			
!		if( CompareByCost( arrIndex(iMate), arrIndex(iPick) )<=0 )then
		if( arrFits(arrIndex(iMate))<arrFits(arrIndex(iPick)) )then
			iPick = iMate
		endif
	enddo
	TournSel = iPick
end function TournSel

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
!	call UniqueIds( sgaParam, arrIndex, nSelCount )

	nPdeCount = min( nExpectPdes, nSelCount )
	!do tournment selection without replacement.
	do i=1, nPdeCount
!		iPick = TournSel_Prob( sgaParam%arrFit, arrIndex, nSelCount, nTournSize, sgaParam%rModelErrorMean, sgaParam%rModelErrorStd )
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

double precision function ComputeModelFitness( sgaParam, hNormParam, index, arrCachedFits )
implicit none
	!argments
	type(SGAPARAMSTRUCT), intent(inout) :: sgaParam
	integer, intent(in) :: hNormParam
	integer, intent(in):: index
	double precision, dimension(:), optional :: arrCachedFits

	double precision, dimension(3*g_nNewWells + g_nOldWells + 5) :: arrVars				!the vars for parsing and normalize the NN parameters.
	double precision :: rFitness, rTotalCost, rPenTnt, rPenRdx, rMaxTntC, rMaxRdxC, rDummyVCGCost, MB1, MB2
	integer :: iDryCellC, iDryCellH, nRemYear
	double precision :: rFourMaxRdxC, rFourMaxTntC
	integer :: nInputs, nOutputs

	nInputs = 3*g_nNewWells + g_nOldWells
	nOutputs = 5

	call DecodeOne( sgaParam, index )
	call PrepareFitness( index, sgaParam, MB1, MB2, arrVars )

	!do the prediction using neural network
	call Normalize( hNormParam, arrVars, 1 )
	!call neural network
	call predict_meta_model( hUmaModel, arrVars, arrVars(nInputs+1:nInputs+3) )
	call predict_meta_model( hCostModel, arrVars, arrVars(nInputs+4:nInputs+5) )
	!parse the data back to global variables.
	call UnNormalize( hNormParam, arrVars, 1 )

	rDummyVCGCost = 10**arrVars(nInputs+3)
	rFourMaxRdxC = 10**arrVars(nInputs+4)
	rFourMaxTntC = 10**arrVars(nInputs+5)
	rMaxRdxC = arrVars(nInputs+1)*rFourMaxRdxC
	rMaxTntC = arrVars(nInputs+2)*rFourMaxTntC

	call CostByNN( rMaxRdxC, rMaxTntC, rDummyVCGCost, rFourMaxRdxC, rFourMaxTntC, rFitness, rTotalCost, rPenRdx, rPenTnt, g_nGridFlagX, g_nGridFlagY, iDryCellC, iDryCellH )

	if( present(arrCachedFits) )arrCachedFits = (/rFitness, rTotalCost, rPenRdx, rPenTnt, rMaxRdxC, rMaxTntC, rDummyVCGCost, rFourMaxRdxC, rFourMaxTntC/)

	ComputeModelFitness = rFitness + MB1 + MB2
end function

subroutine EvalByAnns( sgaParam, hNormParam )
implicit none
	!argument                                                                                                                                                                                         
	type(SGAPARAMSTRUCT), intent(inout) :: sgaParam
	integer, intent(in) :: hNormParam

	integer :: i

	do i=1, sgaParam%nPopSize
		sgaParam%arrFit(i) = ComputeModelFitness( sgaParam, hNormParam, i )
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
	double precision, dimension(nCachedFits) :: arrCachedFits
	integer :: i, id

	double precision :: rFitness, MB1, MB2
	double precision, dimension(3*g_nNewWells + g_nOldWells + 5) :: arrVars		!the vars for parsing and normalize the NN parameters.

	do i = 1, nIds
		id = arrIds(i)
		call DecodeOne( sgaParam, id )
		call PrepareFitness( id, sgaParam, MB1, MB2, arrVars )

		if( SearchChromCache(hChromCache, sgaParam%arrPop(id,:), sgaParam%nChromBits, arrCachedFits, nCachedFits)/=0 )then
			!get the cached fitness from the cache
			rFitness = arrCachedFits(1)

			sgaParam%arrFit(id) = rFitness + MB1 + MB2		!catched PDE fitness doesn't include VCF cost
			sgaParam%arrFlags(id) = nFlag
		endif
	enddo
end subroutine

subroutine EvalByPdes( sgaParam, arrJobIds, nJobCount, nCachedFits )
implicit none
	!argument                                                                                                                                                                                         
	type(SGAPARAMSTRUCT), intent(inout) :: sgaParam
	integer, dimension(:) :: arrJobIds
	integer, intent(in) :: nJobCount, nCachedFits

	!variables
	integer :: i, id
	double precision :: rFitness, rTotalCost, rPenTnt, rPenRdx, rMaxTntC, rMaxRdxC, rDummyVCGCost, MB1, MB2
	integer :: iDryCellC, iDryCellH, nRemYear
	double precision :: rFourMaxRdxC, rFourMaxTntC
	double precision, dimension(3*g_nNewWells + g_nOldWells + 5) :: arrVars		!the vars for parsing and normalize the NN parameters.
	double precision, dimension(nCachedFits) :: arrCachedFits


	!broadcast the jobs
	do i=1, nJobCount
		id = arrJobIds(i)
		call DecodeOne( sgaParam, id )
		call PrepareFitness( id, sgaParam, MB1, MB2, arrVars )
		call BroadcastSlave( id, sgaParam%iCurGen, g_nGridFlagX, g_nGridFlagY )
	enddo
		
	!route the jobs
	if( nJobCount>0 )then 
		call WaitForSlaves( arrJobIds, nJobCount )
	endif

	!collect jobs
	do i=1, nJobCount
		id = arrJobIds(i)
		call DecodeOne( sgaParam, id )
		call PrepareFitness( id, sgaParam, MB1, MB2, arrVars )
		call CollectSlave( id, sgaParam%iCurGen, rFitness, rTotalCost, rPenRdx, rPenTnt, rMaxRdxC, rMaxTntC, rDummyVCGCost, nRemYear, rFourMaxRdxC, rFourMaxTntC, iDryCellC, iDryCellH )

		!save data for analysis and ouputs.
		arrCachedFits = (/rFitness, rTotalCost, rPenRdx, rPenTnt, rMaxRdxC, rMaxTntC, rDummyVCGCost, rFourMaxRdxC, rFourMaxTntC/)
		call InsertChromCache( hChromCache, sgaParam%arrPop(id,:), sgaParam%nChromBits, arrCachedFits, nCachedFits )

		!save result to sgaParam
		sgaParam%arrFlags(id) = PDE_EVAL
		sgaParam%arrFit( id ) = rFitness + MB1 + MB2
	enddo
end subroutine

subroutine UpdateSSD( sgaParam, hNormParam, rStdBase, rSmoothStd, rSmoothStdDev )
implicit none
	type(SGAPARAMSTRUCT), intent(inout) :: sgaParam
	integer, intent(in) :: hNormParam
	double precision, intent(inout) :: rStdBase, rSmoothStd, rSmoothStdDev

	integer :: i, nInputs
	double precision, dimension(sgaParam%nPopSize, 3*g_nNewWells + g_nOldWells + 5) :: arrVars			!the vars for parsing and normalize the NN parameters.
	double precision :: MB1, MB2
	double precision, dimension(3*g_nNewWells + g_nOldWells) :: arrMeans, arrStds
	double precision :: rAvgStd, rDiff

	nInputs = 3*g_nNewWells + g_nOldWells

	do i=1, sgaParam%nPopSize
		call DecodeOne( sgaParam, i )
		call PrepareFitness( i, sgaParam, MB1, MB2, arrVars(i,:) )

		call Normalize( hNormParam, arrVars(i,:), 1 )
	enddo

	call CalcMeanStd( arrVars(:,1:3*g_nNewWells + g_nOldWells), arrMeans, arrStds )
	arrMeanPt = arrMeans

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

subroutine UpdateAnns( sgaParam, hNormParam, bAnnReady, nCachedFits )
use regional_svm_model
!use regional_neural_model
!use neural_model
!use svm_model

implicit none
	!arguments
	type(SGAPARAMSTRUCT), intent(inout) :: sgaParam
	integer, intent(in) :: hNormParam, nCachedFits
	logical, intent(in) :: bAnnReady

	!variables
	integer :: i, nInputs, pos, start_gen
	double precision :: rFitness, rMaxTntC, rMaxRdxC, rDummyVCGCost, MB1, MB2
	double precision :: rFourMaxRdxC, rFourMaxTntC
	double precision, dimension(3*g_nNewWells + g_nOldWells + 5) :: arrVars		!the vars for parsing and normalize the NN parameters.
	double precision, dimension(nCachedFits) :: arrCachedFits

	double precision, dimension( 500, 1 ), save :: arrFitErrors
	integer, dimension(500), save :: arrChromPos
	integer, save :: nFitErrors, nPosCount

	nInputs = 3*g_nNewWells + g_nOldWells

	do i=1, sgaParam%nPopSize
		if( sgaParam%arrFlags(i)/=PDE_EVAL )cycle

		call DecodeOne( sgaParam, i )
		call PrepareFitness( i, sgaParam, MB1, MB2, arrVars )

		pos = SearchChromCache(hChromCache, sgaParam%arrPop(i,:), sgaParam%nChromBits, arrCachedFits, nCachedFits)

		rFitness = arrCachedFits( 1 )
		rMaxRdxC = arrCachedFits( 5 )
		rMaxTntC = arrCachedFits( 6 )
		rDummyVCGCost = arrCachedFits( 7 )
		rFourMaxRdxC = arrCachedFits( 8 )
		rFourMaxTntC = arrCachedFits( 9 )

		!the values are out of range, don't put them into the training or testing files
		if( (rFourMaxTntC>100) .or. (rMaxTntC >100) )cycle

		!increase the pooled pdes
		g_nPooledPdes = g_nPooledPdes + 1

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

		call Normalize( hNormParam, arrVars, 1 )

		!writting it to the training file or testing file
		call AppendToFile( strUmaTrain, arrVars, nInputs+3 )
		call AppendToFile( strCostTrain, (/arrVars(1:nInputs), arrVars(nInputs+4), arrVars(nInputs+5)/), nInputs+2 )

		open( unit=hTrainFit, file=strTrainFit, status='unknown', position='append' )
		write( hTrainFit, *)rFitness + MB1 + MB2
		close(hTrainFit)

		if( sgaParam%arrFit(i)<arrBestPt(nInputs+1) )then
			arrBestPt(nInputs+1) = sgaParam%arrFit(i)
			arrBestPt(1:nInputs) = arrVars(1:nInputs)
		endif
	end do

	start_gen = 3
!	rNetBias = min( 1.5, dble(nDoPde)/dble(sgaParam%nInitSampling) * 1.5 )

	!retrain the neural network                                                                                                                                                                       
	if( (g_nPooledPdes>=sgaParam%nRetrainingPoolSize) .and. (sgaParam%iCurGen>=start_gen) .and. sgaParam%iCurGen<sgaParam%nMaxGens )then
		if( hUmaModel==0 )then
			hUmaModel = create_model()
			hCostModel = create_model()
		else
			sgaParam%bPendingModels = .true.
		endif

		if( sgaParam%iCurGen<5 )then
			arrUmaNet(2) = 8
			arrCostNet(2) = 8
		else if( sgaParam%iCurGen<25 ) then
			arrUmaNet(2)=12
			arrCostNet(2) = 12
		else 
			arrUmaNet(2) = 12
			arrCostNet(2) = 12
		endif

!for single ANN model
!		call initialize( hUmaModel, arrUmaNet, 3, (/0.d0,0.d0,0.d0,0.0d0/) )
!		call initialize( hCostModel,  arrCostNet, 3, (/dble( (log10(2.1*1.05)+0.3)/1.7*1.8-0.9 ), dble( (log10(2.8*1.05)+0.3)/2.2*1.8-0.9 ), 1.2d0/) )

!for regional ANN model
!		if( (sgaParam%iCurGen>=sgaParam%arrBestClusterRange(1)) .and. (sgaParam%iCurGen<sgaParam%arrBestClusterRange(2)) )then
!			call initialize( hUmaModel, arrBestPt, sgaParam%nMinTrainPts, arrUmaNet, 3, (/0.d0,0.d0,0.d0,0.0d0/) )
!			call initialize( hCostModel, arrBestPt, sgaParam%nMinTrainPts, arrCostNet, 3, (/dble( (log10(2.1*1.05)+0.3)/1.7*1.8-0.9 ), dble( (log10(2.8*1.05)+0.3)/2.2*1.8-0.9 ), 1.2d0/) )
!		else
!			call initialize( hUmaModel, arrMeanPt, sgaParam%nMinTrainPts, arrUmaNet, 3, (/0.d0,0.d0,0.d0,0.0d0/) )
!			call initialize( hCostModel, arrMeanPt, sgaParam%nMinTrainPts, arrCostNet, 3, (/dble( (log10(2.1*1.05)+0.3)/1.7*1.8-0.9 ), dble( (log10(2.8*1.05)+0.3)/2.2*1.8-0.9 ), 1.2d0/) )
!		endif

!for single SVM model
!		call initialize( hUmaModel, 28, 3 )
!		call initialize( hCostModel, 28, 2 )

!for region SVM models
		if( (sgaParam%iCurGen>=sgaParam%arrBestClusterRange(1)) .and. (sgaParam%iCurGen<sgaParam%arrBestClusterRange(2)) )then
			call initialize( hUmaModel, arrBestPt, sgaParam%nMinTrainPts, 28, 3 )
			call initialize( hCostModel, arrBestPt, sgaParam%nMinTrainPts, 28, 2 )
		else
			call initialize( hUmaModel, arrMeanPt, sgaParam%nMinTrainPts, 28, 3 )
			call initialize( hCostModel, arrMeanPt, sgaParam%nMinTrainPts, 28, 2 )
		endif

!for all SVM models
		call set_options( hUmaModel, strUmaSvmOpt1, 0 )
		call set_options( hUmaModel, strUmaSvmOpt2, 1 )
		call set_options( hUmaModel, strUmaSvmOpt3, 2 )
		call set_options( hCostModel, strCostSvmOpt1, 0 )
		call set_options( hCostModel, strCostSvmOpt2, 1 )

		call train_meta_model( hUmaModel, strUmaTrain, "umavalid" )
		call train_meta_model( hCostModel, strCostTrain, "umavalid" )

		write( hGaOut, 1114 )get_model_count( hUmaModel )


		!empty the pool
		g_nPooledPdes = 0

		!empty the error cache right after new models are trained.
		nFitErrors = 0
		nPosCount = 0
	endif

	if( hUmaModel/=0 )call UpdateModelErrors()

	1114 format( 'Model is trained with ', i5, 2x, 'internal models' )

contains
	subroutine UpdateModelErrors()
	implicit none
		integer :: pos
		double precision, dimension(1) :: arrModelErrorMean, arrModelErrorStd
		!compute the model error for the cached or pde individuals
		do i=1, sgaParam%nPopSize
			if( sgaParam%arrFlags(i)==NN_EVAL )cycle

			pos = SearchChromCache(hChromCache, sgaParam%arrPop(i,:), sgaParam%nChromBits, arrCachedFits, nCachedFits)
			call assert( pos/=0 )

			if( find(arrChromPos, nPosCount, pos)==0 )then
				nPosCount = nPosCount + 1
				arrChromPos(nPosCount) = pos

				nFitErrors = nFitErrors + 1                                                                                                                                                       
				arrFitErrors(nFitErrors,1) = -ComputeModelFitness( sgaParam, hNormParam, i ) + sgaParam%arrFit(i)
			endif
		end do


		!compute the model error (mean and standard deviation)
		if( nFitErrors > 10 )then
	!	if( nFitErrors > 6 )then
			call abs_sort( arrFitErrors, nFitErrors )
			call CalcMeanStd( arrFitErrors(1:int(0.75*nFitErrors),:), arrModelErrorMean, arrModelErrorStd )

			sgaParam%rModelErrorStd = arrModelErrorStd(1)
			sgaParam%rModelErrorMean = arrModelErrorMean(1)

			open(unit=hModelError, file=strModelError, status='unknown', position='append' )

!			write( hModelError, 1113 )(arrFitErrors(i, 1), i=1,int(0.75*nFitErrors))
!			write( hGaOut, 1112 )rModelErrorMean, sgaParam%rModelErrorMean, rModelErrorStd, sgaParam%rModelErrorStd
			write( hModelError, 1115 )sgaParam%iCurGen, get_model_count( hUmaModel ), arrModelErrorMean(1), sgaParam%rModelErrorMean, arrModelErrorStd(1), sgaParam%rModelErrorStd
			close( hModelError )
		endif

		1113 format( 4(f12.4, 4x) )
		1115 format( i4, 5x, i4, 5x, 4(f12.4,4x) )
	end subroutine

end subroutine


subroutine SaveGenInfo( sgaParam, hNormParam, arrIndex, nCachedFits )
implicit none
	!arguments
	type(SGAPARAMSTRUCT), intent(inout) :: sgaParam
	integer, dimension(sgaParam%nPopSize) :: arrIndex
	integer :: hNormParam, nCachedFits

	!variables.
	double precision :: MB1, MB2, rFitness
	integer :: i, id, pos
	double precision, dimension(nCachedFits) :: arrCachedFits
	double precision :: rret

	open(unit=hGaOut, file=strGaOut, status='unknown', position='append' )

	write( hGaOut, 1000 ) sgaParam%iCurGen
	!write the individual information to the out file
	do i = 1, sgaParam%nPopSize
		id = arrIndex(i)

		if( sgaParam%arrFlags(id)==NN_EVAL )then
			rFitness = ComputeModelFitness( sgaParam, hNormParam, id, arrCachedFits )
		else
			call DecodeOne( sgaParam, id )
			call PrepareFitness( id, sgaParam, MB1, MB2 )
			pos = SearchChromCache( hChromCache, sgaParam%arrPop(id,:), sgaParam%nChromBits, arrCachedFits, nCachedFits )
		endif
		call SaveIndInfo( sgaParam, arrCachedFits, id )
	end do
	close( hGaOut )

	1000 format(/'#################  Generation',i5,'  #################')

end subroutine

subroutine SaveIndInfo(sgaParam, arrFits, id)
use sga
use umacase
implicit none
	!arguments
	type(SGAPARAMSTRUCT), intent(inout) :: sgaParam
	double precision, dimension(:), intent(in) :: arrFits
	integer, intent(in) :: id

	!variables
	integer :: j
                                                                                                                                                                                                      
	if( sgaParam%arrFlags(id)==NN_EVAL )then		!neural network
		write( hGaOut, fmt=1074, advance="NO" ) id, sgaParam%arrFit(id)
	else if( sgaParam%arrFlags(id)==CACHE_EVAL )then	!from cache
		write( hGaOut, fmt=1075, advance="NO" ) id, sgaParam%arrFit(id)
	else
		write( hGaOut, fmt=1076, advance="NO" ) id, sgaParam%arrFit(id)
	endif

	write( hGaOut, '(8(2x, f17.6))', advance="NO" )(arrFits(j), j=2,9)

	!well locations
	do j=1, g_nNewWells
		write( hGaOut, fmt=1078, advance="NO" )nint(sgaParam%arrVars(id, j))
	enddo
	!well pumping rates
	do j=j, g_nNewWells+g_nNewExtWells
		write( hGaOut, fmt=1077, advance="NO" )sgaParam%arrVars(id, j)
	enddo
	!new well flags
	do j=j, 2*g_nNewWells+g_nNewExtWells
		write( hGaOut, fmt=1078, advance="NO" )nint(sgaParam%arrVars(id, j))
	enddo

	!old well pumping rates
	do j=j, 2*g_nNewWells+g_nNewExtWells+g_nOldExtWells
		write( hGaOut, fmt=1077, advance="NO" )sgaParam%arrVars(id, j)
	enddo
	!old well pumping flags
	do j=j, 2*g_nNewWells+g_nNewExtWells+g_nOldExtWells+g_nOldWells
		write( hGaOut, fmt=1078, advance="NO" )nint(sgaParam%arrVars(id, j))
	enddo
	!just print the catridge
	write(hGaOut, fmt=1081, advance="YES")

!	1074 format('-',i4, 2x,f17.4)
!	1075 format('*',i4, 2x,f17.4)
!	1076 format(' ',i4, 2x,f17.4)

	1074 format(i4, '(A)', 2x,f17.4)
	1075 format(i4, '(C)', 2x,f17.4)
	1076 format(i4, '(P)', 2x,f17.4)
	1077 format(2x,f7.4)
	1078 format(2x,i2)
	1079 format(2x,f2.0)
	1081 format(1x)
end subroutine SaveIndInfo

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
use condor_helper
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
	integer :: hNormParam																		!the normalize parameters


	nCachedFits = 9			!save the fitnesse, total cost, penalty rdx, penalty tnt, max rdx C, max tnt C, dummy VCF cost, fourth max rdx C and fourth max tnt C into the chromosome cache
	bAnnReady = .false.
                                                                                                                                                                                                          
	call CreateScalings( hNormParam )

	if( sgaParam%iCurGen==1 )then
		!prepare ncp static folder
		call MakeStaticFolder
		!prepare output files
		call PrepareOutputFiles
		!no pde is in pool yet.
		g_nPooledPdes = 0

		rSmoothStdDev = 0
		arrBestPt(29) = 1d100
	endif                                                                                                                                                                                             
                                                                                                                                                                                                          
	!prepare neural networks.
	call assert( ((hUmaModel==0).and.(hCostModel==0)).or.((hUmaModel/=0).and.(hCostModel/=0)) )

	bAnnReady = hUmaModel/=0;
	if( sgaParam%iCurGen==sgaParam%iStartGen+sgaParam%nMaxGens-1 )bAnnReady = .false.

	!update the fitness only
	if( sgaParam%bUpdateFits )then
		call assert( bAnnReady )
		do i=1, sgaParam%nPopSize
			if( sgaParam%arrFlags(i)==NN_EVAL )then
				sgaParam%arrFit(i) = ComputeModelFitness( sgaParam, hNormParam, i )
			endif
		enddo
		sgaParam%bPendingModels = .false.

		call ReleaseNormalize( hNormParam )
		return
	endif

	arrIndex = (/(i, i=1,sgaParam%nPopSize)/)

	!######################### Pass 1 - evaluate the fitness by the ANNs #########################
	sgaParam%arrFlags = -1
	if( bAnnReady )then
		call EvalByAnns( sgaParam, hNormParam )
		call qsort( arrIndex, sgaParam%nPopSize, sizeof(arrIndex(1)), FitCompare )
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
!			call qsort( arrIndex, sgaParam%nPopSize, sizeof(arrIndex(1)), CompareByCost )
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

	call SaveGenInfo( sgaParam, hNormParam, arrIndex, nCachedFits )
	call SaveCountInfo( sgaParam, nPdeIds )

	!######################### Update SSD and ANNs ###########################
	call UpdateSSD( sgaParam, hNormParam, rStdBase, rSmoothStd, rSmoothStdDev )

	!update the ANNs
	call UpdateAnns( sgaParam, hNormParam, bAnnReady, nCachedFits )

	if( sgaParam%bEliteFlag )then
		!the following logic do the injection. injection the best saved pde individuals to the population
		arrIndex = (/(i, i=1,sgaParam%nPopSize)/)
		call qsort( arrIndex, sgaParam%nPopSize, sizeof(arrIndex(1)), FitInjCompare )
		if( sgaParam%iCurGen>1 )then
			do i=lbound(sgaParam%arrFitInj, 1), ubound(sgaParam%arrFitInj,1)
				id = arrIndex( sgaParam%nPopSize-i+1 )
				sgaParam%arrPop(id,:) = sgaParam%arrPopInj(i,:)
				sgaParam%arrFit(id) = sgaParam%arrFitInj(i)
				sgaParam%arrFlags(id) = CACHE_EVAL
			enddo
		endif
		!save the best to the inj
		do i=lbound(sgaParam%arrFitInj, 1), ubound(sgaParam%arrFitInj,1)
			id = arrIndex(i)
			if( sgaParam%arrFlags(id)==NN_EVAL )exit
			do k=lbound(sgaParam%arrFitInj, 1), ubound(sgaParam%arrFitInj,1)
				if( sgaParam%arrFit(id)<sgaParam%arrFitInj(k) )then
					sgaParam%arrPopInj(k,:)=sgaParam%arrPop(id,:)
					sgaParam%arrFitInj(k) = sgaParam%arrFit(id)
					exit
				endif
			enddo
		enddo
	endif

	call ReleaseNormalize( hNormParam )

	!reload the host file                                                                                                                                                                                                       
	call RefreshHosts
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

	integer(2) function FitInjCompare(a1, a2)
		integer :: a1, a2

		double precision :: r1, r2
		r1 = sgaParam%arrFit(a1)
		r2 = sgaParam%arrFit(a2)
		if( sgaParam%arrFlags(a1)/=sgaParam%arrFlags(a2) )then
			if( sgaParam%arrFlags(a1)==NN_EVAL )then
				FitInjCompare = 1
				return
			else if( sgaParam%arrFlags(a2)==NN_EVAL )then
				FitInjCompare = -1
				return
			endif
		endif

		if( (r1 - r2) == 0 )then
			FitInjCompare = 0
		else if( r1>r2 )then
			FitInjCompare = 1
		else
			FitInjCompare = -1
		endif
	end function FitInjCompare

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

integer function TournSel_Prob( arrFits, arrIndex, nSelSize, nTournSize, rErrMean, rErrStd )
use sga
use Std_helper
implicit none
	!argument
	double precision, dimension(*), intent(in) :: arrFits
	integer, dimension(*), intent(in) :: arrIndex
	integer, intent(in) :: nSelSize, nTournSize
	double precision, intent(in) :: rErrMean, rErrStd
	
	!variable
	integer :: iPick, iMate, i

	iPick = 1+dint(dble(nSelSize-1)*UniRand())	!this is not correct, now for debug only
	do i=2, nTournSize
		iMate = 1+dint(dble(nSelSize-1)*UniRand())
		do while ((nSelSize>nTournSize) .and. (iMate==iPick) )
			iMate = 1+dint(dble(nSelSize-1)*UniRand())
		enddo
			
		if( ProbCompareFitness( arrFits(arrIndex(iMate)), arrFits(arrIndex(iPick)), .false., .false., rErrMean, rErrStd )<0 )then
			iPick = iMate
		endif
	enddo
	TournSel_Prob = iPick
end function TournSel_Prob

end module