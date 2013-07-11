module meta_fit
use casemeta
implicit none

integer :: hChromCache
integer :: totalPDE = 0

!two neural networks for prediction
integer :: hUmaModel = 0
integer :: hCostModel = 0
!integer :: nMaxNewPoints = 120

double precision, dimension( 29 ) :: arrBestPt

contains

!the logic of FitEvalNet is changed on 10/08/03 
! first the subroutine evaluate all fitnesses by Neural network
! then the subroutine randomly pick individuals to evaluate them by PDE.
! All the PDE results are saved into the cache.
! The statiscal file is ga.stat.
                                                                                                                                                                                                          
!using matlab to create neural network                                                                                                                                                                    
subroutine FitEval(sgaParam)                                                                                                                                                                       
use sga
use fmt_helper
use condor_helper
use umacase
use condorio
use chmcache
use scaling
use costfun
use STD_HELPER
USE DFPORT                                                                                                                                                                                                

use metamodel
!use regional_svm_model
use regional_neural_model
!use neural_model

implicit none                                                                                                                                                                                             
	!argument                                                                                                                                                                                         
	type(SGAPARAMSTRUCT), intent(inout) :: sgaParam                                                                                                                                                   
	double precision, dimension(3*g_nNewWells + g_nOldWells + 5) :: arrVars, arrVarsTmp				!the vars for parsing and normalize the NN parameters.

	double precision, dimension(sgaParam%nPopSize, 9) :: arrFits								!save total cost, penalty rdx, penalty tnt, max rdx C, max tnt C and fitnesses into the chromosome cache
	double precision, dimension(sgaParam%nPopSize, 3*g_nNewWells+g_nOldWells) :: arrVarBuf		!the generation inputs var cache, for calculating statics.
	double precision, dimension( 3*g_nNewWells+g_nOldWells ) :: arrMeans, arrStds				!the means and deviation of the input buffer
!	double precision, dimension( 3*g_nNewWells+g_nOldWells+1 ) :: arrBestPt
	integer :: hNormParam																		!the normalize parameters

	!variable                                                                                                                                                                                         
	integer :: i, j, k, id, nPdeCount
	logical :: bUmaNetOk, bCostNetOk, bRetrainNN, bNNReady, bSkipSave
	integer, dimension(6) :: train_gen = (/3, 8, 15, 25, 40, 70/)
!	integer, dimension(6) :: train_gen = (/4, 8, 15, 25, 40, 70/)
!	integer, dimension(6) :: train_gen = (/2, 6, 12, 20, 35, 60/)
	double precision, save :: rProbPde = 0.2857
!	integer :: nInitSampling = 32
	integer :: nFits, nDoPde
	integer :: nInputs, nOutputs

	!variables for adaptive sampling control
	integer, save :: nSampleBase, nNewPoints
	double precision, save :: rDiff, rSmoothStd, rSmoothStdDev, rSmoothUmaMSE, rSmoothUmaDev
	double precision, save :: rStdBase, rUmaMSEBase, rUmaMSE, rCostMSE
	double precision :: rAvgMean, rAvgStd, rSampRatio
	double precision :: rNetBias
	double precision :: rModelErrorMean, rModelErrorStd

!	double precision, dimension( sgaParam%nPopSize ), save :: arrFitErrors
	double precision, dimension( 500 ), save :: arrFitErrors
	integer, save :: nFitErrors
	integer :: nSaveFitErrors

	!variables for sorting
!	integer, dimension( sgaParam%nPopSize*2, sgaParam%nChromBits ) :: arrChromPool
	double precision, dimension( sgaParam%nPopSize ) :: arrFitPool
	integer, dimension( sgaParam%nPopSize ) :: arrIndex, arrIndexTmp
		
	double precision :: rFitness, rTotalCost, rPenTnt, rPenRdx, rMaxTntC, rMaxRdxC, rDummyVCGCost, MB1, MB2
	integer :: iDryCellC, iDryCellH, nRemYear
	double precision :: rFourMaxRdxC, rFourMaxTntC

	!a small cache for computing model error
	integer, save :: arrChromPos( 300 )
!	integer, save :: arrChromPos( sgaParam%nPopSize )
	integer, save :: nChromPos, nPos

	integer :: arrJobIds(sgaParam%nPopSize), arrCachedIds(sgaParam%nPopsize)
	integer :: nJobCount, nCachedCount

!	nChromPos = 0
!	nFitErrors = 0
	nJobCount = 0
	nCachedCount = 0

	nFits = 9			!save the fitnesse, total cost, penalty rdx, penalty tnt, max rdx C, max tnt C, dummy VCF cost, fourth max rdx C and fourth max tnt C into the chromosome cache
	nInputs = 3*g_nNewWells + g_nOldWells
	nOutputs = 5
                                                                                                                                                                                                          
	bUmaNetOk = .false.
	bCostNetOk = .false.
	bRetrainNN = .false.
	bNNReady = .false.

	call CreateScalings( hNormParam )

	if( sgaParam%iCurGen==1 )then

		call MakeStaticFolder

		!prepare output files
		call PrepareOutputFiles

		nNewPoints = 0
		rSmoothStdDev = 0

		arrBestPt(nInputs+1) = 1d100
	endif                                                                                                                                                                                             
                                                                                                                                                                                                          
	open(unit=hGaOut, file=strGaOut, status='unknown', position='append' )
	open(unit=hGaNN, file=strGaNN, status='unknown', position='append' )
	open(unit=hGaStat, file=strGaStat, status='unknown', position='append' )
	open(unit=hGaCount, file=strGaCount, status='unknown', position='append' )
	open(unit=hModelError, file=strModelError, status='unknown', position='append' )
                                                                                                                                                                                                          
	write( hGaOut, 1111 ) sgaParam%iCurGen
	write( hGaNN, 1111 ) sgaParam%iCurGen
                                                                                                                                                                                                          
	!prepare neural networks.
	call assert( ((hUmaModel==0).and.(hCostModel==0)).or.((hUmaModel/=0).and.(hCostModel/=0)) )

	bUmaNetOk = hUmaModel/=0;
	bCostNetOk = hCostModel/=0;
	if( sgaParam%iCurGen==sgaParam%iStartGen+sgaParam%nMaxGens-1 )then
		bUmaNetOk = .false.
		bCostNetOk = .false.
	endif
	if( bUmaNetOk )then
		bNNReady = .true.
	endif

	!update the fitness only
	if( sgaParam%bUpdateFits )then
		call assert( bUmaNetOk .and. bCostNetOk )
		do i=1, sgaParam%nPopSize
			if( sgaParam%arrFlags(i)==NN_EVAL )then
				sgaParam%arrFit(i) = ComputeModelFitness( i )
			endif
		enddo
		sgaParam%bPendingModels = .false.
		return
	endif

	rUmaMSE = 0.0
	rCostMSE = 0.0
	nPdeCount = 0
	sgaParam%arrFlags = -1
	!the first loop assumes using NN only
	if( bUmaNetOk )then
		do i=1, sgaParam%nPopSize
			sgaParam%arrFit(i) = ComputeModelFitness( i )
			sgaParam%arrFlags(i) = NN_EVAL		!0 means a NN results.

			!save data for analysis and ouputs.
			arrFits(i, :) = (/rFitness, rTotalCost, rPenRdx, rPenTnt, rMaxRdxC, rMaxTntC, rDummyVCGCost, rFourMaxRdxC, rFourMaxTntC/)

			!save the normalized variables in buffer for later statical analysis
			call Normalize( hNormParam, arrVars, 1 )
			arrVarBuf(i, :) = arrVars(1:nInputs)
			call SaveNNInd( i )
		enddo
	endif

	!sort the chrosomes by the fitness values to pick the best individuals.
	!put the current generation into the pool to sort it with it last generation
	do i=1, sgaParam%nPopSize
		arrFitPool(i) = sgaParam%arrFit(i)
	enddo
	arrIndex = (/(i, i=1,sgaParam%nPopSize)/)

	if( bUmaNetOk )then	!if no NN is ready, needn't the sorting, use natural order.
		call qsort( arrIndex, sgaParam%nPopSize, sizeof(arrIndex(1)), FitCompare )
		nDoPde = sgaParam%nInitSampling * (rSmoothStd-rSmoothStdDev)/(rStdBase - rSmoothStdDev) !* max( rSmoothHeadMSE / rHeadMSEBase, rSmoothRiskMSE / rRiskMSEBase )
	else
		nDoPde = sgaParam%nPopSize	!all should use pde models or cache
	endif

	!the second loop checked cached individuals
	do i = 1, sgaParam%nPopSize
		id = arrIndex(i)
		call DecodeOne( sgaParam, id )
		call PrepareFitness( id, sgaParam, MB1, MB2, arrVars )

		!either NN is not activated, or it is activated, but the individual is selected to evaluation for PDE.
!		if( (.not.bUseNN) .or. (UniRand()<0.1) )then
!			if( sgaParam%arrFlags(i)==2 )then	!the result is from the cache, needn't PDE results.
!				continue
!			end if

!		bUpdateFit = .false.
!		if( (sgaParam%iCurGen==96) .or. (SearchChromCache( hChromCache, sgaParam%arrPop(id,:), sgaParam%nChromBits, arrFits(id,:), nFits )==0) )then
		nPos = SearchChromCache( hChromCache, sgaParam%arrPop(id,:), sgaParam%nChromBits, arrFits(id,:), nFits )
		if( nPos/=0 )then
			!get it in the cache
			rFitness = arrFits(id, 1)
			rTotalCost = arrFits(id, 2)
			rPenRdx = arrFits(id, 3)
			rPenTnt = arrFits(id, 4)
			rMaxRdxC = arrFits(id, 5)
			rMaxTntC = arrFits(id, 6)
			rDummyVCGCost = arrFits(id, 7)
			rFourMaxRdxC = arrFits(id,8)
			rFourMaxTntC = arrFits(id,9)

			!compute the model error
			if( sgaParam%arrFlags(id)==NN_EVAL )then
				if( find(arrChromPos, nChromPos, nPos)==0 )then
					nChromPos = nChromPos + 1
					arrChromPos(nChromPos) = nPos

					nFitErrors = nFitErrors + 1                                                                                                                                                       
					arrFitErrors(nFitErrors) = sgaParam%arrFit(id) - (rFitness+MB1+MB2)
				endif
			endif

			sgaParam%arrFit(id) = rFitness + MB1 + MB2		!catched PDE fitness doesn't include VCF cost
			sgaParam%arrFlags(id) = CACHE_EVAL
			arrCachedIds( nCachedCount+1 ) = id
			nCachedCount = nCachedCount + 1
!			bUpdateFit = .true.
		end if

!		if( bUpdateFit )then
!			sgaParam%arrFit(id) = rFitness + MB1 + MB2
!			!save the normalized variables in buffer for later statical analysis
!			call Normalize( normRisk, arrVars, 1 )
!			arrVarBuf(id, :) = arrVars(1:nInputs)
!		end if
	end do

	if( bUmaNetOk )then
		if( sgaParam%iCurGen>35 )then
!		if( .true. )then
			call ExpSelPdes( sgaParam, arrIndex, sgaParam%nInitSampling, nDoPde, arrJobIds, nJobCount)
!			call SelectPdes( sgaParam, 10, nDoPde, arrJobIds, nJobCount )
		else
			call SelectPdes( sgaParam, 8, nDoPde, arrJobIds, nJobCount )
		endif
	else
		nJobCount = 0
		do i = 1, sgaParam%nPopSize
			if( sgaParam%arrFlags(i)/=CACHE_EVAL )then
				arrJobIds(nJobCount+1) = i
				nJobCount = nJobCount + 1
			endif
		enddo
!		arrJobIds = (/(i, i=1, sgaParam%nPopSize)/)
!		nJobCount = sgaParam%nPopSize
	endif
	nPdeCount = nPdeCount + nJobCount
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

	rModelErrorMean = 0
	rModelErrorStd	= 0
	!collect jobs
	do i=1, nJobCount
		id = arrJobIds(i)
		call DecodeOne( sgaParam, id )
		call PrepareFitness( id, sgaParam, MB1, MB2, arrVars )
		call CollectSlave( id, sgaParam%iCurGen, rFitness, rTotalCost, rPenRdx, rPenTnt, rMaxRdxC, rMaxTntC, rDummyVCGCost, nRemYear, rFourMaxRdxC, rFourMaxTntC, iDryCellC, iDryCellH )
		bSkipSave = .false.
		if( (rFourMaxTntC>100) .or. (rMaxTntC >100) )bSkipSave = .true.
!		rMaxRdxC = min( rMaxRdxC, 100.d0 )
!		rMaxTntC = min( rMaxTntC, 100.d0 )
!		rDummyVCGCost = max( 0.d0, rDummyVCGCost )
		rMaxRdxC = max( 0.1, min( rMaxRdxC, 25.d0 ) )
		rMaxTntC = max( 0.1, min( rMaxTntC, 75.d0 ) )
		rDummyVCGCost = max( 0.1, min( 1000.d0, rDummyVCGCost ) )
		rFourMaxRdxC = max(0.1, min(rFourMaxRdxC, 25.d0) )
		rFourMaxTntC = max(0.1, min(rFourMaxTntC, 75.d0) )
!		arrVars(nInputs+1) = rMaxRdxC
!		arrVars(nInputs+2) = rMaxTntC
!the code is changed so that the third parameter predicts the actual total cost, rather than the dummy cost.
!		arrVars(nInputs+1) = log10(rMaxRdxC)
!		arrVars(nInputs+2) = log10(rMaxTntC)
		arrVars(nInputs+3) = log10(rDummyVCGCost)
		arrVars(nInputs+4) = log10(rFourMaxRdxC)
		arrVars(nInputs+5) = log10(rFourMaxTntC)
		arrVars(nInputs+1) = rMaxRdxC / rFourMaxRdxC
		arrVars(nInputs+2) = rMaxTntC / rFourMaxTntC

		!save data for analysis and ouputs.
		arrFits(id, :) = (/rFitness, rTotalCost, rPenRdx, rPenTnt, rMaxRdxC, rMaxTntC, rDummyVCGCost, rFourMaxRdxC, rFourMaxTntC/)
		call InsertChromCache( hChromCache, sgaParam%arrPop(id,:), sgaParam%nChromBits, arrFits(id,:), nFits )

		!so no redundent chromosomes will be written into the training file.
!		if( UniRand()<0.999 )then                                                                                                                                                          
!		if( mod(nPdeCount,4)/=0 )then
		call Normalize( hNormParam, arrVars, 1 )
		if( .not. bSkipSave )then

			nNewPoints = nNewPoints + 1
			call AppendDataset()
			open( unit=hTrainFit, file=strTrainFit, status='unknown', position='append' )
			write( hTrainFit, *)rFitness + MB1 + MB2
			close(hTrainFit)
		endif                       
		
		!compute the model error
		if( sgaParam%arrFlags(id)==NN_EVAL )then
			nFitErrors = nFitErrors + 1                                                                                                                                                       
			arrFitErrors(nFitErrors) = sgaParam%arrFit(id) - (rFitness+MB1+MB2)
		endif

		!save result to sgaParam
		sgaParam%arrFlags(id) = PDE_EVAL
		sgaParam%arrFit( id ) = rFitness + MB1 + MB2

		!save the normalized variables in buffer for later statical analysis
		arrVarBuf(id, :) = arrVars(1:nInputs)
		if( sgaParam%arrFit(id)<arrBestPt(nInputs+1) )then
			arrBestPt(nInputs+1) = sgaParam%arrFit(id)
			arrBestPt(1:nInputs) = arrVars(1:nInputs)
		endif

		!calculate the NN MSE on the pde
		if( bUmaNetOk )then
			!calculate the neural network testing error
			!call Normalize( hNormParam, arrVars, 1 )
!			rUmaMSE = rUmaMSE + test_net_cluster( hUmaNet, arrVars, arrVars(nInputs+1:nInputs+3), 1 )
!			rCostMSE = rCostMSE + test_net_cluster( hCostNet, arrVarsTmp, arrVarsTmp(nInputs+1:nInputs+2), 1 )
!			write( hGaNN, * )rUmaMSE, rCostMSE
		endif

	end do

	!write the individual information to the out file
	do i = 1, sgaParam%nPopSize
		id = arrIndex(i)
		call DecodeOne( sgaParam, id )
		call PrepareFitness( id, sgaParam, MB1, MB2 )

		call SaveIndInfo( sgaParam, arrFits(id,:), id )
	end do

!resort the reevaluated fitness
	totalPDE = totalPDE + nPdeCount
	!save the statistical information.                                                                                                                                                                
	rUmaMSE = rUmaMSE / nPdeCount
	rCostMSE = rCostMSE / nPdeCount
	call CalcMeanStd( arrVarBuf, arrMeans, arrStds )
	call SaveGenInfo( arrMeans, arrStds, rUmaMSE, rAvgMean, rAvgStd )

	if( sgaParam%iCurGen==1 )then
		rStdBase = rAvgStd
		rSmoothStd = rAvgStd
		rSmoothUmaMSE = rUmaMSE
		rSmoothUmaDev = 0
	else
		!so jacon's algorithm smooth adding!
		rDiff = rAvgStd - rSmoothStd
		rSmoothStd = rSmoothStd + 0.5*(rDiff)
		rSmoothStdDev = rSmoothStdDev + 0.5*( abs(rDiff) - rSmoothStdDev )

		!jacabon'algorithm for the error
		rDiff = rUmaMSE - rSmoothUmaMSE
		rSmoothUmaMSE = rSmoothUmaMSE + 0.5*(rDiff)
		rSmoothUmaDev = rSmoothUmaDev + 0.5*(abs(rDiff)-rSmoothUmaDev)
		rSmoothUmaMSE = rSmoothUmaMSE + 4*rSmoothUmaDev
	end if

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

	if( (nNewPoints>=sgaParam%nRetrainingPoolSize) .and. (sgaParam%iCurGen>=train_gen(1)) .and. &
			& sgaParam%iCurGen<sgaParam%nMaxGens )then
		bRetrainNN = .true.
	endif

	rNetBias = min( 1.5, dble(nDoPde)/dble(sgaParam%nInitSampling) * 1.5 )
	!retrain the neural network                                                                                                                                                                       
	if( bRetrainNN )then
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
		if( (sgaParam%iCurGen>=sgaParam%arrBestClusterRange(1)) .and. (sgaParam%iCurGen<sgaParam%arrBestClusterRange(2)) )then
			call initialize( hUmaModel, arrBestPt, sgaParam%nMinTrainPts, arrUmaNet, 3, (/0.d0,0.d0,0.d0,0.0d0/) )
			call initialize( hCostModel, arrBestPt, sgaParam%nMinTrainPts, arrCostNet, 3, (/dble( (log10(2.1*1.05)+0.3)/1.7*1.8-0.9 ), dble( (log10(2.8*1.05)+0.3)/2.2*1.8-0.9 ), 1.2d0/) )
		else
			call initialize( hUmaModel, arrMeans, sgaParam%nMinTrainPts, arrUmaNet, 3, (/0.d0,0.d0,0.d0,0.0d0/) )
			call initialize( hCostModel, arrMeans, sgaParam%nMinTrainPts, arrCostNet, 3, (/dble( (log10(2.1*1.05)+0.3)/1.7*1.8-0.9 ), dble( (log10(2.8*1.05)+0.3)/2.2*1.8-0.9 ), 1.2d0/) )
		endif

!for region SVM models
!		if( (sgaParam%iCurGen>=sgaParam%arrBestClusterRange(1)) .and. (sgaParam%iCurGen<sgaParam%arrBestClusterRange(2)) )then
!			call initialize( hUmaModel, arrBestPt, sgaParam%nMinTrainPts, 28, 3 )
!			call initialize( hCostModel, arrBestPt, sgaParam%nMinTrainPts, 28, 2 )
!		else
!			call initialize( hUmaModel, arrMeans, sgaParam%nMinTrainPts, 28, 3 )
!			call initialize( hCostModel, arrMeans, sgaParam%nMinTrainPts, 28, 2 )
!		endif

!for region SVM models
!		call set_options( hUmaModel, strUmaSvmOpt1, 0 )
!		call set_options( hUmaModel, strUmaSvmOpt2, 1 )
!		call set_options( hUmaModel, strUmaSvmOpt3, 2 )
!		call set_options( hCostModel, strCostSvmOpt1, 0 )
!		call set_options( hCostModel, strCostSvmOpt2, 1 )

		call train_meta_model( hUmaModel, strUmaTrain, "umavalid" )
		call train_meta_model( hCostModel, strCostTrain, "umavalid" )

		write( hGaOut, 1114 )get_model_count( hUmaModel )

		!compute the error right after the training
		nFitErrors = 0
		nChromPos = 0
		do i=1, sgaParam%nPopSize
			if( sgaParam%arrFlags(i)==PDE_EVAL )then
				nFitErrors = nFitErrors + 1
				arrFitErrors(nFitErrors) = sgaParam%arrFit(i) - ComputeModelFitness( i )
			endif
		enddo

!		call train_net_cluster( hUmaNet, strUmaTrain, arrBestPt, arrUmaNet, 3, (/0.d0,0.d0,0.d0,0.0d0/), nMinTrainPts )
!		call train_net_cluster( hCostNet, strCostTrain, arrBestPt, arrCostNet, 3, (/dble( (log10(2.1*1.05)+0.3)/1.7*1.8-0.9 ), dble( (log10(2.8*1.05)+0.3)/2.2*1.8-0.9 ), 1.2d0/), nMinTrainPts )

		rUmaMSEBase = rUmaMSE
		rSmoothUmaMSE = rUmaMSE
		nNewPoints = 0

		open(unit=121, file=strUmaTrain, status='unknown')
		open(unit=122, file=strCostTrain, status='unknown')
		do i=1, 10                                                                                                                                                                                
			call DecodeOne( sgaParam, i )
			call PrepareFitness( i, sgaParam, MB1, MB2, arrVars )
			read(121, *)( arrVars(k), k=1,nInputs+3 )
			read(122, *)( arrVarsTmp(k), k=1,nInputs+2 )

			call predict_meta_model( hUmaModel, arrVars, arrVars(nInputs+1:nInputs+3) )
			call predict_meta_model( hCostModel, arrVarsTmp, arrVarsTmp(nInputs+1:nInputs+2) )
			print*, arrVars(29), arrVars(30), arrVars(31), arrVarsTmp(29), arrVarsTmp(30)
		end do
		close(121)
		close(122)                                                                                                                                                                               
                                                                                                                                                                                                          
		call SaveTrainInfo( rUmaMSE, rCostMSE )
	endif

	!compute the model error (mean and standard deviation)
	if( nFitErrors > 10 )then
!	if( nFitErrors > 6 )then
		call abs_sort( arrFitErrors, nFitErrors )
		nSaveFitErrors = nFitErrors
		nFitErrors = nFitErrors*0.75

		rModelErrorMean = 0.0
		do i=1, nFitErrors
			rModelErrorMean = rModelErrorMean + arrFitErrors(i)
		enddo
		rModelErrorMean = rModelErrorMean / nFitErrors
		rModelErrorStd = 0.0
		do i=1, nFitErrors
			rModelErrorStd = rModelErrorStd + (arrFitErrors(i)-rModelErrorMean)**2.0
		enddo
		rModelErrorStd = sqrt( rModelErrorStd / (nFitErrors-1) )

		sgaParam%rModelErrorStd = rModelErrorStd
		sgaParam%rModelErrorMean = rModelErrorMean

		write( hGaOut, 1113 )(arrFitErrors(j), j=1,nFitErrors)
		write( hGaOut, 1112 )rModelErrorMean, sgaParam%rModelErrorMean, rModelErrorStd, sgaParam%rModelErrorStd
		write( hModelError, 1115 )sgaParam%iCurGen, get_model_count( hUmaModel ), rModelErrorMean, sgaParam%rModelErrorMean, rModelErrorStd, sgaParam%rModelErrorStd
		nFitErrors = nSaveFitErrors
	endif

	call ReleaseNormalize( hNormParam )

	close( hGaStat )
	close( hGaOut )
	close( hGaNN )
	close( hGaCount )
	close( hModelError )
    
	!reload the host file                                                                                                                                                                                                       
	call RefreshHosts

	1111 format(/'#################  Generation',i5,'  #################')
	1112 format( 'Model Error (mean, std, sga:std):', 4(f12.4, 4x) )
	1113 format( 4(f12.4, 4x) )
	1114 format( 'Model is trained with ', i5, 2x, 'internal models' )
	1115 format( i4, 5x, i4, 5x, 4(f12.4,4x) )
contains
	double precision function ComputeModelFitness( index )
		integer:: index

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

		ComputeModelFitness = rFitness + MB1 + MB2
	end function
	integer(2) function FitCompare(a1, a2)
		integer :: a1, a2

		double precision :: r1, r2
		r1 = arrFitPool(a1)
		r2 = arrFitPool(a2)

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

	subroutine AppendDataset( )
	implicit none
		!argument
		character(80) :: strUmaFile, strCostFile

		strUmaFile = strUmaTrain
		strCostFile = strCostTrain

		!writting it to the training file or testing file
		call AppendToFile( strUmaFile, arrVars, nInputs+3 )
		arrVarsTmp = (/arrVars(1:nInputs), arrVars(nInputs+4), arrVars(nInputs+5), 0.d0, 0.d0/)
		call AppendToFile( strCostFile, arrVarsTmp, nInputs+2 )
	end subroutine AppendDataset

	subroutine SaveNNInd( id )
	use sga
	use fmt_helper
	implicit none
		!arguments
		integer, intent(in) :: id

!		do i=1, sgaParam%nPopSize
!			id = arrIndex(i)
!			write( hGaNN, fmt = 1000 )id, arrVarBuf(id, :)
!		enddo
		write( hGaNN, fmt = 1000 )id, arrVars
			
		1000 format(i4,2x,31(f17.4,2x))
	end subroutine

	subroutine SaveTrainInfo( rUmaMSE, rCostMSE )
	implicit none
		double precision :: rUmaMSE, rCostMSE
                                                                                                                                                                                                          
		write( hGaOut, * )'Neural network is trained, information'                                                                                                                                                
		write( hGaOut, 1001 ) rUmaMSE, rCostMSE

		1001 format(2(f10.8))
	end subroutine                                                                                                                                                                                    
                                                                                                                                                                                                          
	subroutine SaveGenInfo( arrMeans, arrStds, rUmaMSE, rAvgMean, rAvgStd )
	implicit none                                                                                                                                                                                     
		double precision, dimension(:) :: arrMeans, arrStds
		double precision :: rUmaMSE
		double precision :: rAvgMean, rAvgStd

		integer :: i, dim

		integer :: nPdeNum, nNetNum, nCacheNum

		dim = ubound(arrMeans,1) - lbound(arrMeans,1) + 1
		write( hGaOut, * )'generation information'                                                                                                                                                
		write( hGaOut, 1500 ) ( arrMeans(i), i=1, nInputs )
		write( hGaOut, 1501 ) ( arrStds(i), i=1, nInputs )

		write( hGaStat, 999 )sgaParam%iCurGen
		write( hGaStat, 1500 ) ( arrMeans(i), i=1, nInputs )
		write( hGaStat, 1501 ) ( arrStds(i), i=1, nInputs )

		rAvgMean = sum(arrMeans)
		rAvgMean = rAvgMean / dim

		rAvgStd = sum( arrStds )
		rAvgStd = rAvgStd / dim
		write( hGaOut, 1002 ) rAvgMean, rAvgStd
		write( hGaOut, 1001 ) rUmaMSE
		write( hGaOut, 1003 ) nNewPoints, nPdeCount, rStdBase, rSmoothStd, rSmoothStdDev
		write( hGaOut, 1004 ) rUmaMSEBase, rSmoothUmaMSE, rSmoothUmaDev

		write( hGaStat, 1002 ) rAvgMean, rAvgStd
		write( hGaStat, 1001 ) rUmaMSE
		write( hGaStat, 1003 ) nNewPoints, nPdeCount, rStdBase, rSmoothStd, rSmoothStdDev
		write( hGaStat, 1004 ) rUmaMSEBase, rSmoothUmaMSE, rSmoothUmaDev

		nNetNum = 0
		nCacheNum = 0
		nPdeNum = 0
		do i=1, sgaParam%nPopSize
			if( sgaParam%arrFlags(i)==NN_EVAL )then
				nNetNum = nNetNum + 1
			else if( sgaParam%arrFlags(i)==PDE_EVAL )then
				nPdeNum = nPdeNum + 1
			else if( sgaParam%arrFlags(i)==CACHE_EVAL )then
				nCacheNum = nCacheNum + 1
			endif
		end do
		write( hGaCount, * )sgaParam%iCurGen, nPdeNum, nCacheNum, nNetNum
                                                                                                                                                                                                          
		999  format( 'Generation ', 4i )                                                                                                                                                                         
		1500 format('Avg Mean:', 28(f9.6,2x))                                                                                                                                                                  
		1501 format('Avg Std:', 28(f9.6,2x))                                                                                                                                                                  
		1001 format('MSE of head and risk=', f10.8, 2x, f10.8)
		1002 format( f10.8, 2x, f10.8 )
		1003 format( i5, 2x, i5, 2x, f10.6, 2x, f10.6, 2x, f10.6 )
		1004 format( f10.6, 2x, f10.6, 2x, f10.6 )
	end subroutine                                                                                                                                                                                    
                                                                                                                                                                                                          
	subroutine AppendToFile( strFile, arrVars, n )                                                                                                                                                    
	implicit none                                                                                                                                                                                     
		!arguments                                                                                                                                                                                
		character(*) :: strFile                                                                                                                                                                   
		double precision, dimension(:) :: arrVars                                                                                                                                                 
		integer :: n                                                                                                                                                                              
                                                                                                                                                                                                          
		!variable                                                                                                                                                                                 
		integer, parameter :: hFile = 89                                                                                                                                                          
		integer :: i                                                                                                                                                                              
                                                                                                                                                                                                          
		open( unit=hFile, file=strFile, status='unknown', position='append' )                                                                                                                     
		do i=1, n                                                                                                                                                                                 
			write( hFile, fmt=1001, advance="NO")arrVars(i)                                                                                                                                   
		enddo                                                                                                                                                                                     
                                                                                                                                                                                                          
		write(hFile, fmt=1002, advance="YES")                                                                                                                                                     
                                                                                                                                                                                                          
		close(hFile)                                                                                                                                                                              
	                                                                                                                                                                                                  
		1001 format(2x,f10.3)                                                                                                                                                                     
		1002 format(1x)                                                                                                                                                                           
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
end subroutine 

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
			
		if( arrFits(arrIndex(iMate))<arrFits(arrIndex(iPick)) )then
			iPick = iMate
		endif
	enddo
	TournSel = iPick
end function TournSel

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

subroutine SelectPdes( sgaParam, nTournSize, nMaxPdes, arrPdeIds, nPdeCount )
use sga
use std_helper
implicit none
	type(SGAPARAMSTRUCT), intent(in) :: sgaParam
	integer, intent(in) :: nTournSize, nMaxPdes
	integer, intent(out) :: nPdeCount
	integer, dimension(*), intent(out) :: arrPdeIds

	!variables
	integer, dimension(sgaParam%nPopSize) :: arrIndex
	integer :: i, nSelCount, iPick

	!collect the available network estimation ids
	nSelCount = 0
	do i=1, sgaParam%nPopSize
		if( sgaParam%arrFlags(i)==NN_EVAL )then
			arrIndex( nSelCount+1 ) = i
			nSelCount = nSelCount + 1
		endif
	enddo

	nPdeCount = min( nMaxPdes, nSelCount )
	!do tournment selection
	do i=1, nPdeCount
!		iPick = TournSel_Prob( sgaParam%arrFit, arrIndex, nSelCount, nTournSize, sgaParam%rModelErrorMean, sgaParam%rModelErrorStd )
		iPick = TournSel( sgaParam%arrFit, arrIndex, nSelCount, nTournSize )
		arrPdeIds(i) = arrIndex(iPick)
		call SwapInt( arrIndex(iPick), arrIndex(nSelCount) )
		nSelCount = nSelCount-1
	enddo
end subroutine SelectPdes

subroutine ExpSelPdes( sgaParam, arrIndex, nInitSampling, nMaxPdes, arrPdeIds, nPdeCount )
use sga
use std_helper
implicit none
	type(SGAPARAMSTRUCT), intent(in) :: sgaParam
	integer, dimension(:) :: arrIndex
	integer, intent(in) :: nMaxPdes, nInitSampling
	integer, intent(out) :: nPdeCount
	integer, dimension(*), intent(out) :: arrPdeIds

	!arguments
	integer :: i, id

	nPdeCount = 0
	do i=1, sgaParam%nPopSize
		id = arrIndex(i)
		if( nPdeCount<nMaxPdes )then
!			if( (sgaParam%arrFlags(i)==NN_EVAL) .and. (UniRand()<exp(-1.0/nInitSampling*i)) )then
			if( (sgaParam%arrFlags(id)==NN_EVAL) )then
				arrPdeIds(nPdeCount+1) = id
				nPdeCount = nPdeCount + 1
			endif
		else
			exit
		endif
	enddo
end subroutine ExpSelPdes


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

	1074 format('-',i4,2x,f17.4)
	1075 format('*',i4,2x,f17.4)
	1076 format(' ',i4,2x,f17.4)
	1077 format(2x,f7.4)
	1078 format(2x,i2)
	1079 format(2x,f2.0)
	1081 format(1x)
end subroutine SaveIndInfo

end module