module cluster_fit
implicit none

integer, parameter :: hGAOut	= 1000
integer, parameter :: hGaStat	= 1001
integer, parameter :: hGaNN		= 1002
integer, parameter :: hGaCount	= 1003
integer, parameter :: hTrainFit = 1004
                                                                                                                                                                                                          
character(*), parameter :: strGaOut	= 'ga.out'  
character(*), parameter :: strGaStat = 'ga.stat'
character(*), parameter :: strGaNN = 'gann.out'
character(*), parameter :: strGaCount = 'gacount.out'
character(*), parameter :: strTrainFit = 'trainfit.dat'

character(*), parameter :: strUmaTrain = 'umatrain.dat'
character(*), parameter :: strCostTrain = 'umacosttr.dat'
                                                                                                                                                                                                          
integer :: hChromCache
integer :: totalPDE = 0

!two neural networks for prediction
integer :: hUmaNet = 0
integer :: hCostNet = 0
integer :: nMinTrainPts = 400
integer :: nMaxNewPoints = 120

integer, dimension(3) :: arrUmaNet = (/28, 12, 3/)
integer, dimension(3) :: arrCostNet = (/28, 12, 2/)

contains

!the logic of FitEvalNet is changed on 10/08/03 
! first the subroutine evaluate all fitnesses by Neural network
! then the subroutine randomly pick individuals to evaluate them by PDE.
! All the PDE results are saved into the cache.
! The statiscal file is ga.stat.
                                                                                                                                                                                                          
!using matlab to create neural network                                                                                                                                                                    
subroutine FitEvalCluster(sgaParam)                                                                                                                                                                       
use sga
use fmt_helper
use condor_helper
use umacase
use condorio
use neuro_cluster
use chmcache
use scaling
use costfun
use STD_HELPER
USE DFPORT                                                                                                                                                                                                
implicit none                                                                                                                                                                                             
	!argument                                                                                                                                                                                         
	type(SGAPARAMSTRUCT), intent(inout) :: sgaParam                                                                                                                                                   
	double precision, dimension(3*g_nNewWells + g_nOldWells + 5) :: arrOrigMin, arrOrigMax			!(x,y), pumping rates for new wells, old wells
	double precision, dimension(3*g_nNewWells + g_nOldWells + 5) :: arrNormMin, arrNormMax
	double precision, dimension(3*g_nNewWells + g_nOldWells + 5) :: arrVars, arrVarsTmp				!the vars for parsing and normalize the NN parameters.

	double precision, dimension(sgaParam%nPopSize, 9) :: arrFits								!save total cost, penalty rdx, penalty tnt, max rdx C, max tnt C and fitnesses into the chromosome cache
	double precision, dimension(sgaParam%nPopSize, 3*g_nNewWells+g_nOldWells) :: arrVarBuf		!the generation inputs var cache, for calculating statics.
	double precision, dimension( 3*g_nNewWells+g_nOldWells ) :: arrMeans, arrStds				!the means and deviation of the input buffer
	double precision, dimension( 3*g_nNewWells+g_nOldWells+1 ) :: arrBestPt
	integer :: hNormParam																		!the normalize parameters

	!variable                                                                                                                                                                                         
	integer :: i, j, k, id, nPdeCount
	logical :: bUmaNetOk, bCostNetOk, bRetrainNN, bNNReady, bSkipSave
!	integer, dimension(6) :: train_gen = (/8, 10, 18, 30, 50, 80/)
!	integer, dimension(6) :: train_gen = (/5, 10, 18, 30, 50, 80/)
	integer, dimension(6) :: train_gen = (/3, 8, 15, 25, 40, 70/)
!	integer, dimension(6) :: train_gen = (/4, 8, 15, 25, 40, 70/)
!	integer, dimension(6) :: train_gen = (/2, 6, 12, 20, 35, 60/)
	double precision, save :: rProbPde = 0.2857
	integer :: nInitSampling = 30
	integer :: nFits, nDoPde
	integer :: nInputs, nOutputs

	!variables for adaptive sampling control
	integer, save :: nSampleBase, nNewPoints
	double precision, save :: rDiff, rSmoothStd, rSmoothStdDev, rSmoothUmaMSE, rSmoothUmaDev
	double precision, save :: rStdBase, rUmaMSEBase, rUmaMSE, rCostMSE
	double precision :: rAvgMean, rAvgStd, rSampRatio
	double precision :: rNetBias

	!variables for sorting
!	integer, dimension( sgaParam%nPopSize*2, sgaParam%nChromBits ) :: arrChromPool
	double precision, dimension( sgaParam%nPopSize ) :: arrFitPool
	integer, dimension( sgaParam%nPopSize ) :: arrIndex, arrIndexTmp
		
	double precision :: rFitness, rTotalCost, rPenTnt, rPenRdx, rMaxTntC, rMaxRdxC, rDummyVCGCost, MB1, MB2
	integer :: iDryCellC, iDryCellH, nRemYear
	double precision :: rFourMaxRdxC, rFourMaxTntC

	integer :: arrJobIds(sgaParam%nPopSize), arrCachedIds(sgaParam%nPopsize)
	integer :: nJobCount, nCachedCount

	nJobCount = 0
	nCachedCount = 0
	if( sgaParam%iCurGen==1 )then
		i = RemoveFileC( strGaOut )
		call MakeStaticFolder
	endif

	nFits = 9			!save the fitnesse, total cost, penalty rdx, penalty tnt, max rdx C, max tnt C, dummy VCF cost, fourth max rdx C and fourth max tnt C into the chromosome cache
	nInputs = 3*g_nNewWells + g_nOldWells
	nOutputs = 5
                                                                                                                                                                                                          
	bUmaNetOk = .false.
	bCostNetOk = .false.
	bRetrainNN = .false.
	bNNReady = .false.

	arrBestPt(nInputs+1) = 1d100
 
	arrOrigMin = 0
	arrOrigMax = 0
	arrNormMin = (/(-1.0, i=1,nInputs), (-0.9, i=1,nOutputs)/)
	arrNormMax = (/(1.0, i=1,nInputs), (0.9, i=1,nOutputs)/)

	call PrepareScallingRange( arrOrigMin, arrOrigMax, nInputs+nOutputs )
	hNormParam = CreateNormalize( arrOrigMin, arrOrigMax, arrNormMin, arrNormMax, nInputs+nOutputs )
                                                                                                                                                                                                          
	if( sgaParam%iCurGen==1 )then
		i=RemoveFileC( strGaOut )
		i=RemoveFileC( strGaStat )
		i=RemoveFileC( strUmaTrain )
		i=RemoveFileC( strCostTrain )
		i=RemoveFileC( strGaNN )
		i=RemoveFileC( strGaCount )
		i=RemoveFileC( strTrainFit )

		call OpenOutputFile( hGaCount, strGaCount )
		rewind( hGaCount )
		write(hGaCount, * ) 'Gen#	PDE#	CACHE#	NNET#'
		close( hGaCount)

		nNewPoints = 0
		rSmoothStdDev = 0
	endif                                                                                                                                                                                             
                                                                                                                                                                                                          
	open(unit=hGaOut, file=strGaOut, status='unknown', position='append' )
	open(unit=hGaNN, file=strGaNN, status='unknown', position='append' )
	open(unit=hGaStat, file=strGaStat, status='unknown', position='append' )
	open(unit=hGaCount, file=strGaCount, status='unknown', position='append' )
                                                                                                                                                                                                          
	write( hGaOut, 1111 ) sgaParam%iCurGen
	write( hGaNN, 1111 ) sgaParam%iCurGen
                                                                                                                                                                                                          
	!prepare neural networks.
	call assert( ((hUmaNet==0).and.(hCostNet==0)).or.((hUmaNet/=0).and.(hCostNet/=0)) )

	bUmaNetOk = hUmaNet/=0;
	bCostNetOk = hCostNet/=0;
	if( sgaParam%iCurGen==sgaParam%iStartGen+sgaParam%nMaxGens-1 )then
		bUmaNetOk = .false.
		bCostNetOk = .false.
	endif
	if( bUmaNetOk )then
		bNNReady = .true.
	endif

	rUmaMSE = 0.0
	rCostMSE = 0.0
	nPdeCount = 0
	sgaParam%arrFlags = -1
	!the first loop assumes using NN only
	if( bUmaNetOk )then
		do i=1, sgaParam%nPopSize
			call DecodeOne( sgaParam, i )
			call PrepareFitness( i, sgaParam, MB1, MB2, arrVars )

			!do the prediction using neural network
			call Normalize( hNormParam, arrVars, 1 )
			!call neural network
			call sim_net_cluster( hUmaNet, arrVars, arrVars(nInputs+1:nInputs+3) )
			call sim_net_cluster( hCostNet, arrVars, arrVars(nInputs+4:nInputs+5) )
			!parse the data back to global variables.
			call UnNormalize( hNormParam, arrVars, 1 )

			!calculate the cost by the output of NN
!			rMaxRdxC = arrVars(nInputs+1)
!			rMaxTntC = arrVars(nInputs+2)
!			rMaxRdxC = 10**arrVars(nInputs+1)
!			rMaxTntC = 10**arrVars(nInputs+2)
			rDummyVCGCost = 10**arrVars(nInputs+3)
			rFourMaxRdxC = 10**arrVars(nInputs+4)
			rFourMaxTntC = 10**arrVars(nInputs+5)
			rMaxRdxC = arrVars(nInputs+1)*rFourMaxRdxC
			rMaxTntC = arrVars(nInputs+2)*rFourMaxTntC

			call CostByNN( rMaxRdxC, rMaxTntC, rDummyVCGCost, rFourMaxRdxC, rFourMaxTntC, rFitness, rTotalCost, rPenRdx, rPenTnt, g_nGridFlagX, g_nGridFlagY, iDryCellC, iDryCellH )
!			sgaParam%arrFit(i) = rTotalCost + rPenRdx + rPenTnt + MB1 + MB2
			sgaParam%arrFit(i) = rFitness + MB1 + MB2
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
		nDoPde = nInitSampling * (rSmoothStd-rSmoothStdDev)/(rStdBase - rSmoothStdDev) !* max( rSmoothHeadMSE / rHeadMSEBase, rSmoothRiskMSE / rRiskMSEBase )
	else
		nDoPde = sgaParam%nPopSize	!all should use pde models or cache
	endif

	!the second loop randomly pick individuals to do pde evaluation
!	if( .not.bUseNN )then
!		nDoPde = sgaParam%nPopSize
!	else if( UniRand()<0.1 )then
!		nDoPde = 10
!	else
!		nDoPde = 0
!	endif

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
		if( SearchChromCache( hChromCache, sgaParam%arrPop(id,:), sgaParam%nChromBits, arrFits(id,:), nFits )==0 )then
			!not in cache, do the actual PDE.
			!sample according to the exponential distribution
!			if( nPdeCount<nDoPde )then
!				if( (.not.bUmaNetOk) .or. ( UniRand()<exp(-1.0/(2.0/3.0*nInitSampling)*i) ) )then
!					arrJobIds( nJobCount+1 ) = id
!					nJobCount = nJobCount + 1
!					call BroadcastSlave( id, sgaParam%iCurGen, g_nGridFlagX, g_nGridFlagY )
!					nPdeCount = nPdeCount + 1
!				endif
!			endif
!			sgaParam%arrFlags(id) = -1
		else	!get it in the cache
			rFitness = arrFits(id, 1)
			rTotalCost = arrFits(id, 2)
			rPenRdx = arrFits(id, 3)
			rPenTnt = arrFits(id, 4)
			rMaxRdxC = arrFits(id, 5)
			rMaxTntC = arrFits(id, 6)
			rDummyVCGCost = arrFits(id, 7)
			rFourMaxRdxC = arrFits(id,8)
			rFourMaxTntC = arrFits(id,9)
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
			call ExpSelPdes( sgaParam, arrIndex, nInitSampling, nDoPde, arrJobIds, nJobCount)
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

		nNewPoints = nNewPoints + 1

		!save data for analysis and ouputs.
		arrFits(id, :) = (/rFitness, rTotalCost, rPenRdx, rPenTnt, rMaxRdxC, rMaxTntC, rDummyVCGCost, rFourMaxRdxC, rFourMaxTntC/)
		call InsertChromCache( hChromCache, sgaParam%arrPop(id,:), sgaParam%nChromBits, arrFits(id,:), nFits )

		!so no redundent chromosomes will be written into the training file.
!		if( UniRand()<0.999 )then                                                                                                                                                          
!		if( mod(nPdeCount,4)/=0 )then
		call Normalize( hNormParam, arrVars, 1 )
		if( .not. bSkipSave )then
			call AppendDataset()
			open( unit=hTrainFit, file=strTrainFit, status='unknown', position='append' )
			write( hTrainFit, *)rFitness + MB1 + MB2
			close(hTrainFit)
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
			rUmaMSE = rUmaMSE + test_net_cluster( hUmaNet, arrVars, arrVars(nInputs+1:nInputs+3), 1 )
			rCostMSE = rCostMSE + test_net_cluster( hCostNet, arrVarsTmp, arrVarsTmp(nInputs+1:nInputs+2), 1 )
			write( hGaNN, * )rUmaMSE, rCostMSE
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
!	if( bUmaNetOk )then	!if no NN is ready, needn't the sorting, use natural order.
	if( .false. )then	!if no NN is ready, needn't the sorting, use natural order.
		do i=1, sgaParam%nPopSize
			arrFitPool(i) = sgaParam%arrFit(i)
		enddo
		arrIndexTmp = (/(i, i=1,sgaParam%nPopSize)/)
		call qsort( arrIndexTmp, sgaParam%nPopSize, sizeof(arrIndexTmp(1)), FitCompare )

		do i=1, nCachedCount
			id = arrCachedIds(i)
			!decide if the cached result should be put into training set
			do j=1, sgaParam%nPopSize
				if( arrIndex(j)==id )exit;
			enddo
			do k=1, sgaParam%nPopSize
				if( arrIndexTmp(k)==id )exit;
			enddo
			if( j > k + sgaParam%nPopSize * 0.15 )then
				call DecodeOne( sgaParam, id )
				call PrepareFitness( id, sgaParam, MB1, MB2, arrVars )
				if( SearchChromCache( hChromCache, sgaParam%arrPop(id,:), sgaParam%nChromBits, arrFits(id,:), nFits )/=0 )then
					rMaxRdxC = max( 0.1, min( arrFits(id, 5), 25.d0 ) )
					rMaxTntC = max( 0.1, min( arrFits(id, 6), 75.d0 ) )
					rDummyVCGCost = max( 0.1, min( 1000.d0, arrFits(id, 7) ) )
					rFourMaxRdxC = max(0.1, min(arrFits(id,8), 25.d0) )
					rFourMaxTntC = max(0.1, min(arrFits(id,9), 75.d0) )
					arrVars(nInputs+3) = log10(rDummyVCGCost)
					arrVars(nInputs+4) = log10(rFourMaxRdxC)
					arrVars(nInputs+5) = log10(rFourMaxTntC)
					arrVars(nInputs+1) = rMaxRdxC / rFourMaxRdxC
					arrVars(nInputs+2) = rMaxTntC / rFourMaxTntC
					nNewPoints = nNewPoints + 1
				else
					call assert( .false. )
				endif
				print *, id, ' is appended ', sgaParam%arrFit(id)
				call Normalize( hNormParam, arrVars, 1 )
				call AppendDataset()
			endif
		enddo
	endif


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

	!do the retraining stuff		
!	do i=lbound(train_gen,1), ubound(train_gen, 1)                                                                                                                                                    
!		if( sgaParam%iCurGen==train_gen(i) )then                                                                                                                                                  
!			bRetrainNN = .true.                                                                                                                                                               
!		end if                                                                                                                                                                                    
!	enddo
	if( (nNewPoints>=nMaxNewPoints) .and. (sgaParam%iCurGen>=train_gen(1)) .and. &
			& sgaParam%iCurGen<sgaParam%nMaxGens )then
		bRetrainNN = .true.
	endif
           
	rNetBias = min( 1.5, dble(nDoPde)/dble(nInitSampling) * 1.5 )
	!retrain the neural network                                                                                                                                                                       
	if( bRetrainNN )then
		if( hUmaNet==0 )then
			hUmaNet = create_net_cluster()
			hCostNet = create_net_cluster()
		endif

		call train_net_cluster( hUmaNet, strUmaTrain, arrMeans, arrUmaNet, 3, (/0.d0,0.d0,0.d0,0.0d0/), nMinTrainPts )
		call train_net_cluster( hCostNet, strCostTrain, arrMeans, arrCostNet, 3, (/dble( (log10(2.1*1.05)+0.3)/1.7*1.8-0.9 ), dble( (log10(2.8*1.05)+0.3)/2.2*1.8-0.9 ), 1.2d0/), nMinTrainPts )
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
			call sim_net_cluster( hUmaNet, arrVars, arrVars(nInputs+1:nInputs+3) )
			call sim_net_cluster( hCostNet, arrVarsTmp, arrVarsTmp(nInputs+1:nInputs+2) )
!			print*, arrVars(29), arrVars(30), arrVars(31), arrVarsTmp(29), arrVarsTmp(30)
		end do
		close(121)
		close(122)                                                                                                                                                                               
                                                                                                                                                                                                          
		call SaveTrainInfo( rUmaMSE, rCostMSE )
	endif

	call ReleaseNormalize( hNormParam )

	close( hGaStat )
	close( hGaOut )
	close( hGaNN )
	close( hGaCount )
    
	!reload the host file                                                                                                                                                                                                       
	call RefreshHosts

	1111 format(/'#################  Generation',i5,'  #################')
contains
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


!this function assume the arrMax and arrMin hold the maximum and minimum value of 
!well locations and pumping rates, the subroutine doens't count the heads and risks
subroutine PrepareScallingRange( arrMin, arrMax, nCounts )
use umacase
implicit none
	!arguments 
	integer, intent(in) :: nCounts
	double precision, dimension(nCounts), intent(out) :: arrMin
	double precision, dimension(nCounts), intent(out) :: arrMax
	
	!variables 
	integer :: i, j

	!well locations (x,y) max and min
	do i=1, g_nNewWells
		arrMax(2*i-1) = g_parrNewWells(i)%parrLocs(1)%x
		arrMax(2*i) = g_parrNewWells(i)%parrLocs(1)%y
		arrMin(2*i-1) = arrMax(2*i-1)
		arrMin(2*i) = arrMax(2*i)

		do j=2, g_parrNewWells(i)%nNodes
			arrMax(2*i-1) = max( arrMax(2*i-1), dble(g_parrNewWells(i)%parrLocs(j)%x) )
			arrMax(2*i) = max( arrMax(2*i), dble(g_parrNewWells(i)%parrLocs(j)%y) )
			arrMin(2*i-1) = min( arrMin(2*i-1), dble(g_parrNewWells(i)%parrLocs(j)%x) )
			arrMin(2*i) = min( arrMin(2*i), dble(g_parrNewWells(i)%parrLocs(j)%y) )
		end do
	enddo

	!pumping rates
	arrMin(2*g_nNewWells+1:3*g_nNewWells+g_nOldWells) = (/(0.0, i=1, g_nNewWells), (0.0, i=1, g_nOldWells)/)
	arrMax(2*g_nNewWells+1:3*g_nNewWells+g_nOldWells) = (/(1.0, i=1, g_nNewExtWells), 6.0, 2.5, 2.2, (1.0, i=1, g_nOldExtWells), 5.0, 2.0, 3.1/)

	!max rdx c, max tnt c and dummy VCG cost
	arrMin(3*g_nNewWells+g_nOldWells+1:3*g_nNewWells+g_nOldWells+5) = (/0.5, 0.5, -1.0, -0.3, -0.3/)
	arrMax(3*g_nNewWells+g_nOldWells+1:3*g_nNewWells+g_nOldWells+5) = (/1.0, 1.0, 3.5, 1.4, 1.9/)
!	arrMin(3*g_nNewWells+g_nOldWells+1:3*g_nNewWells+g_nOldWells+4) = (/0.0, 0.0, 0.0, -1.0/)
!	arrMin(3*g_nNewWells+g_nOldWells+1:3*g_nNewWells+g_nOldWells+5) = (/-1.0, -1.0, -1.0, -1.0, -1.0/)
!	arrMax(3*g_nNewWells+g_nOldWells+1:3*g_nNewWells+g_nOldWells+5) = (/2.5, 2.5, 3.5, 2.5, 2.5/)
!	arrMax(3*g_nNewWells+g_nOldWells+1:3*g_nNewWells+g_nOldWells+4) = (/100.0, 100.0, 3.5, 1.0/)
!	arrMax(3*g_nNewWells+g_nOldWells+1:3*g_nNewWells+g_nOldWells+4) = (/100.0, 100.0, 25.0, 1.0/)
!	arrMin(3*g_nNewWells+g_nOldWells+1:3*g_nNewWells+g_nOldWells+3) = (/0.0, 0.0, 0.0/)
!	arrMax(3*g_nNewWells+g_nOldWells+1:3*g_nNewWells+g_nOldWells+3) = (/100.0, 100.0, 25.0/)
!	arrMin(3*g_nNewWells+g_nOldWells+1:3*g_nNewWells+g_nOldWells+3) = (/0.0, 0.0, 3.0/)
!	arrMax(3*g_nNewWells+g_nOldWells+1:3*g_nNewWells+g_nOldWells+3) = (/100.0, 100.0, 4.5/)
end subroutine PrepareScallingRange

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