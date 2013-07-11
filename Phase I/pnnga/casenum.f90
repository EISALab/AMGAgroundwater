module casenum                                                                                                                                                                                            
use STD_HELPER                                                                                                                                                                                            
implicit none                                                                                                                                                                                             
                                                                                                                                                                                                          
integer, parameter :: hSsInp	= 845
integer, parameter :: hRiskInp	= 842
integer, parameter :: hPenRisk	= 844
integer, parameter :: hPenHead	= 843
integer, parameter :: hCsgaOut	= 999
                                                                                                                                                                                                          
integer, parameter :: hGAOut	= 1000
integer, parameter :: hGaStat	= 1001
integer, parameter :: hGaNN		= 1002
integer, parameter :: hGaCount	= 1003
                                                                                                                                                                                                          
character(*), parameter :: strSsInp		= 'ssinp.dat'                                                                                                                                             
character(*), parameter :: strRiskInp	= 'riskinp.dat'                                                                                                                                                   
character(*), parameter :: strPenRisk	= 'penriskinp.dat'                                                                                                                                                
character(*), parameter :: strPenHead	= 'penheadinp.dat'                                                                                                                                                
character(*), parameter :: strCsgaOut	= 'newsga.out'                                                                                                                                                    
                                                                                                                                                                                                          
character(*), parameter :: strGaOut	= 'ga.out'  
character(*), parameter :: strGaStat = 'ga.stat'
character(*), parameter :: strGaNN = 'gann.out'
character(*), parameter :: strGaCount = 'gacount.out'

integer, dimension(3) :: arrHeadNet = (/9, 12, 3/)
integer, dimension(3) :: arrRiskNet = (/12, 15, 2/)
                                                                                                                                                                                                       
integer :: hChromCache
integer :: totalPDE
                                                                                                                                                                                                          
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
	
	                                                                                                                                                                                                          
contains                                                                                                                                                                                                  

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

!the logic of FitEvalNet is changed on 10/08/03 
! first the subroutine evaluate all fitnesses by Neural network
! then the subroutine randomly pick individuals to evaluate them by PDE.
! All the PDE results are saved into the cache.
! The statiscal file is ga.stat.
                                                                                                                                                                                                          
!using matlab to create neural network                                                                                                                                                                    
subroutine FitEvalNet(sgaParam)                                                                                                                                                                           
use sga                                                                                                                                                                                                   
use fmt_helper                                                                                                                                                                                            
use casewell                                                                                                                                                                                              
use neuroio                                                                                                                                                                                               
use condor_helper                                                                                                                                                                                         
use neuronet
use chmcache
use condorio
use scaling
use STD_HELPER
USE DFPORT                                                                                                                                                                                                
implicit none                                                                                                                                                                                             
	!argument                                                                                                                                                                                         
	type(SGAPARAMSTRUCT), intent(inout) :: sgaParam                                                                                                                                                   
	double precision, dimension(4*g_nRemWells+2) :: arrOrigMin, arrOrigMax			!(x,y), head, riskb, riskw.                                                                               
	double precision, dimension(4*g_nRemWells) :: arrHeadNormMin, arrHeadNormMax                                                                                                                      
	double precision, dimension(4*g_nRemWells+2) :: arrRiskNormMin, arrRiskNormMax                                                                                                                    
	double precision, dimension(4*g_nRemWells+2) :: arrVars							!the vars for parsing and normalize the NN parameters.

	double precision, dimension(sgaParam%nPopSize, g_nRemWells+2+1+2+1) :: arrFits	!save all the heads, two risk values, total cost, head and risk penalties and fitnesses into the chromosome cache
	double precision, dimension(sgaParam%nPopSize, 4*g_nRemWells ) :: arrVarBuf		!the generation inputs var cache, for calculating statics.                                                                         
	double precision, dimension( 4*g_nRemWells ) :: arrMeans, arrStds				!the means and deviation of the input buffer
	integer :: normHead, normRisk										!normalize parameters
	
                                                                                                                                                                                                          
	!variable                                                                                                                                                                                         
	integer :: i, j, k, id, hHeadNet, hRiskNet, nPdeCount                                                                                                                                                    
	logical :: bUseNN, bRetrainNN, bNNReady, bUpdateFit                                                                                                                                                         
	double precision :: rRiskB, rRiskW, rHeadMSE, rRiskMSE, rTmpMSE
	character(80) :: strHeadFile, strRiskFile                                                                                                                                                         
!	integer, dimension(6) :: train_gen = (/8, 10, 18, 30, 50, 80/)
!	integer, dimension(6) :: train_gen = (/5, 10, 18, 30, 50, 80/)
	integer, dimension(6) :: train_gen = (/3, 8, 15, 25, 40, 70/)
!	integer, dimension(6) :: train_gen = (/4, 8, 15, 25, 40, 70/)
!	integer, dimension(6) :: train_gen = (/2, 6, 12, 20, 35, 60/)
	double precision, save :: rProbPde = 0.2857
	integer :: nInitSampling = 20
	integer :: nFits, nDoPde

	!variables for adaptive sampling control
	integer, save :: nSampleBase, nNewPoints
	double precision, save :: rDiff, rSmoothStd, rSmoothStdDev, rSmoothHeadMSE, rSmoothHeadDev, rSmoothRiskMSE, rSmoothRiskDev
	double precision, save :: rStdBase, rHeadMSEBase, rRiskMSEBase
	double precision :: rAvgMean, rAvgStd, rSampRatio

	!variables for sorting
!	integer, dimension( sgaParam%nPopSize*2, sgaParam%nChromBits ) :: arrChromPool
	double precision, dimension( sgaParam%nPopSize ) :: arrFitPool
	integer, dimension( sgaParam%nPopSize ) :: arrIndex, arrIndexTmp
	integer, dimension( g_nRemWells ) :: arrWellIndex
		
	integer :: nMaxNewPoints
	integer :: arrJobIds(sgaParam%nPopSize), arrCachedIds(sgaParam%nPopSize)
	integer :: nJobCount, nCachedCount

	nJobCount = 0
	if( sgaParam%iCurGen==1 )then
		i = RemoveFileC( strGaOut )
		call MakeStaticFolder
	endif

	nFits = g_nRemWells + 2 + 1 + 2 + 1				!heads(nRemwells), 2 risks, 1 total cost, 2 penalties(head,risk), 1 fitness
                                                                                                                                                                                                          
	bUseNN = .false.
	bRetrainNN = .false.
	bNNReady = .false.
	nMaxNewPoints = 80
 

	arrOrigMin = 0
	arrOrigMax = 0
	arrHeadNormMin = (/(-1.0, i=1,3*g_nRemWells), (-0.9, i=1,g_nRemWells)/)                                                                                                                           
	arrHeadNormMax = (/(1.0, i=1,3*g_nRemWells), (0.9, i=1,g_nRemWells)/)                                                                                                                             
	arrRiskNormMin = (/(-1.0, i=1,4*g_nRemWells), -0.9, -0.9/)                                                                                                                                        
	arrRiskNormMax = (/(1.0, i=1,4*g_nRemWells), 0.9, 0.9/)                                                                                                                                           
                                                                                                                                                                                                          
	call PrepareScallingRange(arrOrigMin, arrOrigMax, 4*g_nRemWells+2)                                                                                                                                
	normHead = CreateNormalize( arrOrigMin, arrOrigMax, arrHeadNormMin, arrHeadNormMax, 4*g_nRemWells )                                                                                                      
	normRisk = CreateNormalize( arrOrigMin, arrOrigMax, arrRiskNormMin, arrRiskNormMax, 4*g_nRemWells+2 )                                                                                                      
                                                                                                                                                                                                          
	if( sgaParam%iCurGen==1 )then
		i=RemoveFileC( strGaOut )
		i=RemoveFileC( strGaStat )
		i=RemoveFileC( strHeadTrain )
		i=RemoveFileC( strHeadTest )
		i=RemoveFileC( strRiskTrain )
		i=RemoveFileC( strRiskTest )
		i=RemoveFileC( strHeadNN )
		i=RemoveFileC( strRiskNN )
		i=RemoveFileC( strGaNN )
		i=RemoveFileC( strGaCount )

		call OpenOutputFile( hGaCount, strGaCount )
		rewind( hGaCount )
		write(hGaCount, * ) 'Gen#	PDE#	CACHE#	NNET#'
		close( hGaCount)

		open( unit = hGaOut, file=strHeadTest, status='unknown' )
		rewind( hGaOut )
		close( hGaOut )
		open( unit = hGaOut, file=strRiskTest, status='unknown' )
		rewind( hGaOut )
		close( hGaOut )

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
	hHeadNet = CreateNet( arrHeadNet, 3 )
	hRiskNet = CreateNet( arrRiskNet, 3 )
!	call LoadNets( hHeadNet, hRiskNet )
	inquire( FILE=strHeadNN, EXIST=bUseNN )
	if( sgaParam%iCurGen==sgaParam%iStartGen+sgaParam%nMaxGens-1 )then
		bUseNN = .false.
	endif
	if( bUseNN )then
		!load neural net weights.
		call LoadMatlabNet( hHeadNet, strHeadNN )
		call LoadMatlabNet( hRiskNet, strRiskNN )
!		call LoadNetFromMatlab( hHeadNet, strHeadNN )
!		call LoadNetFromMatlab( hRiskNet, strRiskNN )
		bNNReady = .true.
	endif

	rHeadMSE = 0.0
	rRiskMSE = 0.0
	nPdeCount = 0
	sgaParam%arrFlags = -1
	!the first loop assumes using NN only
	if( bUseNN )then
		do i=1, sgaParam%nPopSize
			!order the wells by their number first
			arrWellIndex = (/(k, k=1, g_nRemWells)/)
			call qsort( arrWellIndex, g_nRemWells, sizeof(arrWellIndex(1)), WellCompare )

			call DecodeOne( sgaParam, i )
			call wrapPrepFunc( i, sgaParam%arrVars, sgaParam%nPopSize, sgaParam%nVarCount )
			!predict the heads                                                                                                                                                                
			call ParseVars( arrVars )
			call Normalize( normHead, arrVars, 1 )
			call OrderVars( arrVars, arrWellIndex )
			call SimNet( hHeadNet, arrVars, arrVars(3*g_nRemWells+1:4*g_nRemWells) )
			call UnOrderVars( arrVars, arrWellIndex )
			!parse the data back to global variables.                                                                                                                                         
			call UnNormalize( normHead, arrVars, 1 )                                                                                                                                          
			g_arrHeads = arrVars(3*g_nRemWells+1 : 4*g_nRemWells)                                                                                                                             
                                                                                                                                                                                                          
			!predict the risks                                                                                                                                                                
			call ParseVars( arrVars, g_arrHeads )                                                                                                                                             
			call Normalize( normRisk, arrVars, 1 )                                                                                                                                            
			call OrderVars( arrVars, arrWellIndex )
			call SimNet( hRiskNet, arrVars, arrVars(4*g_nRemWells+1:4*g_nRemWells+2) )                                                                                                        
			call UnOrderVars( arrVars, arrWellIndex )
			!parse the data back to global variables.                                                                                                                                         
			call UnNormalize( normRisk, arrVars, 1 )                                                                                                                                          
			rRiskB = 10**( -arrVars(4*g_nRemWells+1) )                                                                                                                                        
			rRiskW = 10**( -arrVars(4*g_nRemWells+2) )
                                                                                                                                                                                                          
			!calculate the cost by the output of NN                                                                                                                                           
			call CostByNN( g_arrHeads, g_arrOldHeads, g_nRemWells, rRiskB, rRiskW )                                                                                                           
                                                                                                                                                                                                          
			sgaParam%arrFit(i) = g_rFitness                                                                                                                                                           
			sgaParam%arrFlags(i) = NN_EVAL		!0 means a NN results.

			!save data for analysis and ouputs.
			arrFits(i, 1:g_nRemWells) = g_arrHeads
			arrFits(i, g_nRemWells+1) = g_rRiskB
			arrFits(i, g_nRemWells+2) = g_rRiskW
			arrFits(i, g_nRemWells+3) = g_rTotCost
			arrFits(i, g_nRemWells+4) = g_rPenHead
			arrFits(i, g_nRemWells+5) = g_rPenRisk
			arrFits(i, g_nRemWells+6) = g_rFitness

			!save the normalized variables in buffer for later statical analysis
			call ParseVars( arrVars, g_arrHeads, g_rRiskB, g_rRiskW )                                                                                                                                 
			call Normalize( normRisk, arrVars, 1 )
			arrVarBuf(i, :) = arrVars(1:4*g_nRemWells)

			call SaveNNInd( i )
		enddo
	endif

	!sort the chrosomes by the fitness values to pick the best individuals.
	!put the current generation into the pool to sort it with it last generation
	do i=1, sgaParam%nPopSize
		arrFitPool(i) = sgaParam%arrFit(i)
!		arrChromPool(i, :) = sgaParam%arrPop(i,:)
	enddo
	arrIndex = (/(i, i=1,sgaParam%nPopSize)/)

	if( bUseNN )then	!if no NN is ready, needn't the sorting, use natural order.
		call qsort( arrIndex, sgaParam%nPopSize, sizeof(arrIndex(1)), FitCompare )
!		rSampRatio = rSmoothStd / rStdBase
!		rSampRatio = 
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

	nCachedCount = 0
	do i = 1, sgaParam%nPopSize
		id = arrIndex(i)
		call DecodeOne( sgaParam, id )
		call wrapPrepFunc( id, sgaParam%arrVars, sgaParam%nPopSize, sgaParam%nVarCount )

		!either NN is not activated, or it is activated, but the individual is selected to evaluation for PDE.
!		if( (.not.bUseNN) .or. (UniRand()<0.1) )then
!			if( sgaParam%arrFlags(i)==2 )then	!the result is from the cache, needn't PDE results.
!				continue
!			end if

		bUpdateFit = .false.
!		if( (sgaParam%iCurGen==96) .or. (SearchChromCache( hChromCache, sgaParam%arrPop(id,:), sgaParam%nChromBits, arrFits(id,:), nFits )==0) )then
		if( SearchChromCache( hChromCache, sgaParam%arrPop(id,:), sgaParam%nChromBits, arrFits(id,:), nFits )==0 )then
			!not in cache, do the actual PDE.
!			if( (sgaParam%iCurGen==96).or.(nPdeCount<nDoPde) )then
			!sample according to the exponential distribution
!			if( nPdeCount<nDoPde )then
!				if( (.not.bUseNN) .or. ( UniRand()<exp(-1.0/nInitSampling*i) ) )then
!					arrJobIds( nJobCount+1 ) = id
!					nJobCount = nJobCount + 1
!					call BroadcastSlave( id, sgaParam%iCurGen )
!					nPdeCount = nPdeCount + 1
!				endif
!			endif
!			sgaParam%arrFlags(id) = -1
		else	!get it in the cache
			g_arrHeads = arrFits(id, 1:g_nRemWells)
			g_rRiskB = arrFits(id, g_nRemWells+1)
			g_rRiskW = arrFits(id, g_nRemWells+2)
			g_rFitness = arrFits(id, g_nRemWells+6)
			sgaParam%arrFlags(id) = CACHE_EVAL
			arrCachedIds( nCachedCount+1 ) = id
			nCachedCount = nCachedCount + 1

			bUpdateFit = .true.
		end if

		if( bUpdateFit )then
			sgaParam%arrFit(id) = g_rFitness

			!save the normalized variables in buffer for later statical analysis
			call ParseVars( arrVars, g_arrHeads, g_rRiskB, g_rRiskW )                                                                                                                                 
			call Normalize( normRisk, arrVars, 1 )
			arrVarBuf(id, :) = arrVars(1:4*g_nRemWells)
		end if
	end do

	if( bUseNN )then
		if( sgaParam%iCurGen>100 )then
			call ExpSelPdes( sgaParam, arrIndex, nInitSampling, nDoPde, arrJobIds, nJobCount)
		else
			call SelectPdes( sgaParam, 5, nDoPde, arrJobIds, nJobCount )
		endif
	else
		nJobCount = 0
		do i = 1, sgaParam%nPopSize
			if( sgaParam%arrFlags(i)/=CACHE_EVAL )then
				arrJobIds(nJobCount+1) = i
				nJobCount = nJobCount + 1
			endif
		enddo
	endif
	nPdeCount = nPdeCount + nJobCount
	do i=1, nJobCount
		id = arrJobIds(i)
		call DecodeOne( sgaParam, id )
		call wrapPrepFunc( id, sgaParam%arrVars, sgaParam%nPopSize, sgaParam%nVarCount )
		call BroadcastSlave( id, sgaParam%iCurGen )
	enddo
		

	!route the jobs
	if( nJobCount>0 )then 
		call WaitForSlaves( arrJobIds, nJobCount )
	endif

	!collect jobs
	do i=1, nJobCount
		id = arrJobIds(i)
		call DecodeOne( sgaParam, id )
		call wrapPrepFunc( id, sgaParam%arrVars, sgaParam%nPopSize, sgaParam%nVarCount )
		call CollectSlave( id, sgaParam%iCurGen, g_rFitness, g_rTotCost, g_rPenHead, g_rPenRisk, &
					& g_rRiskB, g_rRiskW, g_arrHeads, g_arrOldHeads, g_nRemWells )

		nNewPoints = nNewPoints + 1

		!order the wells by their number first
		arrWellIndex = (/(k, k=1, g_nRemWells)/)
		call qsort( arrWellIndex, g_nRemWells, sizeof(arrWellIndex(1)), WellCompare )

		!calculate the NN MSE on the pde
		if( bUseNN )then
			!calculate the neural network testing error                                                                                                                               
			call ParseVars( arrVars, g_arrHeads )
			call Normalize( normHead, arrVars, 1 )
			call OrderVars( arrVars, arrWellIndex )
			rHeadMSE = rHeadMSE + TestNet( hHeadNet, arrVars, arrVars(3*g_nRemWells+1:4*g_nRemWells), 1 )

			call ParseVars( arrVars, g_arrHeads, g_rRiskB, g_rRiskW )
			call Normalize( normRisk, arrVars, 1 )
			call SaveNNInd( id )
			call OrderVars( arrVars, arrWellIndex )
			rTmpMSE = TestNet( hRiskNet, arrVars, arrVars(4*g_nRemWells+1:4*g_nRemWells+2), 1 )
			rRiskMSE = rRiskMSE + rTmpMSE
			write( hGaNN, * )rTmpMSE
		endif

		!save data for analysis and ouputs.
		arrFits(id, 1:g_nRemWells) = g_arrHeads
		arrFits(id, g_nRemWells+1) = g_rRiskB
		arrFits(id, g_nRemWells+2) = g_rRiskW
		arrFits(id, g_nRemWells+3) = g_rTotCost
		arrFits(id, g_nRemWells+4) = g_rPenHead
		arrFits(id, g_nRemWells+5) = g_rPenRisk
		arrFits(id, g_nRemWells+6) = g_rFitness

		call InsertChromCache( hChromCache, sgaParam%arrPop(id,:), sgaParam%nChromBits, arrFits(id,:), nFits )

		!so no redundent chromosomes will be written into the training file.
!		if( UniRand()<0.999 )then                                                                                                                                                          
!		if( mod(nPdeCount,4)/=0 )then
		call AppendDataset( mod(nNewPoints,4)/=0 )

!		if( mod(nNewPoints,4)/=0 )then
!			strHeadFile = strHeadTrain                                                                                                                                                
!			strRiskFile = strRiskTrain                                                                                                                                                
!		else                                                                                                                                                                              
!			strHeadFile = strHeadTest                                                                                                                                                 
!			strRiskFile = strRiskTest                                                                                                                                                 
!		endif                                                                                                                                                                             
		!writting it to the training file
		!write the head training data into file                                                                                                                                           
!		call ParseVars( arrVars, g_arrHeads )                                                                                                                                             
!		call Normalize( normHead, arrVars, 1 )                                                                                                                                            
!		call OrderVars( arrVars, arrWellIndex )
!		call AppendToFile( strHeadFile, arrVars, 4*g_nRemWells )                                                                                                                          
                                                                                                                                                                                              
		!write the risk training data into file                                                                                                                                           
!		call ParseVars( arrVars, g_arrHeads, g_rRiskB, g_rRiskW )                                                                                                                         
!		call Normalize( normRisk, arrVars, 1 )                                                                                                                                           
!		call OrderVars( arrVars, arrWellIndex )
!		call AppendToFile( strRiskFile, arrVars, 4*g_nRemWells+2 )

		!save result to sgaParam
		sgaParam%arrFlags(id) = PDE_EVAL
		sgaParam%arrFit( id ) = g_rFitness

		!save the normalized variables in buffer for later statical analysis
		call ParseVars( arrVars, g_arrHeads, g_rRiskB, g_rRiskW )                                                                                                                                 
		call Normalize( normRisk, arrVars, 1 )
		arrVarBuf(id, :) = arrVars(1:4*g_nRemWells)
	end do

	!write the individual information to the out file
	do i = 1, sgaParam%nPopSize
		id = arrIndex(i)
		call DecodeOne( sgaParam, id )
		call wrapPrepFunc( id, sgaParam%arrVars, sgaParam%nPopSize, sgaParam%nVarCount )

		call SaveIndInfo( id, arrFits )
	end do

!resort the reevaluated fitness
!	if( bUseNN )then	!if no NN is ready, needn't the sorting, use natural order.
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
				call wrapPrepFunc( id, sgaParam%arrVars, sgaParam%nPopSize, sgaParam%nVarCount )
				if( SearchChromCache( hChromCache, sgaParam%arrPop(id,:), sgaParam%nChromBits, arrFits(id,:), nFits )/=0 )then
					g_arrHeads = arrFits(id, 1:g_nRemWells)
					g_rRiskB = arrFits(id, g_nRemWells+1)
					g_rRiskW = arrFits(id, g_nRemWells+2)
					g_rFitness = arrFits(id, g_nRemWells+6)
					nNewPoints = nNewPoints + 1
				else
					call assert( .false. )
				endif
				print *, id, ' is appended ', sgaParam%arrFit(id)
				call AppendDataset( .true. )
			endif
		enddo
	endif


	totalPDE = totalPDE + nPdeCount
	!save the statistical information.
	rHeadMSE = rHeadMSE / nPdeCount
	rRiskMSE = rRiskMSE / nPdeCount
	call CalcMeanStd( arrVarBuf, arrMeans, arrStds )
	call SaveGenInfo( arrMeans, arrStds, rHeadMSE, rRiskMSE, rAvgMean, rAvgStd )

	if( sgaParam%iCurGen==1 )then
		rStdBase = rAvgStd
		rSmoothStd = rAvgStd
		rSmoothHeadMSE = rHeadMSE
		rSmoothRiskMSE = rRiskMSE
		rSmoothHeadDev = 0
		rSmoothRiskDev = 0
	else
		!so jacon's algorithm smooth adding!
		rDiff = rAvgStd - rSmoothStd
		rSmoothStd = rSmoothStd + 0.5*(rDiff)
		rSmoothStdDev = rSmoothStdDev + 0.5*( abs(rDiff) - rSmoothStdDev )

		!jacabon'algorithm for the error
		rDiff = rHeadMSE - rSmoothHeadMSE
		rSmoothHeadMSE = rSmoothHeadMSE + 0.5*(rDiff)
		rSmoothHeadDev = rSmoothHeadDev + 0.5*(abs(rDiff)-rSmoothHeadDev)
		rSmoothHeadMSE = rSmoothHeadMSE + 4*rSmoothHeadDev

		!for head error
		rDiff = rRiskMSE - rSmoothRiskMSE
		rSmoothRiskMSE = rSmoothRiskMSE + 0.5*(rDiff)
		rSmoothRiskDev = rSmoothRiskDev + 0.5*(abs(rDiff)-rSmoothRiskDev)
		rSmoothRiskMSE = rSmoothRiskMSE + 4*rSmoothRiskDev
	end if

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

	!do the retraining stuff		
!	do i=lbound(train_gen,1), ubound(train_gen, 1)                                                                                                                                                    
!		if( sgaParam%iCurGen==train_gen(i) )then                                                                                                                                                  
!			bRetrainNN = .true.                                                                                                                                                               
!		end if                                                                                                                                                                                    
!	enddo
	if( (nNewPoints>=nMaxNewPoints) .and. (sgaParam%iCurGen>=train_gen(1)) .and. &
			& sgaParam%iCurGen/=sgaParam%nMaxGens )then
		bRetrainNN = .true.
	endif
               
	!retrain the neural network                                                                                                                                                                       
	if( bRetrainNN )then                                                                                                                                                                              
!		call system( 'tool -strip 225 headtrain.dat' );
!		call system( 'tool -strip 225 risktrain.dat' );
!		call system( 'tool -strip 75 headtest.dat' );
!		call system( 'tool -strip 75 risktest.dat' );
		rHeadMSE = TrainMatlabNetEx( strHeadM, arrHeadNet, 3, (/0.d0,0.d0,0.d0,0.0d0/), strHeadTrain, strHeadNN, strHeadTest )
		call ReleaseNet( hHeadNet )
		hHeadNet = CreateNet( arrHeadNet, 3 )
		call LoadMatlabNet( hHeadNet, strHeadNN )

		rRiskMSE = TrainMatlabNetEx( strRiskM, arrRiskNet, 3, (/-0.2454545d0,0.225d0,0.0d0/), strRiskTrain, strRiskNN, strRiskTest )
		call ReleaseNet( hRiskNet )
		hRiskNet = CreateNet( arrRiskNet, 3 )
		call LoadMatlabNet( hRiskNet, strRiskNN )

		rRiskMSEBase = rRiskMSE
		rHeadMSEBase = rHeadMSE
		rSmoothHeadMSE = rHeadMSE
		rSmoothRiskMSE = rRiskMSE
		nNewPoints = 0
!		rHeadMSE = TrainMatlabNet( strHeadM, strHeadTrain, strHeadNN )
!		rRiskMSE = TrainMatlabNet( strRiskM, strRiskTrain, strRiskNN )
                                                                                                                                                                                                          
!		rProbPde = rProbPde * 0.7                                                                                                                                                                 
                                                                                                                                                                                                          
!		call LoadNetFromMatlab( hHeadNet, strHeadNN )                                                                                                                                             
!		call LoadNetFromMatlab( hRiskNet, strRiskNN )                                                                                                                                             
                                                                                                                                                                                                          
!		open(unit=121, file=strHeadTrain, status='unknown')                                                                                                                                       
!		do i=1, 10                                                                                                                                                                                
!		call DecodeOne( sgaParam, i )
!		call wrapPrepFunc( i, sgaParam%arrVars, sgaParam%nPopSize, sgaParam%nVarCount )
!		arrWellIndex = (/(k, k=1, g_nRemWells)/)
!		call qsort( arrWellIndex, g_nRemWells, sizeof(arrWellIndex(1)), WellCompare )
!			read(121, *)( arrVars(k), k=1,12 )                                                                                                                                                
!			call SimNet( hHeadNet, arrVars, arrVars(3*g_nRemWells+1:4*g_nRemWells) )                                                                                                          
!			call UnOrderVars( arrVars, arrWellIndex )
			!parse the data back to global variables.                                                                                                                                         
!			call UnNormalize( normHead, arrVars, 1 )                                                                                                                                          
!			print*, arrVars(10), arrVars(11), arrVars(12)                                                                                                                                     
!		end do                                                                                                                                                                                    
!		close(121)                                                                                                                                                                                
                                                                                                                                                                                                          
!		open(unit=121, file=strRiskTrain, status='unknown')                                                                                                                                       
!		do i=1, 10                                                                                                                                                                                
!		call DecodeOne( sgaParam, i )
!		call wrapPrepFunc( i, sgaParam%arrVars, sgaParam%nPopSize, sgaParam%nVarCount )
!		arrWellIndex = (/(k, k=1, g_nRemWells)/)
!		call qsort( arrWellIndex, g_nRemWells, sizeof(arrWellIndex(1)), WellCompare )
!			read(121, *)( arrVars(k), k=1,14 )                                                                                                                                                
!			call SimNet( hRiskNet, arrVars, arrVars(4*g_nRemWells+1:4*g_nRemWells+2) )                                                                                                        
!			call UnOrderVars( arrVars, arrWellIndex )
!			call UnNormalize( normRisk, arrVars, 1 )                                                                                                                                          
!			arrVars(13) = 10**( -arrVars(4*g_nRemWells+1) )                                                                                                                                        
!			arrVars(14) = 10**( -arrVars(4*g_nRemWells+2) )
!			print*, arrVars(13), arrVars(14)                                                                                                                                                  
!		end do                                                                                                                                                                                    
!		close(121)     
		
		call SaveTrainInfo( rHeadMSE, rRiskMSE )                                                                                                                                                                           
	endif                                                                                                                                                                                             
                                                                                                                                                                                                          
	call ReleaseNet( hHeadNet )                                                                                                                                                                       
	call ReleaseNet( hRiskNet )                                                                                                                                                                       
	call ReleaseNormalize( normHead )
	call ReleaseNormalize( normRisk )

	close( hGaStat )
	close( hGaOut )
	close( hGaNN )
	close( hGaCount )
                                                                                                                                                                                                          
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

	integer(2) function WellCompare(a1, a2)
		integer :: a1, a2
		WellCompare = g_arrWellLocs(a1)-g_arrWellLocs(a2)
	end function

	subroutine AppendDataset( bTraining )
	implicit none
		!argument
		logical :: bTraining
		character(80) :: strHeadFile, strRiskFile

		if( bTraining )then
			strHeadFile = strHeadTrain
			strRiskFile = strRiskTrain
		else                                                                                                                                                                              
			strHeadFile = strHeadTest
			strRiskFile = strRiskTest
		endif                                                                                                                                                                             
		!writting it to the training file
		!write the head training data into file                                                                                                                                           
		call ParseVars( arrVars, g_arrHeads )                                                                                                                                             
		call Normalize( normHead, arrVars, 1 )                                                                                                                                            
		call OrderVars( arrVars, arrWellIndex )
		call AppendToFile( strHeadFile, arrVars, 4*g_nRemWells )                                                                                                                          
                                                                                                                                                                                              
		!write the risk training data into file                                                                                                                                           
		call ParseVars( arrVars, g_arrHeads, g_rRiskB, g_rRiskW )                                                                                                                         
		call Normalize( normRisk, arrVars, 1 )                                                                                                                                           
		call OrderVars( arrVars, arrWellIndex )
		call AppendToFile( strRiskFile, arrVars, 4*g_nRemWells+2 )

		!writting it to the training file or testing file
!		call AppendToFile( strUmaFile, arrVars, nInputs+3 )
!		arrVarsTmp = (/arrVars(1:nInputs), arrVars(nInputs+4), arrVars(nInputs+5), 0.d0, 0.d0/)
!		call AppendToFile( strCostFile, arrVarsTmp, nInputs+2 )
	end subroutine AppendDataset

	subroutine SaveNNInd( id )
	use sga
	use fmt_helper
	use casewell
	implicit none
		!arguments
		integer, intent(in) :: id

!		do i=1, sgaParam%nPopSize
!			id = arrIndex(i)
!			write( hGaNN, fmt = 1000 )id, arrVarBuf(id, :)
!		enddo
		write( hGaNN, fmt = 1000 )id, arrVars
			
		1000 format(i4,2x,14(f17.4,2x))
	end subroutine

	subroutine SaveIndInfo(id, arrFits)                                                                                                                                                                
	use sga                                                                                                                                                                                           
	use fmt_helper                                                                                                                                                                                    
	use casewell                                                                                                                                                                                      
	implicit none                                                                                                                                                                                     
		!arguments                                                                                                                                                                                
		integer, intent(in) :: id
		double precision, dimension(:,:) :: arrFits	!heads(nRemwells), 2 risks, 1 total cost, 2 penalties(head,risk), 1 fitness                                                                                                                                                                                                          
		integer :: j                                                                                                                                                                              
                                                                                                                                                                                                          
		if( sgaParam%arrFlags(id)==NN_EVAL )then		!neural network
			write( hGaOut, fmt=1086, advance="NO" ) id, arrFits(id, g_nRemWells+6), arrFits(id, g_nRemWells+3), arrFits(id, g_nRemWells+4), arrFits(id, g_nRemWells+5), arrFits(id, g_nRemWells+1), arrFits(id, g_nRemWells+2)
		else if( sgaParam%arrFlags(id)==CACHE_EVAL )then	!from cache
			write( hGaOut, fmt=1087, advance="NO" ) id, arrFits(id, g_nRemWells+6), arrFits(id, g_nRemWells+3), arrFits(id, g_nRemWells+4), arrFits(id, g_nRemWells+5), arrFits(id, g_nRemWells+1), arrFits(id, g_nRemWells+2)
		else
			call assert( sgaParam%arrFlags(id)==PDE_EVAL )
			write( hGaOut, fmt=1088, advance="NO" ) id, arrFits(id, g_nRemWells+6), arrFits(id, g_nRemWells+3), arrFits(id, g_nRemWells+4), arrFits(id, g_nRemWells+5), arrFits(id, g_nRemWells+1), arrFits(id, g_nRemWells+2)
			!write( hGaOut, fmt=1076, advance="NO" ) i, sgaParam%arrFit(i), g_rTotCost, g_rPenHead, g_rPenRisk, g_rRiskB, g_rRiskW
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
			!write( hGaOut, fmt=1077, advance="NO" )g_arrHeads(j)                                                                                                                              
			write( hGaOut, fmt=1077, advance="NO" )arrFits(id, j)
		enddo                                                                                                                                                                                     
		!well pumping flags and installation flags                                                                                                                                                
		do j=2*g_nRemWells+1, sgaParam%nVarCount                                                                                                                                                  
			write( hGaOut, fmt=1079, advance="NO" )anint(sgaParam%arrVars(id,j))                                                                                                              
		enddo                                                                                                                                                                                     
		!just print the catridge                                                                                                                                                                  
		write(hGaOut, fmt=1081, advance="YES")                                                                                                                                                    
                                                                                                                                                                                                          
		1077 format(2x,f6.1)                                                                                                                                                                      
		1078 format(2x,i4)                                                                                                                                                                        
		1079 format(2x,f2.0)                                                                                                                                                                      
		1080 format(2x,f7.3)                                                                                                                                                                      
		1081 format(1x)                                                                                                                                                                           
		1086 format('-',i4,2x,2(f17.4,2x),4(f20.10,2x))                                                                                                                                           
		1087 format('*',i4,2x,2(f17.4,2x),4(f20.10,2x))                                                                                                                                           
		1088 format(' ',i4,2x,2(f17.4,2x),4(f20.10,2x))                                                                                                                                               
	end subroutine SaveIndInfo                                                                                                                                                                        
                                                                                                                                                                                                          
	subroutine SaveGenInfo( arrMeans, arrStds, rHeadMSE, rRiskMSE, rAvgMean, rAvgStd )
	implicit none                                                                                                                                                                                     
		double precision, dimension(:) :: arrMeans, arrStds
		double precision :: rHeadMSE, rRiskMSE
		double precision :: rAvgMean, rAvgStd

		integer :: i, dim

		integer :: nPdeNum, nNetNum, nCacheNum

		dim = ubound(arrMeans,1) - lbound(arrMeans,1) + 1
		write( hGaOut, * )'generation information'                                                                                                                                                
		write( hGaOut, 1500 ) ( arrMeans(i), i=1, 4*g_nRemWells )
		write( hGaOut, 1501 ) ( arrStds(i), i=1, 4*g_nRemWells )

		write( hGaStat, 999 )sgaParam%iCurGen
		write( hGaStat, 1500 ) ( arrMeans(i), i=1, 4*g_nRemWells )
		write( hGaStat, 1501 ) ( arrStds(i), i=1, 4*g_nRemWells )

		rAvgMean = sum(arrMeans)
		rAvgMean = rAvgMean / dim

		rAvgStd = sum( arrStds )
		rAvgStd = rAvgStd / dim
		write( hGaOut, 1002 ) rAvgMean, rAvgStd
		write( hGaOut, 1001 ) rHeadMSE, rRiskMSE
		write( hGaOut, 1003 ) nNewPoints, nPdeCount, rStdBase, rSmoothStd, rSmoothStdDev
		write( hGaOut, 1004 ) rHeadMSEBase, rSmoothHeadMSE, rSmoothHeadDev
		write( hGaOut, 1004 ) rRiskMSEBase, rSmoothRiskMSE, rSmoothRiskDev

		write( hGaStat, 1002 ) rAvgMean, rAvgStd
		write( hGaStat, 1001 ) rHeadMSE, rRiskMSE
		write( hGaStat, 1003 ) nNewPoints, nPdeCount, rStdBase, rSmoothStd, rSmoothStdDev
		write( hGaStat, 1004 ) rHeadMSEBase, rSmoothHeadMSE, rSmoothHeadDev
		write( hGaStat, 1004 ) rRiskMSEBase, rSmoothRiskMSE, rSmoothRiskDev

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
		1500 format('Avg Mean:', 12(f9.6,2x))                                                                                                                                                                  
		1501 format('Avg Std:', 12(f9.6,2x))                                                                                                                                                                  
		1001 format('MSE of head and risk=', f10.8, 2x, f10.8)
		1002 format( f10.8, 2x, f10.8 )
		1003 format( i5, 2x, i5, 2x, f10.6, 2x, f10.6, 2x, f10.6 )
		1004 format( f10.6, 2x, f10.6, 2x, f10.6 )
	end subroutine                                                                                                                                                                                    
                                                                                                                                                                                                          
	subroutine SaveTrainInfo( rHeadMSE, rRiskMSE )                                                                                                                                   
	implicit none                                                                                                                                                                                     
		double precision :: rHeadMSE, rRiskMSE                                                                                                                                                    
                                                                                                                                                                                                          
		write( hGaOut, * )'Neural network is trained, information'                                                                                                                                                
		write( hGaOut, 1001 ) rHeadMSE, rRiskMSE                                                                                                                                                  
                                                                                                                                                                                                          
		1001 format(f10.8, 2x, f10.8)                                                                                                                                                             
	end subroutine                                                                                                                                                                                    
                                                                                                                                                                                                          
	!the subroutine put the locations(x,y), Q, Heads, riskb and riskw into array arrVars.                                                                                                             
	!arrHeads, rRiskB, rRiskW are optional parameters. They are put into arrVars only when they are present.                                                                                          
	subroutine ParseVars( arrVars, arrHeads, rRiskB, rRiskW )                                                                                                                                         
	implicit none                                                                                                                                                                                     
		!arguments                                                                                                                                                                                
		double precision, dimension(:) :: arrVars                                                                                                                                                 
		double precision, dimension(:), optional, intent(in) :: arrHeads                                                                                                                          
		double precision, optional, intent(in) :: rRiskB, rRiskW                                                                                                                                  
                                                                                                                                                                                                          
		!variables                                                                                                                                                                                
		integer :: i                                                                                                                                                                              
		double precision :: rB, rW                                                                                                                                                                
                                                                                                                                                                                                          
		do i=1, g_nRemWells                                                                                                                                                                       
			arrVars(2*i-1) = g_parrWellInfos(i)%parrLocs(g_arrWellLocs(i))%x                                                                                                                  
			arrVars(2*i) = g_parrWellInfos(i)%parrLocs(g_arrWellLocs(i))%y                                                                                                                    
		enddo                                                                                                                                                                                     
                                                                                                                                                                                                          
		do i=1, g_nRemWells                                                                                                                                                                       
			arrVars(2*g_nRemWells+i)= g_arrPumpRates(i)                                                                                                                                       
		enddo                                                                                                                                                                                     
                                                                                                                                                                                                          
		if( present(arrHeads) )then                                                                                                                                                               
			do i=1, g_nRemWells                                                                                                                                                               
				arrVars(3*g_nRemWells+i) =arrHeads(i)                                                                                                                                     
			enddo                                                                                                                                                                             
		endif                                                                                                                                                                                     
                                                                                                                                                                                                          
		if( present(rRiskB ) ) then                                                                                                                                                               
			rB = rRiskB                                                                                                                                                                       
			rW = rRiskW                                                                                                                                                                       
			if( rRiskB <=1e-12 )rB = 1e-12                                                                                                                                                    
			if( rRiskW <=1e-8 )rW = 1e-8                                                                                                                                                      
			arrVars(4*g_nRemWells+1) = -log10(rB)                                                                                                                                             
			arrVars(4*g_nRemWells+2) = -log10(rW)                                                                                                                                             
		endif                                                                                                                                                                                     
	end subroutine

	subroutine OrderVars( arrVars, arrWellIndex )
	implicit none
		!arguments
		double precision, dimension(:) :: arrVars
		integer, dimension(:) :: arrWellIndex

		double precision, dimension( ubound(arrVars,1)-lbound(arrVars,1)+1 ) :: arrBuf
		integer :: i, id

		arrBuf = arrVars
		do i=1, g_nRemWells
			id = arrWellIndex(i)
			arrBuf( 2*i-1 ) = arrVars(2*id-1)
			arrBuf( 2*i ) = arrVars(2*id)
		enddo
                                                                                                                                                                                                          
		do i=1, g_nRemWells
			id = arrWellIndex(i)
			arrBuf(2*g_nRemWells+i) = arrVars(2*g_nRemWells+id)
		enddo
                                                                                                                                                                                                          
		do i=1, g_nRemWells
			id = arrWellIndex(i)
			arrBuf(3*g_nRemWells+i) = arrVars(3*g_nRemWells+id)
		enddo

		arrVars = arrBuf
	end subroutine
                                                                                                                                                                                                          
	subroutine UnOrderVars( arrVars, arrWellIndex )
	implicit none
		!arguments
		double precision, dimension(:) :: arrVars
		integer, dimension(:) :: arrWellIndex

		double precision, dimension( ubound(arrVars,1)-lbound(arrVars,1)+1 ) :: arrBuf
		integer :: i, id

		arrBuf = arrVars
		do i=1, g_nRemWells
			id = arrWellIndex(i)
			arrBuf( 2*id-1 ) = arrVars(2*i-1)
			arrBuf( 2*id ) = arrVars(2*i)
		enddo
                                                                                                                                                                                                          
		do i=1, g_nRemWells
			id = arrWellIndex(i)
			arrBuf(2*g_nRemWells+id) = arrVars(2*g_nRemWells+i)
		enddo
                                                                                                                                                                                                          
		do i=1, g_nRemWells
			id = arrWellIndex(i)
			arrBuf(3*g_nRemWells+id) = arrVars(3*g_nRemWells+i)
		enddo

		arrVars = arrBuf
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
                                                                                                                                                                                                          
                                                                                                                                                                                                          
subroutine FitEvalNN(sgaParam)                                                                                                                                                                            
use sga                                                                                                                                                                                                   
use fmt_helper                                                                                                                                                                                            
use casewell                                                                                                                                                                                              
use neuroio                                                                                                                                                                                               
use condor_helper                                                                                                                                                                                         
USE DFPORT                                                                                                                                                                                                
implicit none                                                                                                                                                                                             
	!argument                                                                                                                                                                                         
	type(SGAPARAMSTRUCT), intent(inout) :: sgaParam                                                                                                                                                   
	double precision, dimension(4*g_nRemWells+2) :: arrMin, arrMax                                                                                                                                    
                                                                                                                                                                                                          
	!variable                                                                                                                                                                                         
	integer :: i, k                                                                                                                                                                                   
	logical :: bUseNN, bRetrainNN                                                                                                                                                                     
	double precision :: rRiskB, rRiskW                                                                                                                                                                
                                                                                                                                                                                                          
	bUseNN = .false.                                                                                                                                                                                  
	bRetrainNN = .false.                                                                                                                                                                              
                                                                                                                                                                                                          
	!assume dynaflag is always .false.                                                                                                                                                                
                                                                                                                                                                                                          
	arrMin = 0                                                                                                                                                                                        
	arrMax = 0                                                                                                                                                                                        
	call PrepareScallingRange(arrMin, arrMax, 4*g_nRemWells+2)                                                                                                                                        
                                                                                                                                                                                                          
	if( sgaParam%iCurGen==1 )then                                                                                                                                                                     
		i=RemoveFileC( strGaOut )                                                                                                                                                                 
		i=RemoveFileC( strHeadTrain )                                                                                                                                                             
		i=RemoveFileC( strHeadTest )                                                                                                                                                              
		i=RemoveFileC( strRiskTrain )                                                                                                                                                             
		i=RemoveFileC( strRiskTest )                                                                                                                                                              
		i=RemoveFileC( strHeadNN )                                                                                                                                                                
		i=RemoveFileC( strRiskNN )                                                                                                                                                                
                                                                                                                                                                                                          
		call WriteHeadScaleParam( strHeadScale, arrMin, arrMax )                                                                                                                                  
		call WriteRiskScaleParam( strRiskScale, arrMin, arrMax )                                                                                                                                  
	endif                                                                                                                                                                                             
                                                                                                                                                                                                          
	open(unit=hGaOut, file=strGaOut, status='unknown', position='append' )                                                                                                                            
                                                                                                                                                                                                          
	write( hGaOut, 1111 ) sgaParam%iCurGen                                                                                                                                                            
		                                                                                                                                                                                          
	do i = 1, sgaParam%nPopSize                                                                                                                                                                       
		call DecodeOne( sgaParam, i )                                                                                                                                                             
		call wrapPrepFunc( i, sgaParam%arrVars, sgaParam%nPopSize, sgaParam%nVarCount )                                                                                                           
                                                                                                                                                                                                          
		inquire( FILE=strHeadNN, EXIST=bUseNN )                                                                                                                                                   
                                                                                                                                                                                                          
		if( bUseNN )then                                                                                                                                                                          
			!10% percent still use numerical model                                                                                                                                            
			if( UniRand()<0.1 )bUseNN=.false.                                                                                                                                                 
		endif                                                                                                                                                                                     
                                                                                                                                                                                                          
		if( sgaParam%iCurGen>=95 )then                                                                                                                                                            
			bUseNN = .false.                                                                                                                                                                  
		endif                                                                                                                                                                                     
                                                                                                                                                                                                          
		if( .not. bUseNN )then                                                                                                                                                                    
			call CostWrapper                                                                                                                                                                  
		else                                                                                                                                                                                      
			!write data to data file so that nn can make a prediction                                                                                                                         
			call WriteForHeadNN( strHeadPred, g_arrWellLocs, g_arrPumpRates )                                                                                                                 
			call system( strDoPredHead )                                                                                                                                                      
			print*, 'reading NN Head'                                                                                                                                                         
			call ReadHeadNNPred( arrMin, arrMax, g_nRemWells, g_arrHeads )                                                                                                                    
			print*, 'after reading NN head'                                                                                                                                                   
                                                                                                                                                                                                          
			print*, 'writing for riskNN'                                                                                                                                                      
			call WriteForRiskNN( strRiskPred, g_arrWellLocs, g_arrPumpRates, g_arrHeads )                                                                                                     
			print*, 'after writting for riskNN'                                                                                                                                               
			call system( strDoPredRisk )                                                                                                                                                      
			call ReadRiskNNPred( arrMin, arrMax, g_nRemWells, rRiskB, rRiskW )                                                                                                                
                                                                                                                                                                                                          
			!read nn outputs                                                                                                                                                                  
			!call ReadNNPred( arrMin, arrMax, g_nRemWells, g_arrHeads, rRiskB, rRiskW )                                                                                                       
			!calculate the cost by the output of NN                                                                                                                                           
			call CostByNN( g_arrHeads, g_arrOldHeads, g_nRemWells, rRiskB, rRiskW )                                                                                                           
                                                                                                                                                                                                          
			!call system('scale.bat')                                                                                                                                                         
			!call system('predhead.bat')                                                                                                                                                      
			!call CostWrapper                                                                                                                                                                 
		endif                                                                                                                                                                                     
                                                                                                                                                                                                          
		sgaParam%arrFit(i) = g_rFitness                                                                                                                                                           
                                                                                                                                                                                                          
		call SaveIndInfo(i, bUseNN)                                                                                                                                                               
                                                                                                                                                                                                          
		if( .not. bUseNN ) then                                                                                                                                                                   
			if( UniRand()<0.85 )then                                                                                                                                                          
				call WriteForHeadNN( strHeadTrain, g_arrWellLocs, g_arrPumpRates, g_arrHeads )                                                                                            
				call WriteForRiskNN( strRiskTrain, g_arrWellLocs, g_arrPumpRates, g_arrheads, g_rRiskB, g_rRiskW )                                                                        
			else                                                                                                                                                                              
				call WriteForHeadNN( strHeadTest, g_arrWellLocs, g_arrPumpRates, g_arrHeads )                                                                                             
				call WriteForRiskNN( strRiskTest, g_arrWellLocs, g_arrPumpRates, g_arrheads, g_rRiskB, g_rRiskW )                                                                         
			endif                                                                                                                                                                             
		endif                                                                                                                                                                                     
	end do                                                                                                                                                                                            
                                                                                                                                                                                                          
	if( sgaParam%iCurGen==8 )then                                                                                                                                                                     
		bRetrainNN = .true.                                                                                                                                                                       
	else if( (sgaParam%iCurGen>8) .and. mod(sgaParam.iCurGen, 8)==0 ) then                                                                                                                            
		bRetrainNN = .true.                                                                                                                                                                       
	endif                                                                                                                                                                                             
                                                                                                                                                                                                          
	if( bRetrainNN )then                                                                                                                                                                              
		call system(strDoScaleBat)                                                                                                                                                                
		call system(strDoTrainBat)                                                                                                                                                                
		!call system('scale.bat')                                                                                                                                                                 
		!call system('trainhead.bat')                                                                                                                                                             
	endif                                                                                                                                                                                             
                                                                                                                                                                                                          
	close( hGaOut )                                                                                                                                                                                   
                                                                                                                                                                                                          
	1111 format(/'#################  Generation',i5,'  #################')                                                                                                                            
contains                                                                                                                                                                                                  
	subroutine SaveIndInfo(id, bUseNN)                                                                                                                                                                
	use sga                                                                                                                                                                                           
	use fmt_helper                                                                                                                                                                                    
	use casewell                                                                                                                                                                                      
	implicit none                                                                                                                                                                                     
		!arguments                                                                                                                                                                                
		integer, intent(in) :: id                                                                                                                                                                 
		logical, intent(in) :: bUseNN                                                                                                                                                             
                                                                                                                                                                                                          
		integer :: j                                                                                                                                                                              
                                                                                                                                                                                                          
		if( bUseNN )then                                                                                                                                                                          
			write( hGaOut, fmt=1086, advance="NO" ) i, sgaParam%arrFit(i), g_rTotCost, g_rPenHead, g_rPenRisk, g_rRiskB, g_rRiskW                                                             
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
		do j=2*g_nRemWells+1, sgaParam%nVarCount                                                                                                                                                  
			write( hGaOut, fmt=1079, advance="NO" )anint(sgaParam%arrVars(id,j))                                                                                                              
		enddo                                                                                                                                                                                     
		!just print the catridge                                                                                                                                                                  
		write(hGaOut, fmt=1081, advance="YES")                                                                                                                                                    
                                                                                                                                                                                                          
		1076 format(i4,2x,2(f17.4,2x),4(f20.10,2x))                                                                                                                                               
		1077 format(2x,f6.1)                                                                                                                                                                      
		1078 format(2x,i4)                                                                                                                                                                        
		1079 format(2x,f2.0)                                                                                                                                                                      
		1080 format(2x,f7.3)                                                                                                                                                                      
		1081 format(1x)                                                                                                                                                                           
		1086 format('-',i4,2x,2(f17.4,2x),4(f20.10,2x))                                                                                                                                           
	end subroutine SaveIndInfo                                                                                                                                                                        
                                                                                                                                                                                                          
end subroutine FitEvalNN                                                                                                                                                                                  
                                                                                                                                                                                                          
subroutine FitEval(sgaParam)                                                                                                                                                                              
use sga                                                                                                                                                                                                   
use fmt_helper                                                                                                                                                                                            
use casewell                                                                                                                                                                                              
use neuroio                                                                                                                                                                                               
use condor_helper                                                                                                                                                                                         
USE DFPORT                                                                                                                                                                                                
implicit none                                                                                                                                                                                             
	!argument                                                                                                                                                                                         
	type(SGAPARAMSTRUCT), intent(inout) :: sgaParam                                                                                                                                                   
                                                                                                                                                                                                          
	!variable                                                                                                                                                                                         
	integer :: i, k                                                                                                                                                                                   
	logical :: bSavePDE                                                                                                                                                                               
                                                                                                                                                                                                          
	!assume dynaflag is always .false.                                                                                                                                                                
                                                                                                                                                                                                          
	if( sgaParam%iCurGen==1 )then                                                                                                                                                                     
		i = RemoveFileC( strGaOut )                                                                                                                                                               
	endif                                                                                                                                                                                             
                                                                                                                                                                                                          
	open(unit=hGaOut, file=strGaOut, status='unknown', position='append' )                                                                                                                            
                                                                                                                                                                                                          
	write( hGaOut, 1111 ) sgaParam%iCurGen                                                                                                                                                            
		                                                                                                                                                                                          
	do i = 1, sgaParam%nPopSize                                                                                                                                                                       
		call DecodeOne( sgaParam, i )                                                                                                                                                             
		call wrapPrepFunc( i, sgaParam%arrVars, sgaParam%nPopSize, sgaParam%nVarCount )                                                                                                           
                                                                                                                                                                                                          
		bSavePDE = .true.                                                                                                                                                                         
!		if( SearchChromCache( sgaParam%arrPop(i,:), sgaParam%nChromBits, g_rFitness)==0 )then                                                                                                     
!			call CostWrapper                                                                                                                                                                  
!			call InsertChromCache( sgaParam%arrPop(i,:), sgaParam%nChromBits, g_rFitness )                                                                                                    
!			bSavePDE = .false.                                                                                                                                                                
!		end if                                                                                                                                                                                    
		sgaParam%arrFit(i) = g_rFitness                                                                                                                                                           
                                                                                                                                                                                                          
		call SaveIndInfo(i)                                                                                                                                                                       
                                                                                                                                                                                                          
	end do                                                                                                                                                                                            
                                                                                                                                                                                                          
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
                                                                                                                                                                                                          
		if( bSavePDE )then                                                                                                                                                                        
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
                                                                                                                                                                                                          
!the original code is riskparamset                                                                                                                                                                        
!the origianl parameters nsgaflag and rand is removed                                                                                                                                                     
subroutine GenRiskParam(riskdists,adultin,childin,exptime,bodywt,inrate,showertime,timeinbath,ratiohouse,ratioair)                                                                                        
use casewell                                                                                                                                                                                              
IMPLICIT NONE                                                                                                                                                                                             
	!argument                                                                                                                                                                                         
	double precision, intent(in) :: riskdists(g_nRiskMat2,g_nRiskMat1)                                                                                                                                
	double precision, intent(out)::  adultin, childin, exptime, bodywt, inrate, showertime, timeinbath, ratiohouse, ratioair                                                                          
                                                                                                                                                                                                          
	!variables                                                                                                                                                                                        
	integer nsgaflag                                                                                                                                                                                  
	double precision :: rand                                                                                                                                                                          
                                                                                                                                                                                                          
	nsgaflag = 1                                                                                                                                                                                      
	!generating the sample for each variable exposure/risk parameter                                                                                                                                  
	if(nsgaflag.eq.1) then                                                                                                                                                                            
		adultin = exp(riskdists(1,1))                                                                                                                                                             
		childin = exp(riskdists(2,1))                                                                                                                                                             
		exptime = exp(riskdists(3,1))                                                                                                                                                             
		bodywt = exp(riskdists(4,1))                                                                                                                                                              
		inrate = exp(riskdists(5,1))                                                                                                                                                              
		showertime = exp(riskdists(6,1))                                                                                                                                                          
		timeinbath = exp(riskdists(7,1))                                                                                                                                                          
		ratiohouse = exp(riskdists(8,1))                                                                                                                                                          
		ratioair = exp(riskdists(9,1))                                                                                                                                                            
	else                                                                                                                                                                                              
		call random_number(rand)                                                                                                                                                                  
		adultin = exp(rand*riskdists(1,2)+riskdists(1,1))                                                                                                                                         
		call random_number(rand)                                                                                                                                                                  
		childin = exp(rand*riskdists(2,2)+riskdists(2,1))                                                                                                                                         
		call random_number(rand)                                                                                                                                                                  
		exptime = exp(rand * riskdists(3,2) +riskdists(3,1))                                                                                                                                      
		call random_number(rand)                                                                                                                                                                  
		bodywt = exp (rand * riskdists(4,2) + riskdists(4,1))                                                                                                                                     
		call random_number(rand)                                                                                                                                                                  
		inrate = exp(rand * riskdists(5,2) + riskdists(5,1))                                                                                                                                      
		call random_number(rand)                                                                                                                                                                  
		showertime = exp(rand * riskdists(6,2) + riskdists(6,1))                                                                                                                                  
		call random_number(rand)                                                                                                                                                                  
		timeinbath = exp(rand * riskdists(7,2) + riskdists(7,1))                                                                                                                                  
		call random_number(rand)                                                                                                                                                                  
		ratiohouse = exp(rand * riskdists(8,2) + riskdists(8,1))                                                                                                                                  
		call random_number(rand)                                                                                                                                                                  
		ratioair = exp (rand*riskdists(9,2) + riskdists(9,1))                                                                                                                                     
	endif                                                                                                                                                                                             
! This is the end of the section on generation of the risk and exposure parameters                                                                                                                        
end subroutine GenRiskParam                                                                                                                                                                               
                                                                                                                                                                                                          
 ! ########################################################################################################################################                                                               
                                                                                                                                                                                                          
 ! This file is used to determine the concentration of the steady state at a point downgradient.                                                                                                          
 ! The subroutine uses an analytical equation from Domenico & Schwartz [1990] and uses the source concentration                                                                                           
 ! values obtained from the numerical model.                                                                                                                                                              
                                                                                                                                                                                                          
! The source code for this file is taken from the code by Bryan Smalley                                                                                                                                   
! Written by Gayathri Gopalakrishnan                                                                                                                                                                      
                                                                                                                                                                                                          
! The variables defined here                                                                                                                                                                              
! maxconc[monwellsets] = the array of the maximum concentration over the well sets                                                                                                                        
! ssconc [monwellsets] = the steady state concentration over the well sets                                                                                                                                
! longdisp = longitudinal dispersivity                                                                                                                                                                    
! longdisp1 = longitudinal dispersivity in cm                                                                                                                                                             
! transdisp = transverse dispersivity                                                                                                                                                                     
! transdisp1 = transverse dispersivity in cm                                                                                                                                                              
! velocity = mean velocity in x direction of the contaminant                                                                                                                                              
! RFs = retardation factor for the velocity                                                                                                                                                               
! ssconcw = the maximum steady state concentration over all the wells except the boundary wells                                                                                                           
! ssconcb = the maximum steady state concentration over the wells on the boundary                                                                                                                         
! L= First order degradation constant [1/day]                                                                                                                                                             
! Sw= the width of the source area contaminant plume [m]                                                                                                                                                  
                                                                                                                                                                                                          
subroutine ssanal(ssconcw,ssconcb,longdisp,transdisp,velocity,Rfs,maxconc,monwellsets)                                                                                                                    
 use numerical_libraries                                                                                                                                                                                  
 IMPLICIT NONE                                                                                                                                                                                            
 integer  :: monwellsets                                                                                                                                                                                  
 double precision,dimension(monwellsets),intent(inout) :: maxconc                                                                                                                                         
 double precision,dimension(monwellsets) :: ssconc, x                                                                                                                                                     
                                                                                                                                                                                                          
 double precision:: longdisp,longdisp1,transdisp,transdisp1,velocity,Rfs, &                                                                                                                               
                    x1,x2,x3,x4,L,Sw, &                                                                                                                                                                   
                    ssconcb, ssconcw,maxim,velocity1,val1,val2,val3,val4,val5                                                                                                                             
 integer :: i, j                                                                                                                                                                                          
! real :: erf,erfc                                                                                                                                                                                        
 character(100) :: heading                                                                                                                                                                                
                                                                                                                                                                                                          
 ! This is to initialize the steady state concentration values                                                                                                                                            
  ssconcw = 0.0                                                                                                                                                                                           
  ssconcb = 0.0                                                                                                                                                                                           
                                                                                                                                                                                                          
                                                                                                                                                                                                          
 ! This is the section where the input file is read for the analytical model                                                                                                                              
 call OpenInputFile(hSsInp, strSsInp)                                                                                                                                                                     
 !CALL openfile(hSsInp)                                                                                                                                                                                   
 ! UNIT hSsInp='ssinp.dat'                                                                                                                                                                                
 rewind hSsInp                                                                                                                                                                                            
 read(hSsInp,*) heading                                                                                                                                                                                   
                                                                                                                                                                                                          
  do i=1,monwellsets                                                                                                                                                                                      
    read(hSsInp,*) x(i)                                                                                                                                                                                   
    !print *,'the value of x is',i,x(i)                                                                                                                                                                   
  end do                                                                                                                                                                                                  
 read(hSsInp,*) L,Sw                                                                                                                                                                                      
 close(hSsInp)                                                                                                                                                                                            
 !print *,'the value of L and Sw is',L,Sw                                                                                                                                                                 
                                                                                                                                                                                                          
!DEC$ IF( COST_VERSION>=2 )                                                                                                                                                                               
	! Converting the values to the proper units(cm in this case)                                                                                                                                      
	longdisp1 = longdisp * 100                                                                                                                                                                        
	!print *,'the longdisp value is',longdisp1                                                                                                                                                        
	transdisp1 = transdisp * 100                                                                                                                                                                      
	velocity1 = velocity *100/Rfs                                                                                                                                                                     
	! This is the end of the input and conversions section                                                                                                                                            
!DEC$ ELSE                                                                                                                                                                                                
	 velocity1 = velocity/Rfs                                                                                                                                                                         
	 longdisp1 = longdisp                                                                                                                                                                             
	 transdisp1 = transdisp                                                                                                                                                                           
!DEC$ ENDIF                                                                                                                                                                                               
                                                                                                                                                                                                          
 ! To find the concentration at the receptors                                                                                                                                                             
  maxim = 0.0                                                                                                                                                                                             
  do i=1,monwellsets-1                                                                                                                                                                                    
                                                                                                                                                                                                          
   val1 = Sw/(4*(transdisp1*x(i))**0.5)                                                                                                                                                                   
   val2 = (1 - (1+4*L*longdisp1/velocity1)**0.5)                                                                                                                                                          
   val3 = x(i)/(2*longdisp1)                                                                                                                                                                              
   val4 = exp(real(val2*val3))                                                                                                                                                                            
   val5 = erf(real(val1))                                                                                                                                                                                 
   ssconc(i) = maxconc(i)*val4 * val5                                                                                                                                                                     
    if(ssconc(i).gt.maxim) then                                                                                                                                                                           
         maxim = ssconc(i)                                                                                                                                                                                
    endif                                                                                                                                                                                                 
                                                                                                                                                                                                          
  enddo                                                                                                                                                                                                   
  ssconcw = maxim                                                                                                                                                                                         
                                                                                                                                                                                                          
  i = monwellsets                                                                                                                                                                                         
  val1 = Sw/(4*(transdisp1*x(i))**0.5)                                                                                                                                                                    
  val2 = (1 - (1+4*L*longdisp1/velocity1)**0.5)                                                                                                                                                           
  val3 = x(i)/(2*longdisp1)                                                                                                                                                                               
  val4 = exp(real(val2*val3))                                                                                                                                                                             
  val5 = erf(real(val1))                                                                                                                                                                                  
  ssconcb = maxconc(i) * val4 * val5                                                                                                                                                                      
                                                                                                                                                                                                          
                                                                                                                                                                                                          
 return                                                                                                                                                                                                   
end subroutine ssanal                                                                                                                                                                                     
                                                                                                                                                                                                          
! #########################################################################################################################################                                                               
                                                                                                                                                                                                          
! This subroutine is used to calculate the risk for the given design.                                                                                                                                     
! The values used here are used to find out how much the risk violates the standard.                                                                                                                      
! The risk calculations and input values have been determined from the literature                                                                                                                         
! Written on by Gayathri Gopalakrishnan                                                                                                                                                                   
! The code used for reference is the code by Bryan Smalley                                                                                                                                                
                                                                                                                                                                                                          
! The variables used here are                                                                                                                                                                             
! risk = the risk value that is ultimately returned from the function                                                                                                                                     
! contconc = the contaminant concentration in groundwater (from the simulation model)                                                                                                                     
! n =                                                                                                                                                                                                     
! adultin = the adult ingestion rate                                                                                                                                                                      
! childin = the child ingestion rate                                                                                                                                                                      
! exptime = the exposure duration                                                                                                                                                                         
! bodywt = the bodywt of an adult                                                                                                                                                                         
! inrate = the inhalation rate of the contaminant which is assumed to be the same for children and adults                                                                                                 
! showertime = the time spent taking a shower                                                                                                                                                             
! timeinbath = the time spent in the bathroom after the shower                                                                                                                                            
! ratiohouse = the ratio of contaminant conc of household air per concentration of groundwater                                                                                                            
! ratioair = the ratio of contaminant conc of shower/bathroom air per concentration of groundwater                                                                                                        
! concair = the contaminant conc of household air                                                                                                                                                         
! concshower = the contaminant conc of shower/bathroom                                                                                                                                                    
! riskaning = adult cancer risk(ingestion)                                                                                                                                                                
! riskanc = adult cancer risk (inhalation)                                                                                                                                                                
! riskashw = adult cancer risk(showering)                                                                                                                                                                 
! riskcing = child cancer risk(ingestion)                                                                                                                                                                 
! riskcnc = child cancer risk(inhalation)                                                                                                                                                                 
! riskcshw = child cancer risk (showering)                                                                                                                                                                
! inadult = the inhalation rate in adults                                                                                                                                                                 
! inchild = the inhalation rate in children                                                                                                                                                               
! trunc = the time at which the exposure duration is truncated for children                                                                                                                               
                                                                                                                                                                                                          
subroutine CalcRisk(risk,contconc,adultin,childin,exptime,bodywt,inrate,showertime,timeinbath,ratiohouse,ratioair)                                                                                        
 IMPLICIT NONE                                                                                                                                                                                            
 double precision:: risk,contconc,adultin,childin,exptime,bodywt,inrate,showertime,timeinbath,ratiohouse,ratioair,concair, &                                                                              
                    concshower,riskaing,riskanc,riskashw,riskcing,riskcnc,riskcshw,freq,avgtime,oralslope,inslope,childwt, &                                                                              
                    timein,volfactor,housvol,exrate,bathvol,inadult,inchild,trunc,mixcoeff                                                                                                                
                                                                                                                                                                                                          
                                                                                                                                                                                                          
 ! This is to initialize the risk values                                                                                                                                                                  
  risk = 0.0                                                                                                                                                                                              
                                                                                                                                                                                                          
 call OpenInputFile(hRiskInp, strRiskInp)                                                                                                                                                                 
! CALL openfile(hRiskInp)                                                                                                                                                                                 
 ! UNIT hRiskInp='riskinp.dat'                                                                                                                                                                            
 rewind hRiskInp                                                                                                                                                                                          
 read (hRiskInp,*) freq,avgtime,oralslope,inslope,childwt,timein,volfactor,housvol,exrate,mixcoeff,bathvol                                                                                                
 close(hRiskInp)                                                                                                                                                                                          
 ! The conversion calculations are                                                                                                                                                                        
 timein = timein/24.0                                                                                                                                                                                     
 adultin = adultin/1000.0                                                                                                                                                                                 
 childin = childin/1000.0                                                                                                                                                                                 
 bodywt = bodywt * 0.4536 ! conversion into kg from lb                                                                                                                                                    
 showertime = showertime/24.0                                                                                                                                                                             
 timeinbath = timeinbath/24.0                                                                                                                                                                             
 ! This is the end of the conversion calculations and of the input file section                                                                                                                           
                                                                                                                                                                                                          
 ! this is the calculation of the risk part                                                                                                                                                               
 ! concentration of contaminant in household air                                                                                                                                                          
 concair = contconc * ratiohouse                                                                                                                                                                          
 concshower = contconc * ratioair                                                                                                                                                                         
                                                                                                                                                                                                          
                                                                                                                                                                                                          
 ! the inhalation rate of the adults and children                                                                                                                                                         
 inadult = inrate * bodywt                                                                                                                                                                                
 inchild = inrate * childwt                                                                                                                                                                               
  !print *, ' inhalation rates are', inadult, inchild                                                                                                                                                     
                                                                                                                                                                                                          
 ! Adult risk                                                                                                                                                                                             
 ! from ingestion of tap water, inhalation of air in the house and from showering                                                                                                                         
 riskaing = (contconc*adultin*freq*exptime*oralslope)/(bodywt*avgtime)                                                                                                                                    
  !print *, 'the numbers apart from the risk are',contconc,adultin,freq,exptime,oralslope,bodywt,avgtime                                                                                                  
  !print *, 'risk adult ingestion', riskaing                                                                                                                                                              
 riskanc = (concair * inadult *timein * freq * exptime *inslope)/(bodywt*avgtime)                                                                                                                         
 riskashw = (concshower * inadult * (showertime + timeinbath) * freq *exptime * inslope)/(bodywt * avgtime)                                                                                               
  !print *, 'adult risks are', riskaing, riskanc, riskashw                                                                                                                                                
                                                                                                                                                                                                          
 ! Child risk                                                                                                                                                                                             
 ! from ingestion of tap water, inhalation of air in the house and from showering                                                                                                                         
 ! The time at which the exposure duration is truncated to 6.0 yrs                                                                                                                                        
 trunc = 6.0                                                                                                                                                                                              
   if (exptime.gt.trunc) then                                                                                                                                                                             
     exptime = trunc                                                                                                                                                                                      
   endif                                                                                                                                                                                                  
 riskcing = (contconc * childin * freq * exptime * oralslope)/(childwt * avgtime)                                                                                                                         
 riskcnc = (concair * inchild * timein * freq * exptime * inslope)/(childwt * avgtime)                                                                                                                    
 riskcshw = (concshower * inchild * (showertime + timeinbath) * freq * exptime * inslope)/(childwt * avgtime)                                                                                             
 !print *, 'child risks are', riskcing, riskcnc, riskcshw                                                                                                                                                 
                                                                                                                                                                                                          
  ! calculating the lifetime risk for the given design                                                                                                                                                    
  risk = riskaing + riskanc + riskashw + riskcing + riskcnc + riskcshw                                                                                                                                    
 ! this is the end of the subroutine for the calculation of the risk for the given design                                                                                                                 
                                                                                                                                                                                                          
  return                                                                                                                                                                                                  
end subroutine CalcRisk                                                                                                                                                                                   
                                                                                                                                                                                                          
! This marks the end of the penalty head subroutine                                                                                                                                                       
                                                                                                                                                                                                          
! #########################################################################################################################################                                                               
                                                                                                                                                                                                          
! This function is used to determine the penalty weights for violation of the risk constraint                                                                                                             
! Written  by Gayathri Gopalakrishnan                                                                                                                                                                     
! Will be called later by the function fitness or by cost function to create the correct objective function                                                                                               
                                                                                                                                                                                                          
! The variables declared here are                                                                                                                                                                         
! pen1 = the value of the risk when the design is the current design                                                                                                                                      
! pen2 = the value of the risk when the design is the standard design                                                                                                                                     
! coeff = the coefficient used to calculate the weight given to the risk violation                                                                                                                        
! the input file with the required parameters is used in the include statement in the beginning of the subroutine                                                                                         
                                                                                                                                                                                                          
double precision function CalcViolateRisk( rCurRisk, rStdRisk )                                                                                                                                           
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
		CalcViolateRisk = (rCurRisk - rStdRisk)*rCoeff                                                                                                                                            
	else                                                                                                                                                                                              
		CalcViolateRisk = 0                                                                                                                                                                       
	end if                                                                                                                                                                                            
end function CalcViolateRisk                                                                                                                                                                              
                                                                                                                                                                                                          
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
                                                                                                                                                                                                          
! The variables used here are                                                                                                                                                                             
! penal1 = the head for the given design                                                                                                                                                                  
! penal2 = the head for the standard or the no-flow condition                                                                                                                                             
! penalcalc = the value to be returned to the main function when called which is the penalty for the head violation                                                                                       
! coefficent1 = the coefficient used to multiply the penalty ---used in the input file for penalty head                                                                                                   
! the Input file used here is penheadinput.dat                                                                                                                                                            
!double precision function CalcViolateH( rCurH, rStdH, rMinViolH, rMaxViolH )                                                                                                                             
double precision function CalcViolateH( rCurH, rStdH )                                                                                                                                                    
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
		CalcViolateH = (abs(rStdH-rCurH)-rMargErr*rStdH)*rCoeff                                                                                                                                   
	else                                                                                                                                                                                              
		CalcViolateH = 0                                                                                                                                                                          
	endif                                                                                                                                                                                             
end function CalcViolateH                                                                                                                                                                                 
                                                                                                                                                                                                          
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
! This marks the end of the penalty well subroutine                                                                                                                                                       
                                                                                                                                                                                                          
subroutine PDEModel(arrTxx,rTime, arrH, arrHun, arrCon)                                                                                                                                                   
use pdeio                                                                                                                                                                                                 
IMPLICIT NONE                                                                                                                                                                                             
	!arguments                                                                                                                                                                                        
	double precision, dimension(:), intent(in) :: arrTxx                                                                                                                                              
	double precision, intent(in) :: rTime                                                                                                                                                             
	double precision, dimension(:,:,:), intent(inout) :: arrH, arrHun                                                                                                                                 
	double precision, dimension(:,:), intent(out) :: arrCon                                                                                                                                           
	                                                                                                                                                                                                  
	!variables                                                                                                                                                                                        
	!montoring well locations, it's used to lookup arrCon only                                                                                                                                        
	type(WellLoc), dimension(g_nMonWells) :: arrMonWellLocs                                                                                                                                           
                                                                                                                                                                                                          
!external nummodel                                                                                                                                                                                        
	!prepare datafile to feed modelflow and rt3d                                                                                                                                                      
	call PreNumModel(arrTxx, rTime, g_nFinGridX, g_nFinGridY, g_nFinGridZ )                                                                                                                           
	!call PDE                                                                                                                                                                                         
	if( rTime>0 )call ModFlowEntry('mod2.mfs')                                                                                                                                                        
	call Rt3dEntry( 'rt3d2.rts' )                                                                                                                                                                     
	!call nummodel                                                                                                                                                                                    
	!read PDE result                                                                                                                                                                                  
	call PostNumModel( g_nFinGridX, g_nFinGridY, g_nFinGridZ, arrH, arrHun, arrMonWellLocs, g_nMonWells, arrCon, g_nContNo )                                                                          
end subroutine PDEModel                                                                                                                                                                                   
                                                                                                                                                                                                          
subroutine CalcMaxConSets( arrCon, nCurCon, nWells, arrMaxCon, nSets )                                                                                                                                    
implicit none                                                                                                                                                                                             
	!arguments                                                                                                                                                                                        
	double precision, dimension(:,:), intent(in):: arrCon                                                                                                                                             
	double precision, dimension(:), intent(out) :: arrMaxCon                                                                                                                                          
	integer, intent(in) :: nCurCon, nWells, nSets                                                                                                                                                     
                                                                                                                                                                                                          
	!variables                                                                                                                                                                                        
	integer :: i, k                                                                                                                                                                                   
	integer :: nSetLen                                                                                                                                                                                
                                                                                                                                                                                                          
	nSetLen = nWells / nSets                                                                                                                                                                          
	do i = 1, nSets                                                                                                                                                                                   
		k = (i-1)*nSetLen + 1                                                                                                                                                                     
		arrMaxCon(i) = arrCon(nCurCon, k)                                                                                                                                                         
		do k = k+1, i*nSetLen                                                                                                                                                                     
			arrMaxCon(i) = max( arrMaxCon(i), arrCon(nCurCon, k) )                                                                                                                            
		end do                                                                                                                                                                                    
	end do                                                                                                                                                                                            
end subroutine CalcMaxConSets                                                                                                                                                                             
                                                                                                                                                                                                          
subroutine CostWrapper                                                                                                                                                                                    
use pdeio                                                                                                                                                                                                 
use casewell                                                                                                                                                                                              
use fmt_helper                                                                                                                                                                                            
!use params                                                                                                                                                                                               
implicit none                                                                                                                                                                                             
	!parameters for compitable with the cost function                                                                                                                                                 
	double precision, dimension(g_nRemWells+g_nMonWells) :: Qw                                                                                                                                        
	double precision, dimension(:,:,:), allocatable :: H, Hun                                                                                                                                         
	integer, dimension(g_nRemWells) :: ndrwx, ndrwy, ndrwz                                                                                                                                            
                                                                                                                                                                                                          
	integer :: i, j, t                                                                                                                                                                                
	integer :: nLoc                                                                                                                                                                                   
	type(WellLoc), dimension(:), pointer :: parrLocs                                                                                                                                                  
                                                                                                                                                                                                          
	double precision, dimension(:), allocatable :: arrTxx                                                                                                                                             
	integer nFieldNo                                                                                                                                                                                  
                                                                                                                                                                                                          
	!cost, current time and ir                                                                                                                                                                        
	double precision :: rTotCost, rCurTime                                                                                                                                                            
                                                                                                                                                                                                          
	!risk parameters                                                                                                                                                                                  
	double precision :: rAdultIn, rChildIn, rExpTime, rBodyWt, rInRate, rShowerTime, rTimeBath, rRatioHouse, rRatioAir                                                                                
	double precision :: rRiskPenaltyB, rRiskPenaltyW, rRiskB, rRiskW, rSsConB, rSsConW, rPenHead, rPenRisk                                                                                            
                                                                                                                                                                                                          
	!concentration                                                                                                                                                                                    
	double precision, dimension(g_nContNo, g_nMonWells) :: arrCon                                                                                                                                     
	double precision, dimension(g_nMonWellSets) :: arrMaxCon                                                                                                                                          
                                                                                                                                                                                                          
	!flag to stop remetiation step loop                                                                                                                                                               
	logical :: bBoundFlag, bInterFlag                                                                                                                                                                 
                                                                                                                                                                                                          
	!--------------------------------------code begin here----------------------                                                                                                                      
	allocate( H(g_nFinGridZ,g_nFinGridX,g_nFinGridY), Hun(g_nFinGridZ,g_nFinGridX,g_nFinGridY) )                                                                                                      
	allocate( arrTxx(g_nFem) )                                                                                                                                                                        
                                                                                                                                                                                                          
                                                                                                                                                                                                          
	!wrap for compatible                                                                                                                                                                              
	do i=1,g_nRemWells                                                                                                                                                                                
		!translate the current coordinate                                                                                                                                                         
		nLoc = g_arrWellLocs(i)                                                                                                                                                                   
		parrLocs => g_parrWellInfos(i)%parrLocs                                                                                                                                                   
		ndrwx(i) = parrLocs(nLoc)%x                                                                                                                                                               
		ndrwy(i) = parrLocs(nLoc)%y                                                                                                                                                               
		ndrwz(i) = parrLocs(nLoc)%z                                                                                                                                                               
		Qw(i) = g_arrPumpRates(i)                                                                                                                                                                 
	end do                                                                                                                                                                                            
	do i=g_nRemWells+1, g_nRemWells+g_nMonWells                                                                                                                                                       
		Qw(i) = 0                                                                                                                                                                                 
	end do                                                                                                                                                                                            
                                                                                                                                                                                                          
	!parpare the arrTxx, hardcode the sample number 816                                                                                                                                               
	nFieldNo = 812                                                                                                                                                                                    
	do i = 1, g_nFem                                                                                                                                                                                  
!DEC$ IF(COST_VERSION>=2)                                                                                                                                                                                 
		arrTxx(i) = g_rT(i + g_nFem*(nFieldNo-1)) * l_rB                                                                                                                                          
!DEC$ ELSE                                                                                                                                                                                                
		arrTxx(i) = g_rT(i + g_nFem*(nFieldNo-1)) * l_rB * 365                                                                                                                                    
!DEC$ ENDIF                                                                                                                                                                                               
	end do                                                                                                                                                                                            
	                                                                                                                                                                                                  
	!this section is for hydraulic violation caculation                                                                                                                                               
	if( g_nModelFlag==1 )then                                                                                                                                                                         
		call PDEModel(arrTxx, g_rRemTime*g_nMonInt, H, Hun, arrCon )                                                                                                                              
		rPenHead = 0                                                                                                                                                                              
		!calculate the head violation                                                                                                                                                             
		do i=1, g_nRemWells                                                                                                                                                                       
			rPenHead = rPenHead + CalcPenaltyH( H(ndrwz(i),ndrwx(i),ndrwy(i)), Hun(ndrwz(i),ndrwx(i),ndrwy(i)) )                                                                              
			!g_arrViolaH(i) = CalcViolateH( H(ndrwz(i),ndrwx(i),ndrwy(i)), Hun(ndrwz(i),ndrwx(i),ndrwy(i)))                                                                                   
		end do                                                                                                                                                                                    
 	endif                                                                                                                                                                                             
                                                                                                                                                                                                          
	rPenHead = rPenHead + CalcPenaltyWell( Qw, g_arrWellLocs, g_nRemWells, 58, g_parrWellInfos(1)%rMaxPumpRate )                                                                                      
		                                                                                                                                                                                          
	! This section is to find the maximum concentration of the contaminant in the wells and store                                                                                                     
	! it in maxconc(monwellsets)                                                                                                                                                                      
	arrMaxCon = 0.0                                                                                                                                                                                   
	bBoundFlag = .false.                                                                                                                                                                              
	bInterFlag = .false.                                                                                                                                                                              
	g_rViolaRisk = 0                                                                                                                                                                                  
                                                                                                                                                                                                          
	call GenRiskParam( g_rRiskDists, rAdultIn, rChildIn, rExpTime, rBodyWt, rInRate, rShowerTime, rTimeBath, rRatioHouse, rRatioAir )                                                                 
!DEC$ IF(COST_VERSION>=2)                                                                                                                                                                                 
	do t = 1, g_rRemTime                                                                                                                                                                              
!DEC$ ELSE                                                                                                                                                                                                
	do t = 0, g_rRemTime                                                                                                                                                                              
!DEC$ ENDIF                                                                                                                                                                                               
		if( bBoundFlag .or. bInterFlag )exit                                                                                                                                                      
		!do pp=1,monint ....monint is assumed to be same as remtime...hence not used currently                                                                                                    
		rCurTime = g_nMonInt*t                                                                                                                                                                    
                                                                                                                                                                                                          
		! This section is used to find the concentrations over all the boundary wells                                                                                                             
		! The analytical model is called here. Note that at a later point in time                                                                                                                 
		! a conditional statement must be inserted to ensure that the rt3d can also be called                                                                                                     
		if( g_nModelFlag==1 )then                                                                                                                                                                 
			call PDEModel(arrTxx, rCurTime, H, Hun, arrCon)                                                                                                                                   
		end if                                                                                                                                                                                    
                                                                                                                                                                                                          
		! Looping over all the contaminants                                                                                                                                                       
		! contno is to be supplied through the input file                                                                                                                                         
		! reaction is to be supplied through the input file                                                                                                                                       
		!i-reaction number, j-concentration number                                                                                                                                                
		do i = 1, g_nReactNo                                                                                                                                                                      
			do j=1, g_nContNo                                                                                                                                                                 
				if( g_nModelFlag.ne.0) then                                                                                                                                               
					!To find the maximum concentration using the numerical model                                                                                                      
					call CalcMaxConSets( arrCon, 1, g_nMonWells, arrMaxCon, g_nMonWellSets )                                                                                          
					! To find the steady state concentration at the exposure well                                                                                                     
					call ssanal(rSsConW,rSsConB,l_rDaL,l_rDaT,l_rVelocity,l_rRfs,arrMaxCon,g_nMonWellSets)                                                                            
				end if                                                                                                                                                                    
	                                                                                                                                                                                                  
				!To find the human health risk at the boundary well set                                                                                                                   
				call CalcRisk(rRiskB,rSsConB,rAdultIn,rChildIn,rExpTime,rBodyWt,rInRate,rShowerTime,rTimeBath, &                                                                          
							 & rRatioHouse,rRatioAir)                                                                                                                         
				rRiskPenaltyB = CalcPenaltyRisk(rRiskB, g_rRiskSt)                                                                                                                        
                                                                                                                                                                                                          
				! Stop remediation if the humanhealth risk is violated at any time over the entire remediation period                                                                     
				if( rRiskPenaltyB .ne. 0 )bBoundFlag = .true.                                                                                                                             
                                                                                                                                                                                                          
				! To calculate the human health risk based on the interior wells                                                                                                          
				call CalcRisk(rRiskW,rSsConW,rAdultIn,rChildIn,rExpTime,rBodyWt,rInRate,rShowerTime,rTimeBath, &                                                                          
							 & rRatioHouse,rRatioAir )                                                                                                                        
				rRiskPenaltyW = CalcPenaltyRisk(rRiskW, g_rRiskSt)                                                                                                                        
                                                                                                                                                                                                          
				! Stop remediation if the human health risk is not violated at the current period, since when time is going on, the                                                       
				! concentration will be lower and lower.                                                                                                                                  
				if( rRiskPenaltyW .eq. 0 )bInterFlag = .true.                                                                                                                             
                                                                                                                                                                                                          
			end do !loop of concentration no                                                                                                                                                  
		end do !loop of reaction no                                                                                                                                                               
	end do !loop of remediation time step                                                                                                                                                             
                                                                                                                                                                                                          
	!sum the risk penalty                                                                                                                                                                             
	rPenRisk = rRiskPenaltyB + rRiskPenaltyW                                                                                                                                                          
                                                                                                                                                                                                          
	!this section is to calculate the objective function                                                                                                                                              
	rTotCost = 0.0                                                                                                                                                                                    
	call CalcRemCost( Qw, g_nRemWells+g_nMonWells, g_nRemWells, g_nMonWells, H, Hun, ndrwx, ndrwy, ndrwz, g_nFinGridX, g_nFinGridY, g_nFinGridZ, dble(g_nMonInt), g_rRemTime, t, g_arrFact, rTotCost )
	g_rFitness = rTotCost*g_rCostFactor + rPenRisk + rPenHead                                                                                                                                         
	g_rTotCost = rTotCost                                                                                                                                                                             
	g_rPenHead = rPenHead                                                                                                                                                                             
	g_rPenRisk = rPenRisk                                                                                                                                                                             
	g_rRiskB = rRiskB                                                                                                                                                                                 
	g_rRiskW = rRiskW                                                                                                                                                                                 
                                                                                                                                                                                                          
	call LoadWellHeads( strHeadNew, g_arrWellLocs, g_parrWellInfos, g_arrHeads, g_nRemWells, g_nCoarsGridX, g_nCoarsGridY, g_nCoarsGridZ )                                                            
	call LoadWellHeads( strHeadOld, g_arrWellLocs, g_parrWellInfos, g_arrOldHeads, g_nRemWells, g_nCoarsGridX, g_nCoarsGridY, g_nCoarsGridZ )                                                         
	call CostByNN( g_arrHeads, g_arrOldHeads, g_nRemWells, rRiskB, rRiskW )                                                                                                                           
!	rRiskPenaltyB = CalcPenaltyRisk(rRiskB, g_rRiskSt)                                                                                                                                                
!	rRiskPenaltyW = CalcPenaltyRisk(rRiskW, g_rRiskSt)                                                                                                                                                
!	rPenRisk = rRiskPenaltyB + rRiskPenaltyW                                                                                                                                                          
!	call LoadWellHeads( strHeadNew, g_arrWellLocs, g_parrWellInfos, g_arrHeads, g_nRemWells, g_nCoarsGridX, g_nCoarsGridY, g_nCoarsGridZ )                                                            
!	call LoadWellHeads( strHeadOld, g_arrWellLocs, g_parrWellInfos, g_arrOldHeads, g_nRemWells, g_nCoarsGridX, g_nCoarsGridY, g_nCoarsGridZ )                                                         
!	rPenHead = 0                                                                                                                                                                                      
!	do i=1, g_nRemWells                                                                                                                                                                               
!		rPenHead = rPenHead + CalcPenaltyH( g_arrHeads(i), g_arrOldHeads(i) )                                                                                                                     
!	end do                                                                                                                                                                                            
!	rPenHead = rPenHead + CalcPenaltyWell( Qw, g_arrWellLocs, g_nRemWells, 58, g_parrWellInfos(1)%rMaxPumpRate )                                                                                      
!	call CalcRemCostEx( Qw, g_nRemWells+g_nMonWells, g_nRemWells, g_nMonWells, g_arrHeads, g_arrOldHeads, g_nFinGridX, g_nFinGridY, g_nFinGridZ, dble(g_nMonInt), g_rRemTime, t, g_arrFact, rTotCost )
                                                                                                                                                                                                          
	!cleanup memory                                                                                                                                                                                   
	deallocate( H, Hun )                                                                                                                                                                              
	deallocate( arrTxx )                                                                                                                                                                              
end subroutine                                                                                                                                                                                            
                                                                                                                                                                                                          
subroutine CostByNN( arrHeads, arrOldHeads, nRemWells, rRiskB, rRiskW )                                                                                                                                   
use casewell                                                                                                                                                                                              
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
                                                                                                                                                                                                          
	open(unit=hCsgaOut, file=strCsgaOut, status='unknown', position='append' )                                                                                                                        
!	write( hCsgaOut, *) 'evaluating............... rTotCose, rPenRisk, rPenHead'                                                                                                                      
	write( hCsgaOut, * )rTotCost, rPenRisk, rPenHead                                                                                                                                                  
	close( hCsgaOut )                                                                                                                                                                                 
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
                                                                                                                                                                                                          
subroutine CalcRemCost( Qw, nowells, noremwells, nomonwells, H, Hun, ndrwx, ndrwy, ndrwz, xgrid, ygrid, zgrid, monInt, dt, o, fact, cost )                                                                
implicit none                                                                                                                                                                                             
                                                                                                                                                                                                          
!parameters definition                                                                                                                                                                                    
	integer, intent(in) :: nowells, noremwells, nomonwells                                                                                                                                            
	double precision, intent(in) :: Qw(nowells)                                                                                                                                                       
                                                                                                                                                                                                          
	integer, intent(in) :: ndrwx(noremwells), ndrwy(noremwells), ndrwz(noremwells)                                                                                                                    
                                                                                                                                                                                                          
	integer, intent(in) :: xgrid, ygrid, zgrid                                                                                                                                                        
	double precision, dimension(zgrid,xgrid,ygrid), intent(in) :: H, Hun                                                                                                                              
                                                                                                                                                                                                          
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
			temp = d - H(ndrwz(i),ndrwx(i),ndrwy(i))                                                                                                                                          
			if (temp.lt.0) then                                                                                                                                                               
				temp = 0.d0                                                                                                                                                               
			endif                                                                                                                                                                             
			rwcapcost = rwcapcost + fact(i)*a3 *((d)**(b3)) + a4 * abs((Qw(i)))**b4 * temp ** (b5)                                                                                            
		else                                                                                                                                                                                      
			if (Qw(i).eq. 0.d0) then                                                                                                                                                          
				rwcapcost = rwcapcost                                                                                                                                                     
			else                                                                                                                                                                              
				temp = H(ndrwz(i),ndrwx(i),ndrwy(i)) - Hun(ndrwz(i),ndrwx(i),ndrwy(i))                                                                                                    
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
			temp = d - H(ndrwz(i),ndrwx(i),ndrwy(i))                                                                                                                                          
			if (temp.lt.0.d0) then                                                                                                                                                            
				temp = 0.d0                                                                                                                                                               
			endif                                                                                                                                                                             
			rwopcost = rwopcost + 365.0d0 * a5 * abs(Qw(i))  * &                                                                                                                              
				&((1.0d0+ir)**(monint *dt * o) - 1.0d0)/ (ir *(1.0d0+ir)**(monint *dt * o ))*temp                                                                                         
		else                                                                                                                                                                                      
			temp = H(ndrwz(i),ndrwx(i),ndrwy(i)) - Hun(ndrwz(i),ndrwx(i),ndrwy(i))                                                                                                            
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
end subroutine CalcRemCost                                                                                                                                                                                
                                                                                                                                                                                                          
end module casenum

integer function CompareChroms( arrChrom1, arrChrom2 )
implicit none
	!arguments
	integer, dimension(*), intent(in) :: arrChrom1, arrChrom2
	CompareChroms = 1
end function CompareChroms
