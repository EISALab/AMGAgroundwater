program main
use sga
use caseio
use umacase
use std_helper
!use sgafit
!use cluster_fit
use meta_fit
!use sgannfit
use chmcache
use sampling
implicit none

	type(SGAPARAMSTRUCT), target :: sgaParam
	integer, dimension(:), allocatable :: arrNewWellNodes
	integer :: i

	character*50 :: char_time

	call time(char_time)
	open( unit=287, file='time.txt', status='unknown' )
	rewind(287)
	write(287, *)char_time


	g_sgaParam => sgaParam
	call SetChromNormFunc( Chrom_Normalize )
	call Init

	!prepare sampling moments
	call CreateMoments( sgaParam%arrMomentIds, sgaParam%arrMoments, g_nFields )

!	call DoSGA(sgaParam, ParallelFitEval)
!	call DoSGAEx(sgaParam, FitEvalNet)
!	call DoSGA(sgaParam, FitEvalNet)
	call DoSGAEx(sgaParam, FitEval)
	call Clear

	call time(char_time)
	write(287, *)char_time
!	write(287, *)totalPDE
	close( 287 )
contains

subroutine Init
implicit none
	call InitParams(sgaParam)
	call InitCase

	allocate( arrNewWellNodes(g_nNewWells) )
	do i = 1, g_nNewWells
		arrNewWellNodes(i) = g_parrNewWells(i)%nNodes
	end do

	call InitSocket
!	hChromCache = CreateChromCacheEx()	!the handle is defined in casenum module
	hChromCache = CreateChromCache()	!the handle is defined in casenum module
	call InitGA( sgaParam, g_nStressPeriod, arrNewWellNodes, g_nNewWells, g_nNewExtWells, g_nOldWells, g_nOldExtWells )
end subroutine Init

subroutine Clear
implicit none
	if( hFourYCModel/=0 )then
		call release_meta_model( hFourYCModel )
	endif
	if( hFiveYCModel/=0 )then 
		call release_meta_model( hFiveYCModel )
	endif

	call ClearGA( sgaParam )
	call ReleaseChromCache( hChromCache )
	deallocate( arrNewWellNodes )
	call ClearCase
	call ClearSocket
end subroutine Clear
	
subroutine InitParams( sgaParam )
use DFLIB
implicit none
	type(SGAPARAMSTRUCT), intent(inout) :: sgaParam
	character(40) :: buf

!	integer :: idum = -12345
	integer :: idum = -34690
!	integer :: idum = -39000
!	integer :: idum = -24359
!	integer :: idum = -97518
!	integer :: idum = -72475
!	integer :: idum = -237619
!	integer :: idum = -36760
!	integer :: idum = -12975
!	integer :: idum = -46497
!	integer :: idum = -38900


	sgaParam%bNicheFlag = .false.
	sgaParam%bInjecFlag = .false.
	sgaParam%bEliteFlag = .true.
	sgaParam%bMicroFlag = .false.

	sgaParam%nSelectOpt = SELECT_TOURNAMENT
	sgaParam%nCrossOpt = CROSS_RANDPOINT
	sgaParam%nCrossOpt = CROSS_UNIFORM
	sgaParam%nMutateOpt = 0

	sgaParam%rCrossProb = 0.5

	sgaParam%iStartGen = 1
	sgaParam%bPendingModels = .false.
	sgaParam%rMinPdeFit = 1e10

!	sgaParam%nPopSize = 4
	sgaParam%nPopSize = 120
	sgaParam%nMaxGens = 100
	sgaParam%nMaxDangleGens = 20
	sgaParam%nMinTrainPts = 350
!	sgaParam%nMinTrainPts = 450
!	sgaParam%nMinTrainPts = 10
	sgaParam%bEnableProbSelect = .true.
	sgaParam%bEnableFitUpdating = .true.
	sgaParam%arrBestClusterRange = (/30, 60/)
!	sgaParam%nInitSampling = 2
!	sgaParam%nRetrainingPoolSize = 12
!	sgaParam%nSamples = 2
	sgaParam%nSamples = 6
	sgaParam%nInitSampling = 25
	sgaParam%nRetrainingPoolSize = 120

	if( NARGS()>=2 )then
		call getarg( 1, buf )
		idum = StrToFC( buf )
	endif
	print*, idum
	call UniRand( idum )

	if( NARGS()>=3 )then 
		call getarg( 2, buf )
!		call ReadANGAParam( sgaParam, buf )
	endif
	
	sgaParam%rMutateProb = 1.0/sgaParam%nPopSize
	
end subroutine InitParams

subroutine ReadAnGAParam( sgaParam, fname )
implicit none
	character(*), intent(in) :: fname
	type(SGAPARAMSTRUCT), intent(inout) :: sgaParam
	integer :: hFile

	hFile = 1000
	call OpenInputFile( hFile, fname )
	read( hFile, * )sgaParam%nPopSize, sgaParam%nMaxGens, sgaParam%nMinTrainPts	!population size and max generations
	read( hFile, * )sgaParam%nInitSampling, sgaParam%nRetrainingPoolSize		!inital sampling rate and retraining pool size
	read( hFile, * )sgaParam%bEnableProbSelect, sgaParam%bEnableFitUpdating		!prob_select, fit_updating
	read( hFile, * )sgaParam%arrBestClusterRange								!the range for best clustering
	read( hFile, * )sgaParam%arrProbSelectRange									!the range for probability selection
	read( hFile, * )sgaParam%nMaxDangleGens										!maximum generations to run before finding better solutions

	close( hFile )
end subroutine ReadAnGaParam
	
end program main
