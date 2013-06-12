program main
use sga
use caseio
use umacase
use std_helper
use sgafit
!use cluster_fit
!use sgannfit
use chmcache
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
	call Init
	call DoSGAEx(sgaParam, ParallelFitEval)
!	call DoSGAEx(sgaParam, FitEvalNet)
!	call DoSGA(sgaParam, FitEvalNet)
!	call DoSGAEx(sgaParam, FitEvalCluster)
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
	hChromCache = CreateCustomChromCache()	!the handle is defined in casenum module
!	hChromCache = CreateChromCache()	!the handle is defined in casenum module
	call InitGA( sgaParam, g_nStressPeriod, arrNewWellNodes, g_nNewWells, g_nNewExtWells, g_nOldWells, g_nOldExtWells )
end subroutine Init

subroutine Clear
implicit none
	call ClearGA( sgaParam )
	call ReleaseChromCache( hChromCache )
	deallocate( arrNewWellNodes )
	call ClearCase
	call ClearSocket
end subroutine Clear
	
subroutine InitParams( sgaParam )
use DFLIB
use condor_helper
implicit none
	type(SGAPARAMSTRUCT), intent(inout) :: sgaParam
	character(40) :: buf

!	integer :: idum = -12345
!	integer :: idum = -34690
!	integer :: idum = -39000
!	integer :: idum = -24359
!	integer :: idum = -97518
!	integer :: idum = -72475
!	integer :: idum = -237619
!	integer :: idum = -36760
!	integer :: idum = -12975
!	integer :: idum = -46497
	integer :: idum = -38900


	integer, parameter :: hCaseInput = 36
	character(*), parameter :: strCaseInput = 'caseinp1.dat'

!	call OpenInputFile( hCaseInput, strCaseInput )
!	read( hCaseInput, *) sgaParam%nPopSize
!	close( hCaseInput )

!	call OpenInputFile( hCaseInput, 'seed.txt' )
!	read( hCaseInput, *)idum
!	close( hCaseInput )
 
	if( NARGS()>=2 )then
		call getarg( 1, buf )
		idum = StrToFC( buf )
	endif
		
	print*, idum

	sgaParam%nPopSize = 160
	
	sgaParam%bNicheFlag = .false.
	sgaParam%bInjecFlag = .false.
	sgaParam%bEliteFlag = .true.
	sgaParam%bMicroFlag = .false.

	sgaParam%nSelectOpt = SELECT_TOURNAMENT
	sgaParam%nCrossOpt = CROSS_RANDPOINT
	sgaParam%nCrossOpt = CROSS_UNIFORM
	sgaParam%nMutateOpt = 0

	sgaParam%rCrossProb = 0.5
!	sgaParam%rMutateProb = 0.001
!	sgaParam%rCrossProb = 0.7
!	sgaParam%rMutateProb = 0.01
	sgaParam%rMutateProb = 1.0/sgaParam%nPopSize

	sgaParam%nMaxGens = 10
	sgaParam%iStartGen = 1

	call UniRand( idum )
end subroutine InitParams

end program main
