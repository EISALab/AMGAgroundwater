program main
use SGA
use CASEIO
use CASEWELL
use STD_HELPER
use CASENUM
use nnsgahelper
implicit none

	type(SGAPARAMSTRUCT) :: sgaParam
	double precision, dimension(:), allocatable :: arrMaxPumpRates
	integer, dimension(:), allocatable :: arrPumpFlags, arrWellNodes
	integer :: i

	character*50 :: char_time

	call time(char_time)
	open( unit=287, file='time.txt', status='unknown' )
	rewind(287)
	write(287, *)char_time

	call InitParams(sgaParam)
	call InitCase

	allocate( arrMaxPumpRates(g_nRemWells), arrPumpFlags(g_nRemWells), arrWellNodes(g_nRemWells) )
	do i = 1, g_nRemWells
		arrPumpFlags(i) = g_parrWellInfos(i)%nPumpFlag
		arrMaxPumpRates(i) = g_parrWellInfos(i)%rMaxPumpRate
		arrWellNodes(i) = g_parrWellInfos(i)%nNodes
	end do

	totalPDE = 0
	call InitSocket
	hChromCache = CreateChromCache()	!the handle is defined in casenum module
	call InitGA( sgaParam, arrPumpFlags, arrMaxPumpRates, arrWellNodes, g_nRemWells )

	call DoSGA(sgaParam, FitEvalNet)

	call CleanGA( sgaParam )
	call ReleaseChromCache( hChromCache )
	deallocate( arrMaxPumpRates, arrPumpFlags, arrWellNodes )
	call CleanCase
	call ClearSocket

	call time(char_time)
	write(287, *)char_time
	write(287, *)totalPDE
	close( 287 )
contains
subroutine InitParams( sgaParam )
use DFLIB
use condor_helper
implicit none
	type(SGAPARAMSTRUCT), intent(inout) :: sgaParam
	character(40) :: buf

!	integer :: idum = -43003
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


	integer, parameter :: hCaseInput = 36
	character(*), parameter :: strCaseInput = 'caseinp.dat'

	call OpenInputFile( hCaseInput, strCaseInput )
	read( hCaseInput, *) sgaParam%nPopSize
	close( hCaseInput )

	call OpenInputFile( hCaseInput, 'seed.txt' )
	read( hCaseInput, *)idum
	close( hCaseInput )

	if( NARGS()>=2 )then
		call getarg( 1, buf )
		idum = StrToFC( buf )
	endif
		
	print*, idum

	sgaParam%nPopSize = 100
	
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
	sgaParam%rMutateProb = 0.002

	sgaParam%nMaxGens = 96
	sgaParam%iStartGen = 1

	call UniRand( idum )
end subroutine InitParams

subroutine testNeuralNet
use neuroio
use nnsgahelper
implicit none

	type(NetLayerParam), dimension(3), target :: layerParam
	type(NeuralNetParam) :: netParam
	integer :: hNet

	layerParam(1)%nNeuros = 9

	layerParam(2)%nNeuros = 12
	layerParam(2)%rLearnCoef = 0.5
	layerParam(2)%rMomenCoef = 0.1

	layerParam(3)%nNeuros = 3
	layerParam(3)%rLearnCoef = 0.5
	layerParam(3)%rMomenCoef = 0.1

	netParam%nLayers = 3

	call AssocLayerNetParam( layerParam, netParam )
!	netParam%pLayerParam => layerParam

	hNet = CreateNet( netParam )

!	call TrainMatlabNet( 'aaa.txt', 'bbb.txt' )

	call ReleaseNet( hNet )

end subroutine
end program main
