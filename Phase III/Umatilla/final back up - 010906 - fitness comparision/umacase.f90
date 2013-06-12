module umacase
use std_helper
use sga
implicit none

!well location definition, it's the possible node location of a remediation well (id, x, y, z)
type WellLoc
	integer :: nId
	integer :: x, y, z
	double precision :: rMaxPumpRate	!the maximum pumping rate of the well at the location.
end type WellLoc

!information of a remediation well
!strName : the well name
!nPumpFlag : the pumping flag, 1-injection, 2-extraction, 3-either
!rMaxPumpRate : the maximum possible pumping rate
!nNodes is the length of the array containing all the location candidates for the remediation well
!parrLocs point to an allocated array stocking all the possible nodes of the well location
type RemWellInfo
	integer :: nPumpFlag
	integer :: nNodes
	type(WellLoc), dimension(:), pointer :: parrLocs
end type RemWellInfo

!the array g_parrOldWell and g_parrNewWells read data from the parameter files, it saves the well infomation
!g_parrOldWells(g_nOldWells) and g_parrNewWells(g_nNewWells) has the well information for the old and new wells
type(RemWellInfo), dimension(:), pointer :: g_parrOldWells, g_parrNewWells
integer :: g_nOldWells, g_nNewWells
integer :: g_nOldExtWells, g_nNewExtWells

!stress period
integer, parameter :: g_nStressPeriod = 1

!max capacity coefficient of injection well
double precision, parameter :: g_rMaxAlpha = 0.9

!grid flags
integer :: g_nGridFlagX, g_nGridFlagY


!########################################################################################################
!grid specified parameters for the case study
integer, parameter :: g_nFields = 100-4
integer, parameter :: g_nCoarsGridX = 16
integer, parameter :: g_nCoarsGridY = 8
integer, parameter :: g_nCoarsGridZ = 1
integer, parameter :: g_nFem = g_nCoarsGridX * g_nCoarsGridY * g_nCoarsGridZ
!risk specified parameters
integer, parameter :: g_nRiskMat1 = 2
integer, parameter :: g_nRiskMat2 = 9
!g_rT stores conductivity coefficient of the acquifer
!g_rRiskDisks contains parameters for risk calculation
double precision :: g_rT(g_nFem*g_nFields), g_rRiskDists(g_nRiskMat2, g_nRiskMat1) 

!finer grid X, Y and Z
integer :: g_nFinGridX, g_nFinGridY, g_nFinGridZ


!objective
double precision :: g_rRiskB
double precision :: g_rRiskW
double precision :: g_rTotCost
double precision :: g_rPenHead
double precision :: g_rPenRisk
double precision :: g_rFitness

!decision variables
!g_arrPumpRates is the array contains the flow rate of the remediation wells, it's length is g_nRemWells
!g_arrHeads is the hydraulic heads at the wells, g_arrOldHeads is the Hun at the wells.
!g_arrWellLocs contains the well location number(index) of the remediation wells, it's length is g_nRemWells
double precision, dimension(:), allocatable :: g_arrPumpRates
double precision, dimension(:), allocatable :: g_arrHeads, g_arrOldHeads
integer, dimension(:), allocatable :: g_arrWellLocs

double precision, dimension(:), allocatable :: g_arrFact !I don't know what's it's for

!constraints
double precision, dimension(:), allocatable :: g_arrViolaH !<0 less than minimum H, 0-no violate, >0 higher than maximum H
double precision :: g_rViolaRisk	!0-no violate, >0 the violation (curRisk-stdRisk)

!tempory global vairalbes storing the current solutions of decision variables
double precision, dimension(:), allocatable :: g_arrTmpX


integer, parameter :: hMonRemFile = 20
character(*), parameter :: strMonRemFile = 'monrem.dat'

integer, parameter :: hFileOptRes = 21
integer, parameter :: hFileOptWel = 22

character(*), parameter :: strFileOptRes = 'uma4.res'
character(*), parameter :: strFileOptWel = 'optdemo.wel'
character(*), parameter :: fmt_res = '(I3/I3/I3/F25.5)'
character(*), parameter :: fmt_wel = '(I2,I4,I4,F25.5)'

!type(SGAPARAMSTRUCT), pointer :: g_sgaParam

contains

double precision function Q_F2G( var )
implicit none
	!arguments
	double precision, intent(in) :: var

	Q_F2G = var * 1.4238d-5
end function Q_F2G

double precision function Q_G2F( var )
implicit none
	!arguments
	double precision, intent(in) :: var

	Q_G2F = dble(nint(var / 1.4238d-5))
end function Q_G2F

! ########################################################################################################

! This is a subroutine that initializes some of the variables defined in SGAPARAMSTRUCT
! Since these variables are dependant on the case study, thus they are initialized here.
! Also note that the dimension '15' given to some of the variables should be as close to the
! actual number of variables, to avoid arraybound problems.
! This also creates the chromosome, once the details of the case study are known.
! written by Meghna Babbar, Jan 04, 2001

subroutine InitGA( sgaParam, nStressPeriod, arrWellNodes, nNewWells, nNewExtWells, nOldWells, nOldExtWells )
use sga
implicit none
	!arguments
	integer, intent(in) :: nStressPeriod, nNewWells, nNewExtWells, nOldWells, nOldExtWells
	integer, dimension(nNewWells), intent(in) :: arrWellNodes

	type(SGAPARAMSTRUCT), intent(inout) :: sgaParam

	!variables
	integer :: i, k, nFlags, nBits, nSumBits, nVarCount, nEachStressVars

	! nVarCount stores the maximum no. of parameters in the chromosome
	! which is, for each stress period, the new well locations, pumping rates for new extration wells, 
	! installation flags for new wells, pumping rates for old extraction wells, installation flags for old wells.
	nEachStressVars = nNewWells + nNewExtWells + nNewWells + nOldExtWells + nOldWells
	nVarCount = nStressPeriod * nEachStressVars

	!allocatable memory for sgaParam
	allocate( sgaParam%arrVars(sgaParam%nPopSize, nVarCount), sgaParam%arrVarIndex(nVarCount) )
	allocate( sgaParam%arrVarMin(nVarCount), sgaParam%arrVarMax(nVarCount), sgaParam%arrVarStride(nVarCount) )

	!allocate the chrosome bits for each decision variables, first well location number, 
	!then pumping rates, then installation flag

	do k = 1, nStressPeriod
		!for well location number
		do i = 1, nNewWells 
			nBits = ceiling( log10( dble(arrWellNodes(i)) ) / log10( 2.0 ) )
			sgaParam%arrVarIndex(i + (k-1)*nEachStressVars ) = nBits
			call assert( (2.0**nBits) >= arrWellNodes(i) )
		enddo

		!for pumping rates
		do i=nNewWells + 1, nNewWells + nNewExtWells
			sgaParam%arrVarIndex(i + (k-1)*nEachStressVars) = 7
		enddo

		!for all other flags, bit length is always 1
		do i=nNewWells+nNewExtWells+1, 2*nNewWells + nNewExtWells
			sgaParam%arrVarIndex(i + (k-1)*nEachStressVars) = 1
		enddo

		do i=2*nNewWells+nNewExtWells+1, 2*nNewWells+nNewExtWells + nOldExtWells
			sgaParam%arrVarIndex(i + (k-1)*nEachStressVars) = 7
		enddo

		do i = 2*nNewWells+nNewExtWells + nOldExtWells+1, 2*nNewWells+nNewExtWells + nOldExtWells + nOldWells
			sgaParam%arrVarIndex(i + (k-1)*nEachStressVars) = 1
		enddo
		sgaParam%arrVarMin((k-1)*nEachStressVars+1:k*nEachStressVars) = (/(1.0, i=1, nNewWells), (0.0, i=1,(nEachStressVars-nNewWells))/)
		sgaParam%arrVarMax((k-1)*nEachStressVars+1:k*nEachStressVars) = (/(dble(arrWellNodes(i)), i=1, nNewWells), (1.d0, i=1,(nEachStressVars-nNewWells))/)
	enddo

	!now in arrVarIndex it's the length of bits needed to code the variable
	!calculate the stride, which is minimum discretization of each binary value 
	do i=1, nVarCount
		sgaParam%arrVarStride(i) = (sgaParam%arrVarMax(i)-sgaParam%arrVarMin(i)) / (2**sgaParam%arrVarIndex(i)-1)
	enddo

	!construct the chrosome index array
	nSumBits = 0
	do i=1, nVarCount
		nBits = sgaParam%arrVarIndex(i)
		sgaParam%arrVarIndex(i) = nSumBits + 1
		nSumBits = nSumBits + nBits
	enddo

!	allocate( sgaParam%arrPopInj(max(sgaParam%nPopSize/20,3), nSumBits), sgaParam%arrFitInj(max(sgaParam%nPopSize/20,3)) )
!	allocate( sgaParam%arrPop(sgaParam%nPopSize, nSumBits), sgaParam%arrFit(sgaParam%nPopSize), sgaParam%arrFlags(sgaParam%nPopSize) )

	allocate( sgaParam%arrPopInj(max(sgaParam%nPopSize/20,3), nSumBits), sgaParam%arrFitInj(max(sgaParam%nPopSize/20,3)) )
	allocate( sgaParam%arrCostsInj(max(sgaParam%nPopSize/20,3)), sgaParam%arrCostStdsInj(max(sgaParam%nPopSize/20,3)) )
	allocate( sgaParam%arrViolsInj(max(sgaParam%nPopSize/20,3)), sgaParam%arrViolStdsInj(max(sgaParam%nPopSize/20,3)) )
	allocate( sgaParam%arrPop(sgaParam%nPopSize, nSumBits), sgaParam%arrFit(sgaParam%nPopSize), sgaParam%arrFlags(sgaParam%nPopSize) )

	allocate( sgaParam%arrFitStds(sgaParam%nPopSize), sgaParam%arrCosts(sgaParam%nPopSize), sgaParam%arrViols(sgaParam%nPopSize) )
	allocate( sgaParam%arrCostStds(sgaParam%nPopSize), sgaParam%arrViolStds(sgaParam%nPopSize) )
	allocate( sgaParam%arrReliabs(sgaParam%nPopSize) )

	!new parameters for sampling.
	allocate( sgaParam%arrRealizations(sgaParam%nPopSize, sgaParam%nSamples) )
	allocate( sgaParam%arrMoments(g_nFields), sgaParam%arrMomentIds(g_nFields) )

	sgaParam%arrFitInj = 1d100
	sgaParam%nChromBits = nSumBits
	sgaParam%nVarCount = nVarCount
end subroutine InitGA

subroutine ClearGA( sgaParam )
use sga
implicit none
	!arguments
	type(SGAPARAMSTRUCT), intent(inout) :: sgaParam

	call assert( allocated(sgaParam%arrPop) .and. allocated(sgaParam%arrFit) )
	call assert( allocated(sgaParam%arrVars) .and. allocated(sgaParam%arrVarIndex) )
	call assert( allocated(sgaParam%arrVarMin) .and. allocated(sgaParam%arrVarMax) )
	call assert( allocated(sgaParam%arrVarStride) )

	deallocate( sgaParam%arrPopInj, sgaParam%arrFitInj )
	deallocate( sgaParam%arrCostsInj, sgaParam%arrCostStdsInj, sgaParam%arrViolsInj, sgaParam%arrViolStdsInj )
	deallocate( sgaParam%arrPop, sgaParam%arrFit, sgaParam%arrFlags )
	deallocate( sgaParam%arrVars, sgaParam%arrVarIndex )
	deallocate( sgaParam%arrVarMin, sgaParam%arrVarMax, sgaParam%arrVarStride )
	deallocate( sgaParam%arrRealizations )
	deallocate( sgaParam%arrReliabs )
	deallocate( sgaParam%arrFitStds, sgaParam%arrCosts, sgaParam%arrViols, sgaParam%arrCostStds, sgaParam%arrViolStds )
	deallocate( sgaParam%arrMoments, sgaParam%arrMomentIds )
end subroutine ClearGA

!The function is modified to return the normalized violation (rViol)
!arrVars is optional parameter, if it is existing, it will contain the (x,y) locations and pumping rates for new wells plus the pumping rates for the old wells
subroutine PrepareFitness( id, sgaParam, MB1, MB2, rViol, arrRetVars )
use sga
implicit none
	!argument
	integer :: id
	double precision, intent(out) :: MB1, MB2, rViol
	type(SGAPARAMSTRUCT), intent(inout) :: sgaParam
	double precision, dimension(:), intent(out), optional :: arrRetVars

!	sgaParam%arrVars(id,:) = (/17.0,31.0,6.0,29.0,31.0,21.0,14.0,0.8016,   0.7019,   0.7732,   0.5386,   1.0,   1.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.2874,   0.9961,   0.6530,   0.7429,   0.0,   1.0,   1.0,   0.0,   0.0,   1.0,   1.0/)
!	sgaParam%arrVars(id,:) = (/16.6825,27.4127,6.0000,  29.0000,  32.0000,  21.0000,  14.0000,   0.9130,   0.5142,   0.2884,   0.6598,   1.0000,   1.0000,   0.0000,   0.0000,   0.0000,   0.0000,   1.0000,   0.7889,   0.9941,   0.9658,   0.8065,   0.0000,   1.0000,   1.0000,   0.0000,   0.0000 ,  1.0000,   1.0000/)
!	sgaParam%arrVars(id,:) = (/5.0,17.0,18.0,23.0,26.0,23.0,31.0,3.0,0.5347,0.8299,0.8847,0.4262,1.0,1.0,0.0,0.0,0.0,0.0,0.0,0.8768,0.8788,0.8905,0.2571,0.0,1.0,1.0,0.0,0.0,1.0,1.0/)
!	sgaParam%arrVars(id,:) = (/5.0,16.0,18.0,24.0,26.0,23.0,31.0,3.0,0.5347,0.8309,0.8768,0.0362,1.0,1.0,0.0,0.0,0.0,0.0,0.0,0.7752,0.9091,0.8905,0.2571,0.0,1.0,1.0,0.0,0.0,1.0,1.0/)
!	sgaParam%arrVars(id,:) = (/15.0,46.0,17.0,29.0,17.0,30.0,20.0,0.4286,0.7460,0.2857,0.3492,1.0,1.0,0.0,0.0,0.0,0.0,1.0,0.2540,0.8730,0.9841,0.3651,0.0,1.0,1.0,0.0,0.0,0.0,0.0/)
!	sgaParam%arrVars(id,:) = (/16.0,18.0,23.0,26.0,23.0,31.0,3.0,0.5347,0.8348,0.9081,0.0362,1.0,1.0,0.0,0.0,0.0,0.0,0.0,0.2581,0.9091,0.9687,0.2571,0.0,1.0,1.0,0.0,0.0,1.0,1.0/)
!	sgaParam%arrVars(id,:) = (/16.6825,24.1111,4.0000,18.0000,1.0000,29.0000,29.0000,0.8798,0.8182,0.8192,0.1310,1.0000,1.0000,0.0000,0.0000,0.0000,0.0000,0.0000,0.1789,0.9228,0.5318,0.6540,0.0000,1.0000,1.0000,0.0000,0.0000,1.0000,1.0000/)
!	sgaParam%arrVars(id,:) = (/15.8571,17.5079,16.0000, 6.0000,14.0000,13.0000, 3.0000, 0.9208, 0.9619, 0.5230, 0.1369, 1.0000, 1.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.3206, 0.9208, 0.3812, 0.8280, 0.0000, 1.0000, 1.0000, 0.0000, 0.0000, 1.0000, 1.0000/)
!	sgaParam%arrVars(id,:) = (/14.2063,15.8571,13.0000,23.0000, 6.0000,18.0000, 4.0000, 0.3803, 0.8837, 0.5142, 0.1320, 1.0000, 1.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.2845, 0.9013, 0.9707, 0.4379, 0.0000, 1.0000, 1.0000, 0.0000, 0.0000, 1.0000, 1.0000/)
!	sgaParam%arrVars(id,:) = (/14.2063,15.8571,13.0000,23.0000, 6.0000,18.0000, 3.0000, 0.3803, 0.8837, 0.0147, 0.1310, 1.0000, 1.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.9413, 0.9013, 0.9697, 0.1916, 0.0000, 1.0000, 1.0000, 0.0000, 0.0000, 1.0000, 1.0000/)
!	sgaParam%arrVars(id,:) = (/15.8571,14.2063,31.0000,29.0000, 7.0000, 2.0000, 8.0000, 0.6901, 0.5630, 0.5552, 0.7107, 1.0000, 1.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.4106, 0.9687, 0.9130, 0.6031, 0.0000, 1.0000, 1.0000, 0.0000, 0.0000, 1.0000, 1.0000/)
!	sgaParam%arrVars(id,:) = (/15.8571,17.5079, 1.0000,23.0000,26.0000,24.0000, 2.0000, 0.5337, 0.6608, 0.8524, 0.5943, 1.0000, 1.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.4037, 0.9756, 0.9775, 0.3001, 0.0000, 1.0000, 1.0000, 0.0000, 0.0000, 1.0000, 1.0000/)
	if( present(arrRetVars) )then
		call wrapPrepFunc( sgaParam%arrVars(id,:), sgaParam%nVarCount, g_nNewWells, g_nNewExtWells, g_nOldWells, g_nOldExtWells, g_nStressPeriod, MB1, MB2, rViol, arrRetVars )
	else
		call wrapPrepFunc( sgaParam%arrVars(id,:), sgaParam%nVarCount, g_nNewWells, g_nNewExtWells, g_nOldWells, g_nOldExtWells, g_nStressPeriod, MB1, MB2, rViol )
	endif
end subroutine PrepareFitness

subroutine wrapPrepFunc( arrVars, nVarCount, nNewWells, nNewExtWells, nOldWells, nOldExtWells, nStressPeriod, MB1, MB2, rViol, arrRetVars)
implicit none
	!argument
	integer, intent(in) :: nVarCount, nNewWells, nNewExtWells, nOldWells, nOldExtWells, nStressPeriod
	double precision, dimension(nVarCount), intent(in) :: arrVars
	double precision, intent(out) :: MB1, MB2, rViol
	double precision, dimension(:), intent(out), optional :: arrRetVars

	!variables
	integer :: nNewExts, nOldExts, nNewInjs, nOldInjs, nTotExts, nTotInjs
	integer :: i, j, k, id, knum, nEachStressVars
	integer :: nw1, nw2
	double precision :: rTotalExtQ, rNewInjQ, rOldInjQ, QQext
	double precision :: alpha, rDeltaQInj, rDeltaQExt

	!helper array to decode the location id
	integer :: arrNewWellLocIds( nNewWells )
	integer :: arrNewBasins( nStressPeriod )

	!helper array to decode the flags and Q from the long var array
	double precision :: arrNewWellFlags( nNewWells ), arrOldWellFlags( nOldWells )
	double precision :: arrNewWellQ( nNewWells ), arrOldWellQ( nOldWells )

	call OpenOutputFile( hFileOptRes, strFileOptRes )
	call OpenOutputFile( hFileOptWel, strFileOptWel )

	nEachStressVars = 2*nNewWells + nNewExtWells + nOldWells + nOldExtWells

	!do while for each stress period
	do k = 1, nStressPeriod
		id = (k-1)*nEachStressVars			!id is the start offset of the variables for this period

		!the arrVars structure is { new_well_locations(nNewWells), new_ext_well_Q(nNewExtWells), new_well_flag(nNewWells), old_ext_well_Q(nOldExtWells), old_well_flag(nOldWells) }
		arrNewWellQ = (/arrVars(id+nNewWells+1 : id+nNewWells+nNewExtWells), (1.d0, i=1,nNewWells-nNewExtWells) /)
		arrOldWellQ = (/arrVars(id+2*nNewWells+nNewExtWells+1 : id+2*nNewWells+nNewExtWells+nOldExtWells), (1.d0, i=1, nOldWells-nOldExtWells) /)
		arrNewWellFlags = arrVars(id+nNewWells+nNewExtWells+1 : id+2*nNewWells+nNewExtWells )
		arrOldWellFlags = arrVars( id+2*nNewWells+nNewExtWells+nOldExtWells+1 : id+nEachStressVars )

		!decode well locations and pumping rates
		do i=1, nNewWells
			arrNewWellLocIds(i) = int(anint(arrVars(i)))
			arrNewWellQ(i) = g_parrNewWells(i)%parrLocs( arrNewWellLocIds(i) )%rMaxPumpRate * arrNewWellFlags(i) * arrNewWellQ(i)
		end do
		do i=1, nOldWells
			arrOldWellQ(i) = g_parrOldWells(i)%parrLocs(1)%rMaxPumpRate * arrOldWellFlags(i) * arrOldWellQ(i)
		end do

		!compute the total extration rate.
		nNewExts = 0
		nOldExts = 0
		rTotalExtQ = 0.0
		do i=1, nNewExtWells
			nNewExts = nNewExts + arrNewWellFlags(i)
			rTotalExtQ = rTotalExtQ + arrNewWellQ(i)
		end do
		do i=1, nOldExtWells
			nOldExts = nOldExts + arrOldWellFlags(i)
			rTotalExtQ = rTotalExtQ + arrOldWellQ(i)
		end do

		!compute the installed old and new injection wells
		nNewInjs = 0
		nOldInjs = 0
		do i=nNewExtWells+1, nNewWells
			nNewInjs = nNewInjs + arrNewWellFlags(i)
		enddo
		do i=nOldExtWells+1, nOldWells
			nOldInjs = nOldInjs + arrOldWellFlags(i) * g_parrOldWells(i)%nNodes
		enddo

		!compute the injection rate assuming only one well operates for the new injection wells
		rNewInjQ = 0
		rOldInjQ = 0
		do i=nOldExtWells+1, nOldWells
			rOldInjQ = rOldInjQ + arrOldWellQ(i) * g_parrOldWells(i)%nNodes
		enddo
		do i=nNewExtWells+1, nNewWells
			rNewInjQ = rNewInjQ + arrNewWellQ(i)
		enddo

		!compute the number of injection wells installed at each location
		knum = 0
		alpha = 0
		rDeltaQInj = Q_F2G( rTotalExtQ )
		if( nNewInjs+nOldInjs > 0 )then
			if( rNewInjQ>0 )then
				knum = ceiling( (rTotalExtQ / g_rMaxAlpha - rOldInjQ) / (2*rNewInjQ) )
			else
				knum = 4
			endif
			knum = min( max( 0, knum ), 4 )
			if( 2*knum*rNewInjQ+rOldInjQ > 0 )alpha = rTotalExtQ / (2*knum*rNewInjQ + rOldInjQ)

			rDeltaQInj = Q_F2G( (alpha - g_rMaxAlpha) * ( 2 * knum*rNewInjQ + rOldInjQ ) )
			if( rDeltaQInj < 0 )rDeltaQInj = 0
		endif

		nw1 = nint( arrVars(1)*arrNewWellFlags(1) )
		nw2 = nint( arrVars(2)*arrNewWellFlags(2) )

		!QQext=parent(indiv,newwells+1)*parent(indiv,newwells+5)*maxpumpnew(1,nint(parent(indiv,1)))+parent(indiv,newwells+2)*parent(indiv,newwells+5)*maxpumpnew(2,nint(parent(indiv,1)))
		QQext = arrNewWellQ(1) + arrNewWellQ(2)

		rDeltaQExt = 0
		if( nw1 == nw2 )then
!correcting for the constraint but
!			rDeltaQExt = Q_F2G( QQext ) - g_parrNewWells(1)%parrLocs( int(anint(arrVars(1))) )%rMaxPumpRate
			rDeltaQExt = QQext - g_parrNewWells(1)%parrLocs( int(anint(arrVars(1))) )%rMaxPumpRate
			if( rDeltaQExt < 0 )rDeltaQExt = 0
		endif
		rDeltaQExt = Q_F2G( rDeltaQExt )
		MB1 = rDeltaQext + rDeltaQinj
		MB2 = 100.0*( Q_F2G(rTotalExtQ)-1170.d0)

		if(MB2.lt.0.d0) MB2=0.d0
!		MB=MB1+MB2

		!compute the normalized violations.
		rViol = 0
		if( MB1 > 0 )then
			rViol = rViol + MB1 / 1300
		endif
		if( MB2 > 0 )then
			rViol = rViol + ( Q_F2G(rTotalExtQ) - 1170.d0 )/1170.d0
		endif
		
		write( hFileOptRes, '(I3)' )2*knum*nNewInjs + nOldInjs + nNewExts + nOldExts
		write( hFileOptWel, '(I3)' )100
		write( hFileOptWel, '(I3)' )2*knum*nNewInjs + nOldInjs + nNewExts + nOldExts
		call WriteWell( arrNewWellLocIds, arrNewWellFlags, arrOldWellFlags, arrNewWellQ, arrOldWellQ, nNewWells, nOldWells, knum, alpha, hFileOptRes, fmt_res )
		call WriteWell( arrNewWellLocIds, arrNewWellFlags, arrOldWellFlags, arrNewWellQ, arrOldWellQ, nNewWells, nOldWells, knum, alpha, hFileOptWel, fmt_wel )

		if( present(arrRetVars) )then
			j = (3*nNewWells+nOldWells)*(k-1) + 1
			do i=1, nNewWells
				arrRetVars(j) = g_parrNewWells(i)%parrLocs( arrNewWellLocIds(i) )%x
				arrRetVars(j+1) = g_parrNewWells(i)%parrLocs( arrNewWellLocIds(i) )%y
				j = j + 2
			end do
			do i=1, nNewExtWells
				arrRetVars(j) = arrNewWellQ(i) / g_parrNewWells(i)%parrLocs(2)%rMaxPumpRate
				j = j + 1
			enddo
			do i=nNewExtWells+1, nNewWells
				arrRetVars(j) = arrNewWellQ(i)*alpha*2.0*knum / g_parrNewWells(i)%parrLocs(2)%rMaxPumpRate
				j = j + 1
			enddo

			do i=1, nOldExtWells
				arrRetVars(j) = arrOldWellQ(i) / g_parrOldWells(i)%parrLocs(1)%rMaxPumpRate
				j = j + 1
			enddo
			do i=nOldExtWells+1, nOldWells
				arrRetVars(j) = arrOldWellQ(i)*alpha*g_parrOldWells(i)%nNodes / g_parrOldWells(i)%parrLocs(1)%rMaxPumpRate
				j = j + 1
			enddo
		endif

	enddo

	!calculate the number of new basins in each stress period
	arrNewBasins = 0

	!calculate new basins in the first stress period
	arrNewWellFlags = arrVars( nNewWells+nNewExtWells+1 : 2*nNewWells+nNewExtWells )
	do i = 1, nNewWells
		if( g_parrNewWells(i)%nPumpFlag==2 .and. arrNewWellFlags(i)==1 )then
			arrNewBasins(1) = arrNewBasins(1) + 1
		endif
	enddo

	!calculate the remaining stress period
	do k = 2, nStressPeriod
inner:	do i = 1, nNewWells
			if( g_parrNewWells(i)%nPumpFlag==2 .and. arrVars( nNewWells+nNewExtWells+i+(k-1)*nEachStressVars )==1 )then
				do j=1, k-1
					id = (j-1)*nEachStressVars
					if( arrVars(nNewWells+nNewExtWells+i+id)==1.0 .and. arrVars(i+id)==arrVars(i+(k-1)*nEachStressVars) )cycle inner
				enddo
			endif
			if( g_parrNewWells(i)%nPumpFlag==2 )arrNewBasins(k) = arrNewBasins(k)+1
		enddo inner
	enddo

	!Writing the number of new basin for each stress period in the uma file
	write( hFileOptRes, * )(arrNewBasins(i), i=1, nStressPeriod)

	close( hFileOptRes )
	close( hFileOptWel )
end subroutine wrapPrepFunc

	subroutine WriteWell( arrNewWellLocIds, arrNewWellFlags, arrOldWellFlags, arrNewWellQ, arrOldWellQ, nNewWells, nOldWells, knum, alpha, hFile, strfmt )
	implicit none
		!argument
		integer, intent(in) :: knum, nNewWells, nOldWells, hFile
		integer, dimension(:), intent(in) :: arrNewWellLocIds
		double precision, intent(in) :: alpha
		double precision, dimension(:), intent(in) :: arrNewWellFlags, arrOldWellFlags, arrNewWellQ, arrOldWellQ
		character(*), intent(in) :: strfmt

		!well location offset for multiple wells
		integer :: i, k, id
		integer :: arrWellOffset(2,8) = reshape( (/0,0, 1,0, 0,1, 1,1, 2,0, 2,1, 3,0, 3,1/), (/2,8/) )

		do i = 1, nNewWells
			id = arrNewWellLocIds(i)
			if( g_parrNewWells(i)%nPumpFlag==1 .and. arrNewWellFlags(i)==1 )then
				!this is a extraction well
				if( hFile==hFileOptRes )write( hFile, '(I3)' )-i
				write( hFile, strfmt )g_parrNewWells(i)%parrLocs(id)%z, g_parrNewWells(i)%parrLocs(id)%y, &
											& g_parrNewWells(i)%parrLocs(id)%x, -arrNewWellQ(i)
			endif
			if( g_parrNewWells(i)%nPumpFlag==2 .and. arrNewWellFlags(i)==1 )then
				do k=1, 2*knum
					if( hFile==hFileOptRes )write( hFile, '(I3)' )i
					write( hFile, strfmt )g_parrNewWells(i)%parrLocs(id)%z, g_parrNewWells(i)%parrLocs(id)%y + arrWellOffset(1,k), &
											& g_parrNewWells(i)%parrLocs(id)%x + arrWellOffset(2,k), alpha * arrNewWellQ(i)
				enddo
			endif
		enddo

		do i = 1, nOldWells
			if( g_parrOldWells(i)%nPumpFlag==1 .and. arrOldWellFlags(i)==1 )then
				!this is a extraction well
				if( hFile==hFileOptRes )write( hFile, '(I3)' )-i
				write( hFile, strfmt )g_parrOldWells(i)%parrLocs(1)%z, g_parrOldWells(i)%parrLocs(1)%y, &
											& g_parrOldWells(i)%parrLocs(1)%x, -arrOldWellQ(i)
			endif
			if( g_parrOldWells(i)%nPumpFlag==2 .and. arrOldWellFlags(i)==1 )then
				do k=1, g_parrOldWells(i)%nNodes
					if( hFile==hFileOptRes )write( hFile, '(I3)' )i
					write( hFile, strfmt )g_parrOldWells(i)%parrLocs(k)%z, g_parrOldWells(i)%parrLocs(k)%y, &
											& g_parrOldWells(i)%parrLocs(k)%x, alpha * arrOldWellQ(i)
				enddo
			endif
		enddo
		100		FORMAT (I3)
		101		FORMAT (F25.5)
 	end subroutine WriteWell


subroutine Chrom_Normalize( arrChrom )
use sga
implicit none
	integer, dimension(*), intent(inout) :: arrChrom

	!variables
	double precision, dimension( g_sgaParam%nVarCount ) :: arrVars
	double precision :: arrNewWellFlags( g_nNewWells ), arrOldWellFlags( g_nOldWells )
	double precision :: arrNewWellQ( g_nNewExtWells ), arrOldWellQ( g_nOldExtWells )
	integer :: arrNewWellLocIds( g_nNewWells )

	integer :: i
	
	call DecodeOneEx( g_sgaParam, arrChrom, arrVars )

	!the arrVars structure is { new_well_locations(nNewWells), new_ext_well_Q(nNewExtWells), new_well_flag(nNewWells), old_ext_well_Q(nOldExtWells), old_well_flag(nOldWells) }
	arrNewWellFlags = arrVars(g_nNewWells+g_nNewExtWells+1 : 2*g_nNewWells+g_nNewExtWells )
	arrOldWellFlags = arrVars( 2*g_nNewWells+g_nNewExtWells+g_nOldExtWells+1 : 2*g_nNewWells + g_nNewExtWells + g_nOldWells + g_nOldExtWells )
	arrNewWellQ = arrVars(g_nNewWells+1 : g_nNewWells+g_nNewExtWells)
	arrOldWellQ = arrVars(2*g_nNewWells+g_nNewExtWells+1 : 2*g_nNewWells+g_nNewExtWells+g_nOldExtWells)

	!decode well locations and pumping rates, note, the Qs are still the coefficient, not actual pumping rates
	do i=1, g_nNewWells
		arrNewWellLocIds(i) = int(anint(arrVars(i)))
		!the well is disabled, always set the location to 1
		if( arrNewWellFlags(i)<0.5 )then
			arrNewWellLocIds(i) = 1
		endif
	end do
	do i=1,g_nNewExtWells
		arrNewWellQ(i) = arrNewWellFlags(i) * arrNewWellQ(i)
	enddo
	do i=1, g_nOldExtWells
		arrOldWellQ(i) = arrOldWellFlags(i) * arrOldWellQ(i)
	end do

	!copy normalized variables back to the arrVars.
	arrVars = (/ dble(arrNewWellLocIds), arrNewWellQ, arrNewWellFlags, arrOldWellQ, arrOldWellFlags /)

	call EncodeOneEx( g_sgaParam, arrVars, arrChrom )
!	call DecodeOneEx( g_sgaParam, arrChrom, arrVars )

end subroutine

end module umacase

integer function CompareChroms( arrChrom1, arrChrom2 )
use umacase
use sga
implicit none
	!arguments
	integer, dimension(*), intent(in) :: arrChrom1, arrChrom2

	!variables
	double precision, dimension( g_sgaParam%nVarCount ) :: arrVars1, arrVars2
	double precision :: arrNewWellFlags1( g_nNewWells ), arrOldWellFlags1( g_nOldWells )
	double precision :: arrNewWellFlags2( g_nNewWells ), arrOldWellFlags2( g_nOldWells )
	double precision :: arrNewWellQ1( g_nNewExtWells ), arrOldWellQ1( g_nOldExtWells )
	double precision :: arrNewWellQ2( g_nNewExtWells ), arrOldWellQ2( g_nOldExtWells )
	integer :: i
	
	call DecodeOneEx( g_sgaParam, arrChrom1, arrVars1 )
	call DecodeOneEx( g_sgaParam, arrChrom2, arrVars2 )

	arrNewWellFlags1 = arrVars1(g_nNewWells+g_nNewExtWells+1 : 2*g_nNewWells+g_nNewExtWells )
	arrOldWellFlags1 = arrVars1( 2*g_nNewWells+g_nNewExtWells+g_nOldExtWells+1 : 2*g_nNewWells + g_nNewExtWells + g_nOldWells + g_nOldExtWells )
	arrNewWellFlags1 = arrVars2(g_nNewWells+g_nNewExtWells+1 : 2*g_nNewWells+g_nNewExtWells )
	arrOldWellFlags1 = arrVars2( 2*g_nNewWells+g_nNewExtWells+g_nOldExtWells+1 : 2*g_nNewWells + g_nNewExtWells + g_nOldWells + g_nOldExtWells )
	!check the well locations
	do i = 1, g_nNewWells
		if( anint(arrVars1(i))*arrNewWellFlags1(i) /= anint(arrVars2(i))*arrNewWellFlags2(i) )then
			CompareChroms = 0
			return
		endif
	enddo

	do i=1, g_nNewWells
		if( abs( arrNewWellFlags1(i)-arrNewWellFlags2(i) )<EPS )then
			CompareChroms = 0
			return
		endif
	enddo

	do i=1, g_nOldWells
		if( abs( arrOldWellFlags1(i)-arrOldWellFlags2(i) )<EPS )then
			CompareChroms = 0
			return
		endif
	enddo

	!check the new well pumping rates
	arrNewWellQ1 = arrVars1(g_nNewWells+1 : g_nNewWells+g_nNewExtWells)
	arrOldWellQ1 = arrVars1(2*g_nNewWells+g_nNewExtWells+1 : 2*g_nNewWells+g_nNewExtWells+g_nOldExtWells)
	arrNewWellQ2 = arrVars2(g_nNewWells+1 : g_nNewWells+g_nNewExtWells)
	arrOldWellQ2 = arrVars2(2*g_nNewWells+g_nNewExtWells+1 : 2*g_nNewWells+g_nNewExtWells+g_nOldExtWells)

	do i = 1, g_nNewExtWells
		if( abs( arrNewWellQ1(i)*arrNewWellFlags1(i) - arrNewWellQ2(i)*arrNewWellFlags2(i) )>EPS )then
			CompareChroms = 0
			return
		endif
	enddo

	do i = 1, g_nOldExtWells
		if( abs( arrOldWellQ1(i)*arrOldWellFlags1(i) - arrOldWellQ2(i)*arrOldWellFlags2(i) )>EPS )then
			CompareChroms = 0
			return
		endif
	enddo

	CompareChroms = 1
end function CompareChroms
