module casewell
use STD_HELPER
implicit none

!well location definition, it's the possible node location of a remediation well (id, x, y, z)
type WellLoc
	integer :: nId
	integer :: x, y, z
end type WellLoc

!information of a remediation well
!strName : the well name
!nPumpFlag : the pumping flag, 1-injection, 2-extraction, 3-either
!rMaxPumpRate : the maximum possible pumping rate
!nNodes is the length of the array containing all the location candidates for the remediation well
!parrLocs point to an allocated array stocking all the possible nodes of the well location
type RemWellInfo
	character(10) :: strName
	integer :: nPumpFlag
	double precision :: rMaxPumpRate
	integer :: nNodes
	type(WellLoc), dimension(:), pointer :: parrLocs
end type RemWellInfo

!the array g_parrWellInfos read data from the parameter files, it stocks the well infomation
!g_nRemWells, the length of g_parrWellInfos
type(RemWellInfo), dimension(:), pointer :: g_parrWellInfos
integer :: g_nRemWells

!grid specified parameters for the case study
integer, parameter :: g_nFields = 1000
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

integer :: g_nMonInt, g_nMonWells, g_nMcSamps, g_nModelFlag, g_nGridFlagX, g_nGridFlagY, g_nReactNo, g_nContNo
double precision :: g_rRemTime, g_rRiskSt, g_rCostFactor

! monwellsets are the number of groups (for the numerical model)
! that monitoring wells are divided into.
integer, parameter :: g_nMonWellSets = 5

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

contains

! ########################################################################################################

! This is a subroutine that initializes some of the variables defined in SGAPARAMSTRUCT
! Since these variables are dependant on the case study, thus they are initialized here.
! Also note that the dimension '15' given to some of the variables should be as close to the
! actual number of variables, to avoid arraybound problems.
! This also creates the chromosome, once the details of the case study are known.
! written by Meghna Babbar, Jan 04, 2001

subroutine InitGA( sgaParam, arrPumpFlags, arrMaxPumpRate, arrWellNodes, nRemWells )
use sga
implicit none
	!arguments
	integer, intent(in) :: nRemWells
	double precision, dimension(nRemWells), intent(in) :: arrMaxPumpRate
	integer, dimension(nRemWells), intent(in) :: arrPumpFlags, arrWellNodes

	type(SGAPARAMSTRUCT), intent(inout) :: sgaParam

	!variables
	integer :: i, nFlags, nBits, nSumBits, nVarCount

	!first count how many binary variables is requried to represent the injection and extraction flag
	nFlags = 0
	do i = 1, nRemWells
		if( arrPumpFlags(i)==WELL_DONTCARE )then
			nFlags = nFlags + 1
		endif
	enddo

	! nVarCount stores the maximum no. of parameters in the chromosome
	! which is equal to well locations, pumping rates, whether to install wells,
	! and parameters for the wells which have pumpflag=3, i.e. wells that can be 
	! either injection or extraction.
	nVarCount = nFlags + nRemWells + nRemWells + nRemWells

	!allocatable memory for sgaParam
	allocate( sgaParam%arrVars(sgaParam%nPopSize, nVarCount), sgaParam%arrVarIndex(nVarCount) )
	allocate( sgaParam%arrVarMin(nVarCount), sgaParam%arrVarMax(nVarCount), sgaParam%arrVarStride(nVarCount) )

!	arrNichFlags = (/1, i=1, nVarCount/)

	!allocate the chrosome bits for each decision variables, first well location number, 
	!then pumping rates, then installation flag and then injection-extraction flag

	!for well location number
	do i = 1, nRemWells 
		nBits = ceiling( log10( dble(arrWellNodes(i)) ) / log10( 2.0 ) )
		sgaParam%arrVarIndex(i) = nBits
		call assert( (2.0**nBits) >= arrWellNodes(i) )
	enddo
	!for pumping rates
	do i=1, nRemWells
		nBits = ceiling( log10( dble(arrMaxPumpRate(i)) ) / log10( 2.0 ) )
		sgaParam%arrVarIndex(i+nRemWells) = nBits
		call assert( (2.0**nBits) >= arrMaxPumpRate(i) )
	enddo
	!for all other flags, bit length is always 1
	do i=2*nRemWells+1, nVarCount
		sgaParam%arrVarIndex(i) = 1
	enddo

	sgaParam%arrVarMin = (/(1.0, i=1, nRemWells), (0.0, i=1,(nVarCount-nRemWells))/)
	sgaParam%arrVarMax = (/(dble(arrWellNodes(i)), i=1, nRemWells), (arrMaxPumpRate(i), i=1, nRemWells), (dble(1.0), i=1, (nRemWells+nFlags))/)
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

	allocate( sgaParam%arrPopInj(max(sgaParam%nPopSize/20,3), nSumBits), sgaParam%arrFitInj(max(sgaParam%nPopSize/20,3)), sgaParam%arrFitStdsInj(max(sgaParam%nPopSize/20,3)) )
	allocate( sgaParam%arrCostsInj(max(sgaParam%nPopSize/20,3)), sgaParam%arrCostStdsInj(max(sgaParam%nPopSize/20,3)) )
	allocate( sgaParam%arrViolsInj(max(sgaParam%nPopSize/20,3)), sgaParam%arrViolStdsInj(max(sgaParam%nPopSize/20,3)) )
	allocate( sgaParam%arrPop(sgaParam%nPopSize, nSumBits), sgaParam%arrFit(sgaParam%nPopSize), sgaParam%arrFitStds(sgaParam%nPopSize), sgaParam%arrFlags(sgaParam%nPopSize) )
	allocate( sgaParam%arrRealizations(sgaParam%nPopSize, sgaParam%nSamples) )
	allocate( sgaParam%arrCosts(sgaParam%nPopSize), sgaParam%arrViols(sgaParam%nPopSize) )
	allocate( sgaParam%arrCostStds(sgaParam%nPopSize), sgaParam%arrViolStds(sgaParam%nPopSize) )
	allocate( sgaParam%arrMoments(g_nFields), sgaParam%arrMomentIds(g_nFields) )

	sgaParam%arrFitInj = 1d100
	sgaParam%nChromBits = nSumBits
	sgaParam%nVarCount = nVarCount
end subroutine InitGA

subroutine CleanGA( sgaParam )
use sga
implicit none
	!arguments
	type(SGAPARAMSTRUCT), intent(inout) :: sgaParam

	call assert( allocated(sgaParam%arrPop) .and. allocated(sgaParam%arrFit) )
	call assert( allocated(sgaParam%arrVars) .and. allocated(sgaParam%arrVarIndex) )
	call assert( allocated(sgaParam%arrVarMin) .and. allocated(sgaParam%arrVarMax) )
	call assert( allocated(sgaParam%arrVarStride) )

	deallocate( sgaParam%arrPopInj, sgaParam%arrFitInj, sgaParam%arrFitStdsInj )
	deallocate( sgaParam%arrCostsInj, sgaParam%arrCostStdsInj, sgaParam%arrViolsInj, sgaParam%arrViolStdsInj )
	deallocate( sgaParam%arrPop, sgaParam%arrFit, sgaParam%arrFitStds, sgaParam%arrFlags )
	deallocate( sgaParam%arrVars, sgaParam%arrVarIndex )
	deallocate( sgaParam%arrVarMin, sgaParam%arrVarMax, sgaParam%arrVarStride )
	deallocate( sgaParam%arrRealizations )
	deallocate( sgaParam%arrCosts, sgaParam%arrViols, sgaParam%arrCostStds, sgaParam%arrViolStds )
	deallocate( sgaParam%arrMoments, sgaParam%arrMomentIds )

end subroutine CleanGA

! ########################################################################################
subroutine ReadFactor( arrFact, nRemWells, arrWellLocs )
implicit none
	integer :: nRemWells, nWells
	double precision, dimension(nRemWells), intent(inout) :: arrFact
	integer, dimension(nRemWells), intent(in) :: arrWellLocs

	!variable
	integer i, j, nMonRemWells, idMonRemWell

	do i=1, nRemWells
		arrFact(i) = 1.0
	enddo

	call OpenInputFile( hMonRemFile, strMonRemFile )
	read( hMonRemFile, * ) nMonRemWells

	do i=1, nMonRemWells
		read( hMonRemFile, * ) idMonRemWell
		do j=1, nRemWells
			if( arrWellLocs(j)==idMonRemWell )then
				arrFact(j)=1.0
			endif
		enddo
	enddo

	close( hMonRemFile )
end subroutine ReadFactor

subroutine wrapPrepFunc( id, arrVars, nPopSize, nVarCount)
implicit none
	!argument
	integer, intent(in) :: id, nPopSize, nVarCount
	double precision, dimension(nPopSize,nVarCount), intent(in) :: arrVars
	!integer, intent(in) :: nsamps, dynflag, 

	!variables
	integer :: nPumpFlag, nWellFlags
	integer :: i, k
	double precision :: posneg
 
	! This is where the dynamic sampling is allocated if dynflag=1. The variable nsamps is used for the number
	! of dynamic samples and is allocated to mcsamps which is then sent to fitfunc.

	!if (dynflag.eq.1) then
	!  g_nMcSamps = nsamps
	!endif

	! This is where the actual location of the well in a finer grid is calculated, depending
	! upon the gridflag or level of fineness.

	!decode well locations and pumping rates
	do i=1, g_nRemWells
		g_arrWellLocs(i) = int(anint(arrVars(id,i)))
		g_arrPumpRates(i) = anint( arrVars(id, g_nRemWells+i) )
	end do

	call ReadFactor( g_arrFact, g_nRemWells, g_arrWellLocs )

	!adjust the value of pumping rates
	nWellFlags = nVarCount - 3*g_nRemWells	! well locs, well pumping rates, inj_exj flags and well install flags
	k = 1
	do i=1, g_nRemWells
		nPumpFlag = g_parrWellInfos(i)%nPumpFlag
		if( nPumpFlag.eq.2 )then
			g_arrPumpRates(i) = -g_arrPumpRates(i)
		else if( nPumpFlag.eq.3 )then
			posneg = arrVars(id, 2*g_nRemWells+k)
			if( posneg .lt. 0.5 )then
				g_arrPumpRates(i) = -g_arrPumpRates(i)
			endif

			k = k+1
		end if
	end do

	call assert( k==(nWellFlags+1) )

	!The following statements check for the installation of the wells. 
	do i=1, g_nRemWells
		if( anint(arrVars(id, 2*g_nRemWells+nWellFlags+i)).lt. 0.5 )then
			g_arrPumpRates(i) = 0
		end if
	end do

	! This is where the monitoring well's pumping rates are set to zero
	! no moniroting storage here.
end subroutine wrapPrepFunc

subroutine Chrom_Normalize( arrChrom )
use sga
implicit none
	integer, dimension(*), intent(inout) :: arrChrom

	!variables
	double precision, dimension( g_sgaParam%nVarCount ) :: arrVars
	integer, dimension( g_nRemWells ) :: arrWellLocs
	double precision, dimension(g_nRemWells) :: arrPumpRates
	integer :: i, k, nWellFlags, nVarCount, nPumpFlag, posneg

	nVarCount = g_sgaParam%nVarCount
	call DecodeOneEx( g_sgaParam, arrChrom, arrVars )

	!decode well locations and pumping rates
	do i=1, g_nRemWells
		arrWellLocs(i) = int(anint(arrVars(i)))
		arrPumpRates(i) = anint( arrVars(g_nRemWells+i) )
	end do

	!adjust the value of pumping rates
	nWellFlags = nVarCount - 3*g_nRemWells	! well locs, well pumping rates, inj_exj flags and well install flags
	k = 1
	do i=1, g_nRemWells
		nPumpFlag = g_parrWellInfos(i)%nPumpFlag
		if( nPumpFlag.eq.2 )then
			arrPumpRates(i) = -arrPumpRates(i)
		else if( nPumpFlag.eq.3 )then
			posneg = arrVars(2*g_nRemWells+k)
			if( posneg .lt. 0.5 )then
				arrPumpRates(i) = -arrPumpRates(i)
			endif

			k = k+1
		end if
	end do

	call assert( k==(nWellFlags+1) )

	!The following statements check for the installation of the wells. 
	do i=1, g_nRemWells
		if( anint(arrVars(2*g_nRemWells+nWellFlags+i)).lt. 0.5 )then
			arrPumpRates(i) = 0
		end if
	end do

	do i=1, g_nRemWells
		if( arrPumpRates(i)<0 )arrPumpRates(i) = -arrPumpRates(i)
	enddo

	!copy the normalized results back to arrVars
	!The following statements check for the installation of the wells. 
	do i=1, g_nRemWells
		if( arrVars(2*g_nRemWells+nWellFlags+i).lt. 0.5 )then
			arrVars(2*g_nRemWells+nWellFlags+i) = 0
			arrVars(i) = 1
		else
			arrVars(2*g_nRemWells+nWellFlags+i) = 1
			arrVars(i) = arrWellLocs(i)
		endif
!		arrVars(i) = arrWellLocs(i) * arrVars(2*g_nRemWells+nWellFlags+i)
		arrVars(g_nRemWells+i) = arrPumpRates(i) * arrVars(2*g_nRemWells+nWellFlags+i)
	end do

	k = 1
	do i=1, g_nRemWells
		nPumpFlag = g_parrWellInfos(i)%nPumpFlag
		if( nPumpFlag.eq.3 )then
			if( arrVars(2*g_nRemWells+nWellFlags+k)<0.5 )then
				arrVars(2*g_nRemWells+k) = 0
			endif
			k = k+1
		endif
	enddo

	call EncodeOneEx( g_sgaParam, arrVars, arrChrom )
	call DecodeOneEx( g_sgaParam, arrChrom, arrVars )

end subroutine

end module casewell