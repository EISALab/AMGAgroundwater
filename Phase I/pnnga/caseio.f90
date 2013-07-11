module CASEIO
use casewell
use STD_HELPER
implicit none

integer, parameter :: hCondFile=12
integer, parameter :: hRiskFile=14
integer, parameter :: hWellLocFile = 16
integer, parameter :: hOptFile	= 18

character(*), parameter :: strCondFile = 'cond_16_8.dat'
character(*), parameter :: strRiskFile = 'riskdists.dat'
character(*), parameter :: strWellLocFile = 'rwelloc.dat'
character(*), parameter :: strOptFile = 'gainp.dat'


contains

subroutine InitCase
implicit none
	call ReadCondInfo
	call ReadWellInfo
	call ReadRiskInfo
	call ReadOptInfo

	allocate( g_arrPumpRates(g_nRemWells), g_arrWellLocs(g_nRemWells) )
	allocate( g_arrHeads(g_nRemWells), g_arrOldHeads(g_nRemWells) )
	allocate( g_arrFact(g_nRemWells) )
	allocate( g_arrViolaH(2*g_nRemWells) )
	allocate( g_arrTmpX(2*g_nRemWells) )

end subroutine InitCase

subroutine CleanCase
implicit none
	integer :: i

	deallocate( g_arrPumpRates, g_arrWellLocs )
	deallocate( g_arrHeads, g_arrOldHeads )
	deallocate( g_arrViolaH )
	deallocate( g_arrFact )
	do i=1, g_nRemWells
		deallocate( g_parrWellInfos(i)%parrLocs )
	end do
	deallocate( g_parrWellInfos )
	deallocate( g_arrTmpX )
end subroutine CleanCase


!################################################################################	                
! Added by Meghna Babbar July 07, 2001.
! This is the subroutine that reads the data for the remediation wells. 
! the total no. of possible locations for the remediation wells and their pumping flags
! decide the length of the string for the GA.
! Data is read from rwelloc.dat, which is a single file that stores the possible locations of all remediation wells.
! In serial order, it stores the 'name of the well', 'injection/extraction/both pumping flag'(that decides what kind 
! of pumping well it is),'maximum pumping capacity of a well', 'possible number of nodal locations for the well', and finally followed by the 'actual list of
! possible locations'
! pumpflag(noremwells) = array of flags for the remediation wells, that decides what kind of well it is
!                        pumpflag=1 : injection
!                        pumpflag=2 : extraction
!                        pumpflag=3 : either of the above
subroutine ReadWellInfo
implicit none
	integer :: nRemWellCount
	integer :: i,j
	type(WellLoc), dimension(:), pointer :: parrLocs
	
	if( associated(g_parrWellInfos) )then
		return
	endif
	
	!open the 'wellloc.dat'
	call OpenInputFile( hWellLocFile, strWellLocFile )
	rewind(hWellLocFile)
	!read the first line which should be the number of remediation wells
	read( hWellLocFile, * ) nRemWellCount
	allocate( g_parrWellInfos(nRemWellCount) )	!allocate enough memory to hold all the well information
	
	!reading loop to collect all the information of each well
	do i=1, nRemWellCount
		read(hWellLocFile, *) g_parrWellInfos(i)%strName
		read(hWellLocFile, *) g_parrWellInfos(i)%nPumpFlag
		read(hWellLocFile, *) g_parrWellInfos(i)%rMaxPumpRate
		read(hWellLocFile, *) g_parrWellInfos(i)%nNodes
		!allocate enough memory to hold the candidate locations of the wells
		allocate( g_parrWellInfos(i)%parrLocs( g_parrWellInfos(i)%nNodes ) )
		parrLocs => g_parrWellInfos(i)%parrLocs
		do j=1, g_parrWellInfos(i)%nNodes	!the loop reads the x,y,z of one node
			read(hWellLocFile, *) parrLocs(j)%nId, parrLocs(j)%x, parrLocs(j)%y, parrLocs(j)%z
		end do
	end do
	g_nRemWells = nRemWellCount	!store global Remediation well number
	
	close(hWellLocFile)
end subroutine ReadWellInfo

subroutine ReadCondInfo
implicit none
	integer :: i,j

	call OpenInputFile( hCondFile, strCondFile )
	rewind( hCondFile )
	
	!read all the conductivity coefficient into g_rT
	do i=1, g_nFem
		read(hCondFile,*) (g_rT((i-1)*g_nFields+j), j=1,g_nFields)
	end do
		
	close( hCondFile )
end subroutine ReadCondInfo

subroutine ReadRiskInfo
implicit none
	integer i, j
	
	call OpenInputFile( hRiskFile, strRiskFile )
	rewind( hRiskFile )
	
	read(hRiskFile, *)(g_rRiskDists(j,1),g_rRiskDists(j,2), j=1,9)
	close( hRiskFile )
end subroutine ReadRiskInfo

subroutine ReadOptInfo
implicit none
	integer :: nNsgaFlag, nSampFlag	!the two variables are used by GA, here they are useless
	
	call OpenInputFile( hOptFile, strOptfile )
	rewind( hOptFile )
	
	read( hOptFile, * ) g_rRemTime
	read( hOptFile, * ) g_nMonInt, g_nMonWells, g_nMcSamps
	read( hOptFile, * ) g_rRiskSt
	read( hOptFile, * ) nNsgaFlag, g_nModelFlag, nSampFlag	!just skip the two variables
	read( hOptFile, * ) g_nGridFlagX, g_nGridFlagY
	read( hOptFile, * ) g_nReactNo, g_nContNo
	read( hOptFile, * ) g_rCostFactor

	g_nFinGridX = g_nCoarsGridX * 3**(g_nGridFlagX-1)
	g_nFinGridY = g_nCoarsGridY * 3**(g_nGridFlagY-1)
	g_nFinGridZ = g_nCoarsGridZ
	close( hOptFile )	
end subroutine ReadOptInfo

end module CASEIO