module CASEIO
use umacase
use std_helper
implicit none

integer, parameter :: hCondFile=12
integer, parameter :: hRiskFile=14
integer, parameter :: hWellFile = 16
integer, parameter :: hOptFile	= 18

character(*), parameter :: strCondFile = 'cond_16_8.dat'
character(*), parameter :: strRiskFile = 'riskdists.dat'
character(*), parameter :: strOptFile = 'gainp.dat'

character(*), parameter :: strWellFineFile = 'uma_fine.txt'
character(*), parameter :: strWellCoarseFile = 'uma_coarse.txt'

contains

subroutine InitCase
implicit none
	g_nGridFlagX = 2
	g_nGridFlagY = 2
	call ReadWellInfo( .true., 7, 7 )

!	allocate( g_arrPumpRates(g_nRemWells), g_arrWellLocs(g_nRemWells) )
!	allocate( g_arrHeads(g_nRemWells), g_arrOldHeads(g_nRemWells) )
!	allocate( g_arrFact(g_nRemWells) )
!	allocate( g_arrViolaH(2*g_nRemWells) )
!	allocate( g_arrTmpX(2*g_nRemWells) )

end subroutine InitCase


subroutine ClearCase
implicit none
	integer :: i

!	deallocate( g_arrPumpRates, g_arrWellLocs )
!	deallocate( g_arrHeads, g_arrOldHeads )
!	deallocate( g_arrViolaH )
!	deallocate( g_arrFact )
	do i=1, g_nNewWells
		deallocate( g_parrNewWells(i)%parrLocs )
	end do
	do i=1, g_nOldWells
		deallocate( g_parrOldWells(i)%parrLocs )
	enddo
	deallocate( g_parrNewWells )
	deallocate( g_parrOldWells )
!	deallocate( g_arrTmpX )
end subroutine ClearCase

!################################################################################	                
! Shengquan Yan 2004. 4. 20.
! This is the subroutine that reads the well data from uma_fine.txt or uma_coarse.txt
! The data are saved into the global strucutre g_parrWellInfos.
! bFineGrid, if true then read the fine grid well file, otherwise read from the coarse grid well file
! NOTE: the well file's first line must be the old well number and new well number.
subroutine ReadWellInfo( bFineGrid, nOldWells, nNewWells )
implicit none
	!arguments
	logical, intent(in) :: bFineGrid
	integer, intent(in) :: nOldWells, nNewWells

	!variables
	character(40) :: strWellFile
	integer :: nOldExtWells, nNewExtWells
	integer :: i,j
	type(WellLoc), dimension(:), pointer :: parrLocs
	
	if( associated(g_parrOldWells) )then
		return
	endif
	call assert( .not. associated(g_parrNewWells) )

	if( bFineGrid )then
		strWellFile = strWellFineFile
	else
		strWellFile = strWellCoarseFile
	endif
	
	!open the 'wellloc.dat'
	call OpenInputFile( hWellFile, strWellFile )
	rewind(hWellFile)
	allocate( g_parrOldWells(nOldWells) )	!allocate enough memory to hold all the well information
	allocate( g_parrNewWells(nNewWells) )
	
	!reading loop to collect all the information of each well
	do i=1, nOldWells
		read(hWellFile, *) g_parrOldWells(i)%nNodes
		!allocate enough memory to hold the candidate locations of the wells
		allocate( g_parrOldWells(i)%parrLocs( g_parrOldWells(i)%nNodes ) )
		parrLocs => g_parrOldWells(i)%parrLocs
		do j=1, g_parrOldWells(i)%nNodes	!the loop reads the x,y,z of one node
			read(hWellFile, *) parrLocs(j)%nId, parrLocs(j)%x, parrLocs(j)%y, parrLocs(j)%z, parrLocs(j)%rMaxPumpRate, g_parrOldWells(i)%nPumpFlag
			parrLocs(j)%rMaxPumpRate = Q_G2F( parrLocs(j)%rMaxPumpRate );
		end do
	end do
	g_nOldWells = nOldWells	!store global Remediation well number
	
	!reading loop to collect all the information of each well
	do i=1, nNewWells
		read(hWellFile, *) g_parrNewWells(i)%nNodes
		!allocate enough memory to hNew the candidate locations of the wells
		allocate( g_parrNewWells(i)%parrLocs( g_parrNewWells(i)%nNodes ) )
		parrLocs => g_parrNewWells(i)%parrLocs
		do j=1, g_parrNewWells(i)%nNodes	!the loop reads the x,y,z of one node
			read(hWellFile, *) parrLocs(j)%nId, parrLocs(j)%x, parrLocs(j)%y, parrLocs(j)%z, parrLocs(j)%rMaxPumpRate, g_parrNewWells(i)%nPumpFlag
			parrLocs(j)%rMaxPumpRate = Q_G2F( parrLocs(j)%rMaxPumpRate );
		end do
	end do
	g_nNewWells = nNewWells	!store global Remediation well number

	!count how many extraction wells in old and new
	nOldExtWells = 0
	do i=1, nOldWells
		if( g_parrOldWells(i)%nPumpFlag==1 )nOldExtWells = nOldExtWells+1
	enddo
	g_nOldExtWells = nOldExtWells

	nNewExtWells = 0
	do i=1, nNewWells
		if( g_parrNewWells(i)%nPumpFlag==1 )nNewExtWells = nNewExtWells + 1
	enddo
	g_nNewExtWells = nNewExtWells

	close(hWellFile)
end subroutine ReadWellInfo

end module CASEIO