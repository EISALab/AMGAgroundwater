module pdeio
use casewell
use STD_HELPER
implicit none

!DEC$ DEFINE COST_VERSION=1

integer, parameter :: hMod2Well = 812
integer, parameter :: hMod2Bcf	= 814
integer, parameter :: hMod2Bas	= 815
integer, parameter :: hRt3dBtn	= 816
integer, parameter :: hRt3dSsm	= 817

character(*), parameter :: strMod2Well	= 'mod2.wel'
character(*), parameter :: strMod2Bcf	= 'mod2.bcf'
character(*), parameter :: strMod2Bas	= 'mod2.bas'
character(*), parameter :: strRt3dBtn	= 'rt3d2.btn'
character(*), parameter :: strRt3dSsm	= 'rt3d2.ssm'

integer, parameter :: hMonLoc	= 823
integer, parameter :: hConOut	= 822
integer, parameter :: hHeadOld	= 826
integer, parameter :: hHeadNew	= 826

character(*), parameter :: strMonLoc	= 'monwelloc.dat'
character(*), parameter :: strConOut	= 'newconc.out'
character(*), parameter :: strHeadOld	= 'headold.out'
character(*), parameter :: strHeadNew	= 'headnew.out'

contains

!PreNumModel is called just before Modflow and Rt3d is called to prepare the input parameters
!for the two numerical models
!the original code is inputrt3d() in SGA
subroutine PreNumModel( arrT, rTime, nGridX, nGridY, nGridZ )
implicit none
	!arguments
	double precision, dimension(:), intent(in) :: arrT
	double precision, intent(in) :: rTime
	integer, intent(in) :: nGridX, nGridY, nGridZ

	call WriteMod2Well
	call WriteMod2Bcf(arrT, nGridX, nGridY, nGridZ)
	call WriteRt3dBtn(rTime, nGridX, nGridY, nGridZ)
	call WriteRt3dSsm
	call WriteMod2Bas(rTime, nGridX, nGridY, nGridZ)
end subroutine PreNumModel

!PostNumModel is called just after Modflow and Rt3d is called to load the calculation result for optimization
!the original code from meghna babbar is
! monwell()
! rtout()
! modflowout()
subroutine PostNumModel( nGridX, nGridY, nGridZ, arrH, arrHun, arrMonWellLocs, nMonWells, arrCon, nNumCon )
implicit none
	!argument
	integer, intent(in) :: nGridX, nGridY, nGridZ
	double precision, dimension(:,:,:), intent(inout) :: arrH, arrHun
	type(WellLoc), dimension(:), intent(inout) :: arrMonWellLocs
	integer, intent(in) :: nMonWells
	double precision, dimension(:,:), intent(inout) :: arrCon
	integer, intent(in) :: nNumCon

	!variables
	double precision :: rScaleX, rScaleY

	rScaleX = 3**(g_nGridFlagX-1)
	rScaleY = 3**(g_nGridFlagY-1)
	call ReadMonWell( rScaleX, rScaleY, arrMonWellLocs, nMonWells)
	call ReadRt3dCon(arrMonWellLocs, nMonWells, nGridX, nGridY, nGridZ, nNumCon, arrCon)
	call ReadModFlowHead( arrH, arrHun, nGridX, nGridY, nGridZ )
end subroutine PostNumModel

subroutine WriteMod2Well
implicit none
	integer :: i, j
	type(WellLoc), dimension(:), pointer :: parrLocs


	call OpenOutputFile( hMod2Well, strMod2Well )
	rewind( hMod2Well )
	!skip the unrelevant data
	do i=1,2
		read(hMod2Well, *)
	end do
	do i=1, g_nRemWells
		parrLocs => g_parrWellInfos(i)%parrLocs
		j = g_arrWellLocs(i)
!DEC$ IF(COST_VERSION>=2)
		write(hMod2Well, 1000) parrLocs(j)%z, parrLocs(j)%y, parrLocs(j)%x, g_arrPumpRates(i)
!DEC$ ELSE
		write(hMod2Well, 1000) parrLocs(j)%z, parrLocs(j)%y, parrLocs(j)%x, g_arrPumpRates(i)*365
!DEC$ ENDIF
	end do
	endfile( hMod2Well )
	close( hMod2Well )
1000 format(I2, I3, I3, F10.0)
end subroutine WriteMod2Well

subroutine WriteMod2Bcf(arrT, nGridX, nGridY, nGridZ)
implicit none
	double precision, dimension(:), intent(in) :: arrT
	integer, intent(in) :: nGridX, nGridY, nGridZ
	
	integer :: i, j, k, t
	integer :: nLines, nLastLen
	real :: rGridX

	rGridX = real(nGridX)	
	nLines = ceiling(rGridX/10.0)		!calculate the number of lines for each row of nGridX
	nLastLen = nGridX - 10*(nLines-1)	!the last line's data length
	
	call OpenOutputFile( hMod2Bcf, strMod2Bcf )
	rewind( hMod2Bcf )
	!skip the unrelevant data
	do i=1, 6
		read(hMod2Bcf, *)
	end do
		
	j=1
	do k=1, nGridY
		!write all the dataon nGridX, write in format max number of data is 10
		do i=1, nLines-1
			write(hMod2Bcf, 1000) (arrT(t), t=j, j+9)
			j = j+10
		end do
		!write the last line
		write(hMod2Bcf, 1001) (arrT(t), t=j, j+nLastLen-1)
		j = j+nLastLen
	end do
	endfile( hMod2Bcf )
	close( hMod2Bcf )
1000 format(10G15.6)
1001 format(10(G15.6))
end subroutine WriteMod2Bcf

subroutine WriteRt3dBtn(rTime, nGridX, nGridY, nGridZ)
implicit none
	double precision, intent(in) :: rTime
	integer, intent(in) :: nGridX, nGridY, nGridZ
	
	integer :: i
	integer :: nLines, nSteps, nMaxSteps
	real :: rGridX
	double precision :: rPeriod, rInitPeriod

	rGridX = real(nGridX)	
	nLines = ceiling(rGridX/10.0)		!calculate the number of lines for each row of nGridX
	
	call OpenOutputFile( hRt3dBtn, strRt3dBtn )
	rewind( hRt3dBtn )
	!skip the unrelevant data
	do i=1, (18+(nLines*nGridY))
		read(hRt3dBtn, *)
	end do
	
!DEC$ IF(COST_VERSION>=2)
	nSteps = 1
	nMaxSteps = 1000
	rPeriod = 1.0
	rInitPeriod = 0.0
	write(hRt3dBtn, 1000) rTime, nSteps, rPeriod
	write(hRt3dBtn, 1001) rInitPeriod, nMaxSteps
!DEC$ ELSE
	nSteps = 1
	nMaxSteps = 1000000
	rPeriod = 1.0
	rInitPeriod = 0.05
	write(hRt3dBtn, 1000) rTime, nSteps, rPeriod
	write(hRt3dBtn, 1001) 0.05, nMaxSteps		!the first is transportsz
!DEC$ ENDIF

	close( hRt3dBtn )
1000 FORMAT(F10.0,I10,G15.7)
1001 FORMAT(F10.2,I10)
end subroutine WriteRt3dBtn

subroutine WriteMod2Bas(rTime, nGridX, nGridY, nGridZ)
implicit none
	double precision, intent(in) :: rTime
	integer, intent(in) :: nGridX, nGridY, nGridZ
	
	integer :: i
	integer :: nLines, nBlocks, nVal
	real :: rGridX
	double precision :: rVal

	rGridX = real(nGridX)	
	nLines = ceiling(rGridX/10.0)		!calculate the number of lines for each row of nGridX
	nBlocks = ceiling(rGridX/80.0)		!calculate the number of blocks for each row
	
	call OpenOutputFile( hMod2Bas, strMod2Bas )
	rewind( hMod2Bas )
	!skip the unrelevant data
	do i=1, (8+ nBlocks*nGridY + nLines*nGridY)
		read(hMod2Bas, *)
	end do
	nVal = 1
	rVal = 1.0
	write(hMod2Bas, 1000) rTime, nVal, rVal
	close( hMod2Bas )
1000 FORMAT(F10.0,I10,F10.0)
end subroutine WriteMod2Bas

!write the rt3df2.ssm	
subroutine WriteRt3dSsm
implicit none
	integer :: i, j
	double precision :: css1, css2
	integer :: itype
	type(WellLoc), dimension(:), pointer :: parrLocs
	
	call OpenOutputFile( hRt3dSsm, strRt3dSsm )
	rewind( hRt3dSsm )
	do i=1, 3
		read(hRt3dSsm, *)
	end do
	css1 = 0.0
	css2 = 0.0
	itype = -1
	
	do i=1, g_nRemWells
		parrLocs => g_parrWellInfos(i)%parrLocs
		j = g_arrWellLocs(i)
		write(hRt3dSsm, 1000) parrLocs(j)%z, parrLocs(j)%x, parrLocs(j)%y, css1,itype,css2
	end do
	endfile( hRt3dSsm )
	close( hRt3dSsm )
1000	FORMAT(3I10,F10.0,I10,F10.0)
end subroutine WriteRt3dSsm

!************************************************** the follow code is from meghna babbar********************
!the original from meghna babbar is monwell()
subroutine ReadMonWell( rScaleX, rScaleY, arrMonWellLocs, nMonWells)
IMPLICIT NONE
	!arguments
	double precision, intent(in) :: rScaleX, rScaleY
	type(WellLoc), dimension(:), intent(out) :: arrMonWellLocs
	integer, intent(in) :: nMonWells

	!variables
	integer :: id, x, y, z
	integer :: i
 
	call OpenInputFile(hMonLoc, strMonLoc) 
	rewind hMonLoc
	do i=1,nMonWells
		read(hMonLoc, *) id, x, y, z
		arrMonWellLocs(i)%nId = id
		arrMonWellLocs(i)%x = rScaleX*(x-1) + ceiling(rScaleX/2)
		arrMonWellLocs(i)%y = rScaleY*(y-1) + ceiling(rScaleY/2)
		arrMonWellLocs(i)%z = z
	end do
	close(hMonLoc)
end subroutine ReadMonWell

!the original code from meghna babbar is rtout()
subroutine ReadRt3dCon(arrMonWells, nMonWells, nGridX, nGridY, nGridZ, nNumCon, arrCon)
IMPLICIT NONE
	!argument
	type(WellLoc), dimension(:), intent(in) :: arrMonWells
	integer, intent(in) :: nMonWells
	integer, intent(in) :: nGridX, nGridY, nGridZ
	integer, intent(in) :: nNumCon
	double precision, dimension(:,:) :: arrCon

	!variables
	double precision, dimension(:,:,:), allocatable :: arrConOut
	integer :: i,j,k

	allocate( arrConOut(nGridX, nGridY, nGridZ) )
 
	call OpenInputFile(hConOut,strConOut)
	do i = 1, nGridZ
		do j = 1, nGridY
			do k = 1, nGridX
				read(hConOut, *)arrConOut(k, j, i)
			end do
		end do
	end do
	close(hConOut)

	! This is the end of the section where the conc are read in from the rt3dout file
	! The next section is the one where the monitoring well concentrations are picked out

	do i = 1, nNumCon
		do j = 1, nMonWells
			arrCon(i, j) = arrConOut( arrMonWells(j)%x, arrMonWells(j)%y, arrMonWells(j)%z )
		end do
	end do

	deallocate( arrConOut )
end subroutine ReadRt3dCon

!the original code from meghna babbar is modflowout()
subroutine ReadModFlowHead(H,Hun,xgrid,ygrid,zgrid)
IMPLICIT NONE
	!arguments
	integer, intent(in) :: xgrid,ygrid,zgrid 
	double precision, dimension(zgrid,xgrid,ygrid),intent(out) :: Hun, H
	!parameters
	integer :: i,j,k
 
 
	!note the output from modelfow is H(xgrid, ygrid, zgrid)
	!H(k,j,i)(i=1,zgrid, (j=1,ygrid, (k=1,xgrid)))
	call OpenInputFile( hHeadOld, strHeadOld )
	do i=1,zgrid
		do j=1,ygrid
			do k=1,xgrid
				read(hHeadOld, *)Hun(i,k,j)
			end do
		end do
	end do
	CLOSE(hHeadOld)

	!note the output from modelfow is H(xgrid, ygrid, zgrid)
	!H(k,j,i)(i=1,zgrid, (j=1,ygrid, (k=1,xgrid)))
	call OpenInputFile( hHeadNew, strHeadNew )
	do i=1,zgrid
		do j=1,ygrid
			do k=1,xgrid
				read(hHeadNew, *)H(i,k,j)
			end do
		end do
	end do
	CLOSE(hHeadNew)
end subroutine ReadModFlowHead

end module pdeio
