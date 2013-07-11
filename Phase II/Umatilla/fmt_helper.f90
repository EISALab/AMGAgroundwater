module fmt_helper

implicit none

contains

!this subroutine load all the hydraulic head data from the datafile
!then lookup the array to find the remediation head
!the result is stored back to rWellHeads
!nWellIds: is the well location number array
!possibnodes: is the maximum well location canditates array
!rwlcoars: should be storing the well location number, x, y and z coordinate
subroutine LoadWellHeads( strHeadFile, arrWellIds, arrWellInfos, arrHeads, nRemWells, xcoars, ycoars, zcoars )
use umacase
implicit none
	!argument
	integer, intent(in) :: nRemWells
	integer, intent(in):: xcoars, ycoars, zcoars
	character(*), intent(in) :: strHeadFile
	type(RemWellInfo), intent(in) :: arrWellInfos(nRemWells)
	integer, intent(in) :: arrWellIds(nRemWells)
	double precision, intent(out) :: arrHeads(nRemWells)

	!varables
	!character(*) :: strHeadFile
	!parameter( strHeadFile='headnew.out' )

	double precision, allocatable, dimension(:) :: pHeads
	integer :: nHeads, nowells, nNodeId
	integer :: memerr
	integer :: i

	nHeads = xcoars * ycoars * zcoars
	allocate( pHeads(nHeads), stat=memerr )
	if( memerr/=0 )then
		print *, "memory allocate error!"
		stop
	end if

	call readhead2( pHeads, nHeads, strHeadFile )

	do i=1, nRemWells
		nNodeId = arrWellIds(i)
		arrHeads(i) = lookuphead(pHeads, arrWellInfos(i)%parrLocs(nNodeId)%x, arrWellInfos(i)%parrLocs(nNodeId)%y, arrWellInfos(i)%parrLocs(nNodeId)%z, xcoars, ycoars, zcoars )
	end do

	deallocate( pHeads )
end subroutine LoadWellHeads

!lookupcoarsid will look up the table to find the correct index correspoinding the well location number
!the subroutine first assume the location number equals the index number if it's not ok
!the subroutine then look through all the canditate locations to find the correct one
integer function lookupcoarsid( nodes, rwlcoars, nWellId, nLocId )
	implicit none

	integer, intent(in) :: nodes
	integer, dimension(:,:,:), intent(in) :: rwlcoars
	integer, intent(in) :: nWellId, nLocId

	integer :: i

	if( rwlcoars(nWellId, nLocId, 1)==nLocId )then
		lookupcoarsid = nLocId
		return
	else
		do i=1, nodes
			if( rwlcoars(nWellId, i, 1)==nLocId )then
				lookupcoarsid = nLocId
				return
			end if
		end do
	end if
	!look up failed
	lookupcoarsid = -1
end function lookupcoarsid

!pHead: The allocatable array, the subroutine will allocate memory and then read
!all the head data into this array.
!nHeads: Number of head data
!strFile: File name contains hydraulic head data
subroutine readhead2( pHeads, nHeads, strFile )
	implicit none
	double precision, dimension(:), intent(out) :: pHeads
	integer, intent(in) :: nHeads
	character(*), intent(in) :: strFile

	integer :: hHeadFile
	parameter( hHeadFile=125 )

	integer :: ioerr
	integer :: i


	open( unit=hHeadFile, file=strFile, status='old', iostat=ioerr )
	if( ioerr/=0 )then
		print *, strFile, ' read error!'
		stop
	endif
	rewind hHeadFile

	do i=LBOUND(pHeads, 1), UBOUND(pHeads,1)
		read( hHeadFile, * ) pHeads(i)
	end do

	close( hHeadFile )

end subroutine readhead2


double precision function lookuphead( pHeads, xId, yId, zId, xCount, yCount, zCount )
	implicit none
	double precision, dimension(:), intent(in) :: pHeads
	integer, intent(in) :: xId, yId, zId
	integer, intent(in) :: xCount, yCount, zCount

	integer :: i

!the allign of the 3-D matrix is assumed by z, y, x
	i = (zCount-1)*(xCount*yCount) + (yId-1)*(xCount) + xId

!for debug only
!	print*, xId, yId, zId, i
	
	lookuphead = pHeads(i)

end function lookuphead


end module fmt_helper
