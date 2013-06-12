module sampling
use std_helper
use sga
implicit none

integer, parameter :: hFile = 1050

contains

!subroutine GetBaseCond( arrTstd, gridx, gridy, gridz )
!implicit none
	!argument
!	integer :: gridx, gridy, gridz
!	double precision, dimension(gridx*gridy*gridz), intent(out) :: arrTstd

	!variables
!	integer :: i, size

!	arrTstd = 0
!	size = gridx*gridy*gridz
!	do i=1, g_nFields
!		arrTstd = arrTstd + g_rT( (i-1)*size+1 : i*size )
!	enddo
!	arrTstd = arrTstd / g_nFields
!end subroutine

subroutine CreateMoments( arrIds, moments, count )
implicit none
	integer, intent(in) :: count
	integer, intent(out) :: arrIds(count)
	double precision, intent(out) :: moments(count)

! The CreateMoments function is modified for the Umatilla case since the moments has already been created by Abhishek.
	integer :: i

	call OpenInputFile( hFile, 'moments.txt' )
	read( hFile, * )(moments(i), i=1,count)
	close( hFile )

	call OpenInputFile( hFile, 'moments_numbers.txt' )
	read( hFile, *) (arrIds(i), i=1,count)
	close( hFile )

!	integer :: i, size
!	double precision, dimension(:), allocatable :: arrCond, arrCondStd

!	size = g_nCoarsGridX * g_nCoarsGridY * g_nCoarsGridZ
!	allocate( arrCond(size) )
!	allocate( arrCondStd(size) )

!	call GetBaseCond( arrCondStd, g_nCoarsGridX, g_nCoarsGridY, g_nCoarsGridZ )
!	do i=1, count
!		arrCond = g_rT( (i-1)*size+1 : i*size )
!		moments(i) = CalcMoment2( arrCond, arrCondStd, g_nCoarsGridY, g_nCoarsGridX, g_nCoarsGridZ )
!	enddo

!	arrIds = (/ (i, i=1,count) /)
!	call qsort( arrIds, count, sizeof(arrIds(1)), MomentCompare )

!	deallocate( arrCond, arrCondStd )

contains
	integer(2) function MomentCompare( i1, i2 )
	implicit none
		!argument
		integer, intent(in) :: i1, i2

		!varaibles
		if( moments(i1) < moments(i2) )then
			MomentCompare = -1
		else if( moments(i1)>moments(i2) ) then
			MomentCompare = 1
		else 
			MomentCompare = 0
		endif
	end function

end subroutine

double precision function CalcMoment2( conds, conds_std, nrow, ncol, nlay )
implicit none
	integer :: nrow, ncol, nlay
!	double precision, dimension(nrow*ncol*nlay) :: conds, conds_std
	!the cond is stored by columns first, then rows and layers
	double precision, dimension(ncol, nrow, nlay) :: conds, conds_std
	!variables
	double precision :: moment, i, j, k

	moment = 0
	do k= 1, nlay
		do i=1, nrow
			do j=1, ncol
!				moment = moment + (conds_std(i,j,k)-conds(i,j,k)) * (conds_std(i,j,k)-conds(i,j,k))
!				moment = moment + conds(i,j,k) * conds(i,j,k)
!				moment = moment + sqrt(dble((i-1)*(i-1)+(j-1)*(j-1))) * abs(conds_std(i,j,k)-conds(i,j,k))
				moment = moment + sqrt(dble(i)*(i)+(j)*(j)) * abs(conds_std(j,i,k)-conds(j,i,k))
!				moment = moment + sqrt(dble(i)*(i)+(j)*(j)) * abs(conds_std((k-1)*(nrow*ncol) + (i-1)*ncol + j)-conds((k-1)*(nrow*ncol) + (i-1)*ncol + j))
			enddo
		enddo
	enddo
	CalcMoment2 = moment / nrow / ncol / nlay
end function


double precision function calc_moment( conds, conds_std, nrow, ncol, nlay )
implicit none
	integer :: nrow, ncol, nlay
	real, dimension(nrow, ncol, nlay) :: conds, conds_std
	!variables
	double precision :: moment, i, j, k

	moment = 0
	do k= 1, nlay
		do i=1, nrow
			do j=1, ncol
!				moment = moment + (conds_std(i,j,k)-conds(i,j,k)) * (conds_std(i,j,k)-conds(i,j,k))
!				moment = moment + conds(i,j,k) * conds(i,j,k)
!				moment = moment + sqrt(dble((i-1)*(i-1)+(j-1)*(j-1))) * abs(conds_std(i,j,k)-conds(i,j,k))
				moment = moment + sqrt(dble(i)*(i)+(j)*(j)) * abs(conds_std(i,j,k)-conds(i,j,k))
			enddo
		enddo
	enddo
	calc_moment = moment / nrow / ncol / nlay
end function

subroutine ChooseRealizations( sgaParam, arrRealizations )
use sga
implicit none                                                                                                                                                                                             
	!argument
	type(SGAPARAMSTRUCT), intent(inout) :: sgaParam
	integer, dimension(:,:), intent(inout) :: arrRealizations

	!variables
	integer :: i, k
	do i = 1, sgaParam%nPopSize
		call LatintubeSample2( sgaParam%arrMoments, sgaParam%arrMomentIds, arrRealizations(i,:) )
		!call SampleRealizations( 1, 100, arrRealizations(i,:) )
	end do
end subroutine

subroutine SampleRealizations( id1, id2, arrRealizations )
implicit none
	!arguments
	integer, intent(in) :: id1, id2
	integer, dimension(:), intent(inout) :: arrRealizations

	!variables
	integer :: i, samples, count, index
	integer, dimension(id2-id1+1) :: ids

	ids = (/(i, i=id1,id2)/)
	
	count = id2-id1+1
	samples = ubound(arrRealizations, 1) - lbound(arrRealizations, 1) + 1

	!for debugging
!	arrRealizations = (/(1, i=1,samples)/)
!	return

	do i=1, samples
		!randomly sample an id 
		index = ceiling( UniRand()*count )
		if (index==0 ) index = 1

		!sample it
		arrRealizations(i) = ids(index)

		!sample without replacement
		call SwapInt( ids(index), ids(count) )
		count = count-1
	enddo
end subroutine

!arrIds has the sorted Ids, arrMoments are not sorted, in natural order.
subroutine LatintubeSample( arrMoments, arrIds, arrRealizations )
implicit none
	!arguments
	double precision, dimension(:), intent(in) :: arrMoments
	integer, dimension(:), intent(in) :: arrIds
	integer, dimension(:), intent(out) :: arrRealizations

	!variables
	double precision :: rMomentStrid, rMomentStart, rMomentEnd
	integer :: i, samples, count, index, ids, ide

	count = ubound(arrMoments, 1) - lbound(arrMoments, 1) + 1
	samples = ubound(arrRealizations, 1) - lbound(arrRealizations, 1) + 1

	!compute the moment interval.
!	rMomentStrid = ( arrMoments(count) - arrMoments(1) ) / samples
	rMomentStrid = ( arrMoments(arrIds(count)) - arrMoments(arrIds(1)) ) / samples

	ids = 1
	do i=1, samples
!		rMomentStart = arrMoments(ids)
		rMomentStart = arrMoments( arrIds(ids) )
		rMomentEnd = rMomentStart + rMomentStrid
		ide = ids
		!find the position where moment is just bigger than rMomentEnd
!		do while( ide<=count .and. arrMoments(ide)<rMomentEnd )
		do while( ide<=count .and. arrMoments( arrIds(ide) )<rMomentEnd )
			ide = ide + 1
		end do
		if( ide>count )ide = count

		!this tube range is [ids, ide-1], or [ids, ide)
		index = floor( UniRand()* (ide-ids) ) + ids
		if( index>ide )index = ide

		!sample it
		arrRealizations(i) = arrIds(index)

		ids = ide
	enddo
end subroutine

!arrIds has the Ids in nature order, arrMoments are sorted.
subroutine LatintubeSample2( arrMoments, arrIds, arrRealizations )
implicit none
	!arguments
	double precision, dimension(:), intent(in) :: arrMoments
	integer, dimension(:), intent(in) :: arrIds
	integer, dimension(:), intent(out) :: arrRealizations

	!variables
	double precision :: rMomentStrid, rMomentStart, rMomentEnd
	integer :: i, samples, count, index, ids, ide

	count = ubound(arrMoments, 1) - lbound(arrMoments, 1) + 1
	samples = ubound(arrRealizations, 1) - lbound(arrRealizations, 1) + 1

	!compute the moment interval.
	rMomentStrid = ( arrMoments(count) - arrMoments(1) ) / samples
!	rMomentStrid = ( arrMoments(arrIds(count)) - arrMoments(arrIds(1)) ) / samples

	ids = 1
	do i=1, samples
		rMomentStart = arrMoments(ids)
!		rMomentStart = arrMoments( arrIds(ids) )
		rMomentEnd = rMomentStart + rMomentStrid
		ide = ids
		!find the position where moment is just bigger than rMomentEnd
		do while( ide<=count .and. arrMoments(ide)<rMomentEnd )
!		do while( ide<=count .and. arrMoments( arrIds(ide) )<rMomentEnd )
			ide = ide + 1
		end do
		if( ide>count )ide = count

		!this tube range is [ids, ide-1], or [ids, ide)
		index = floor( UniRand()* (ide-ids) ) + ids
		if( index>ide )index = ide

		!sample it
		arrRealizations(i) = arrIds(index)

		ids = ide
	enddo
end subroutine
	
end module
