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

!	arrIds = (/ (i, i=1,count) /)

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
	integer :: arrPdeIds(sgaParam%nPopSize)

	arrPdeIds = (/ (i, i=1, sgaParam%nPopSize) /)
	call ChooseRealizations2( sgaParam, arrPdeIds, sgaParam%nPopSize, arrRealizations )
	return


	do i = 1, sgaParam%nPopSize
!		call LatintubeSample2( sgaParam%arrMoments, sgaParam%arrMomentIds, arrRealizations(i,:) )
		call FixedSampling( sgaParam%arrMoments, sgaParam%arrMomentIds, arrRealizations(i,:) )
		!call SampleRealizations( 1, 100, arrRealizations(i,:) )
	end do
end subroutine

subroutine ChooseRealizations2( sgaParam, arrPdeIds, nPdeIds, arrRealizations )
use sga
use chmcache
implicit none                                                                                                                                                                                             
	!argument
	type(SGAPARAMSTRUCT), intent(inout) :: sgaParam
	integer, dimension(:), intent(in) :: arrPdeIds
	integer, dimension(:,:), intent(inout) :: arrRealizations
	integer :: nPdeIds

	!variables
	integer :: i, k, id, count, nThisSamples, nSampledIds
	integer, dimension(100) :: arrSampledIds

	do i=1, nPdeIds
		id = arrPdeIds(i)
		nSampledIds = GetSampleCountCache( hChromCache, sgaParam%arrPop(id,:), sgaParam%nChromBits )
		if( nSampledIds > 0 )then
			call GetSampleIdsCache( hChromCache, sgaParam%arrPop(id,:), sgaParam%nChromBits, arrSampledIds )
		endif

		count = ubound(sgaParam%arrMoments, 1) - lbound(sgaParam%arrMoments, 1) + 1
		nThisSamples = sgaParam%nSamples

		call NonReplaceLatintubeSample( sgaParam%arrMomentIds, count, arrSampledIds, nSampledIds, arrRealizations(id,:), nThisSamples )
	enddo
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

!	arrRealizations = (/10, 67, 46, 30, 20, 75/)
!	arrRealizations = (/51, 75, 67, 46, 20, 73/)
!	arrRealizations = (/31, 26, 46, 75, 39, 50/)
	!for debugging
	arrRealizations = (/(i, i=1,samples)/)
	return

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

!the Latintube sampling is WRONG!!!!!!!!!!!!!!!!!!!!!!!!!!

!arrIds has the Ids in nature order, arrMoments are sorted.
subroutine LatintubeSample2( arrMoments, arrIds, arrRealizations )
implicit none
	!arguments
	double precision, dimension(:), intent(in) :: arrMoments
	integer, dimension(:), intent(in) :: arrIds
	integer, dimension(:), intent(out) :: arrRealizations

	!variables
!	double precision :: rMomentStrid, rMomentStart, rMomentEnd
	integer :: i, samples, count, index, ids, ide
	double precision :: step

	count = ubound(arrMoments, 1) - lbound(arrMoments, 1) + 1
	samples = ubound(arrRealizations, 1) - lbound(arrRealizations, 1) + 1

	step = dble(count) / samples
	ids = 1
	do i = 1, samples
		ide = int(1 + i*step)

		!this tube range is [ids, ide-1], or [ids, ide)
		index = floor( UniRand()* (ide-ids) ) + ids
		if( index>ide )index = ide
		if( index>count )index = count

		!sample it
		arrRealizations(i) = arrIds(index)

		ids = ide
	enddo

	!compute the moment interval.
!	rMomentStrid = ( arrMoments(count) - arrMoments(1) ) / samples
!	rMomentStrid = ( arrMoments(arrIds(count)) - arrMoments(arrIds(1)) ) / samples

!	ids = 1
!	do i=1, samples
!		rMomentStart = arrMoments(ids)
!		rMomentStart = arrMoments( arrIds(ids) )
!		rMomentEnd = rMomentStart + rMomentStrid
!		ide = ids
		!find the position where moment is just bigger than rMomentEnd
!		do while( ide<=count .and. arrMoments(ide)<rMomentEnd )
!		do while( ide<=count .and. arrMoments( arrIds(ide) )<rMomentEnd )
!			ide = ide + 1
!		end do
!		if( ide>count )ide = count

		!this tube range is [ids, ide-1], or [ids, ide)
!		index = floor( UniRand()* (ide-ids) ) + ids
!		if( index>ide )index = ide

		!sample it
!		arrRealizations(i) = arrIds(index)

!		ids = ide
!	enddo
end subroutine

!random sample an integer from a range [begin_id, end_id) or [begin_id, end_id-1]
integer function RandomSample( begin_id, end_id )
implicit none
	!arguments
	integer, intent(in) :: begin_id, end_id

	RandomSample = floor( UniRand()* (end_id-begin_id) ) + begin_id
end function

!compute the begin and end range of index with size step, count is the maximum id [begin, end)
function GetRange( index, step, count )
implicit none
	!argument
	integer :: GetRange(2)
	integer, intent(in) :: index, count
	double precision, intent(in) :: step

	GetRange = (/ 1+(index-1)*step, 1+index*step /)
	if( GetRange(2)>count )then
		GetRange(2)=count+1
	endif
end function

!arrIds has the Ids in nature order, arrMoments are sorted.
subroutine NonReplaceLatintubeSample( arrIds, count, arrSampledIds, nSampledIds, arrSamplingIds, nThisSamples )
implicit none
	!arguments
	integer, dimension(:), intent(in) :: arrIds, arrSampledIds
	integer, dimension(:), intent(out) :: arrSamplingIds
	integer :: count, nSampledIds, nThisSamples

	!variables
	integer :: i, id_bin, id, nTotalSamples, nBins
	integer :: rng(2)
	double precision :: step
	logical, dimension(:), allocatable :: arrIdFlags
	integer, dimension(:), allocatable :: arrBins


	nTotalSamples = nThisSamples + nSampledIds
	step = dble(count) / nTotalSamples

	allocate( arrIdFlags(count), arrBins(nTotalSamples) )

	!set the flags of arrIdFlags .true. sampled; .false. not sampled yet.
	call FillIdFlags()
	nBins = 0
	do i=1, nTotalSamples
		rng = GetRange( i, step, count )
		if( .not. CheckBin( rng ) )then
			nBins = nBins + 1
			arrBins(nBins) = i
		endif
	enddo

	call assert( nBins>=nThisSamples )

	do i=1, nThisSamples
		id_bin = RandomSample( 1, nBins + 1 )	!id the index of the sampling bin
		!for debug
		!id_bin = 1
		rng = GetRange( arrBins(id_bin), step, count )
		id = RandomSample( rng(1), rng(2) )
		do while( arrIdFlags(id) )
			id = RandomSample( rng(1), rng(2) )
		enddo

		arrSamplingIds(i) = arrIds(id)

		!sample without replacement
		call SwapInt( arrBins(id_bin), arrBins(nBins) )
		nBins = nBins -1
	enddo

	deallocate( arrIdFlags, arrBins )

contains

	!check if any of the ids in the bin defined by [ rgn(1), rgn(2) ) is already sampled in arrSampledIds
	!return true if the some id in the bin is sampled (bin is already sampled)
	logical function CheckBin( rng )
	implicit none
		integer :: rng(2)

		integer :: i
		do i=rng(1), rng(2)-1
			if( arrIdFlags(i) )then	!this id is already sampled.
				CheckBin = .true.
				return
			endif
		enddo
		CheckBin = .false.
	end function

	!fill the arrIdFlags array. .false. - not sampled yet. .true.- already sampled
	subroutine FillIdFlags()
	implicit none
		
		integer :: i, j
		arrIdFlags = .false.		!non one is sampled

		do i=1, count
			do j=1, nSampledIds
				if( arrIds(i)==arrSampledIds(j) )then
					arrIdFlags(i) = .true.
					exit
				endif
			enddo
		enddo
	end subroutine

end subroutine

!arrIds has the Ids in nature order, arrMoments are sorted.
subroutine FixedSampling( arrMoments, arrIds, arrRealizations )
implicit none
	!arguments
	double precision, dimension(:), intent(in) :: arrMoments
	integer, dimension(:), intent(in) :: arrIds
	integer, dimension(:), intent(out) :: arrRealizations

	!variables
!	double precision :: rMomentStrid, rMomentStart, rMomentEnd
	integer :: i, samples, count, index, ids, ide
	double precision :: step

	count = ubound(arrMoments, 1) - lbound(arrMoments, 1) + 1
	samples = ubound(arrRealizations, 1) - lbound(arrRealizations, 1) + 1

	arrRealizations = (/31, 26, 46, 75, 39, 50/)

end subroutine

end module
