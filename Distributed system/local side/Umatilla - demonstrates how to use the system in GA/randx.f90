module randx
implicit none
!These are the variables used to control the random behavior. They are saved and restored during restart

	double precision :: mbig=8800000.d0,mseed=1618033.d0,mz=0.d0,fac=1./8800000.d0
	double precision :: mj,mk,ma

	integer :: iff,inext,inextp
	dimension ma(55)
	data iff /0/

contains

!ran1 and ran2 are compitable to the old version prototype.
subroutine ran1( idum, rand_x )
implicit none
	!argument
	integer, intent(in) :: idum
	double precision, intent(out) :: rand_x

	if( idum < 0 )then
		rand_x = UniRand(idum)
	else
		rand_x = UniRand()
	endif
end subroutine ran1

subroutine ran2( idum, rand_x )
implicit none
	!argument
	integer, intent(in) :: idum
	double precision, intent(out) :: rand_x

	call rand1( idum, rand_x )
end subroutine ran2

subroutine SaveRand( hFile )
implicit none
	integer, intent(in) :: hFile
	
	write( hFile, * ) mbig, mseed, mz, fac, mj, mk
	write( hFile, * ) iff, inext, inextp
	write( hFile, * ) ma
end subroutine SaveRand

subroutine LoadRand( hFile )
implicit none
	integer, intent(in) :: hFile
	
	read( hFile, * ) mbig, mseed, mz, fac, mj, mk
	read( hFile, * ) iff, inext, inextp
	read( hFile, * ) ma
end subroutine LoadRand

!##################################################################################
!Returns a uniform random deviate between 0.0 and 1.0.  Set idum to 
!any negative value to initialize or reinitialize the sequence.
!This function is taken from W.H. Press', "Numerical Recipes" p. 199.!
double precision function UniRand( idum )
implicit none
save
	!argument
	optional :: idum
	integer :: idum
	
	!variable
	integer :: i, ii, k

	if( present(idum) )then
		if(idum.lt.0 .or. iff.eq.0) then
			iff=1
			mj=mseed-dble(iabs(idum))
			mj=dmod(mj,mbig)
			ma(55)=mj
			mk=1
			do i=1,54
				ii=mod(21*i,55)
				ma(ii)=mk
				mk=mj-mk
				if(mk.lt.mz) mk=mk+mbig
				mj=ma(ii)
			enddo
			do k=1,4
				do i=1,55
					ma(i)=ma(i)-ma(1+mod(i+30,55))
					if(ma(i).lt.mz) ma(i)=ma(i)+mbig
				enddo
			enddo
			inext=0
			inextp=31
			idum=1
		endif
	endif

	inext=inext+1
	if(inext.eq.56) inext=1
	inextp=inextp+1
	if(inextp.eq.56) inextp=1
	mj=ma(inext)-ma(inextp)
	if(mj.lt.mz) mj=mj+mbig
	ma(inext)=mj
	UniRand=mj*fac

end function UniRand

end module randx