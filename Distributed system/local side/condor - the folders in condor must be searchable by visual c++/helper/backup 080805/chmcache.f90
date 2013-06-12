module chmcache
implicit none                                                                                           

interface                                                                                               
!******************subroutine interface for chromosome cache*********************
	integer function CreateChromCache
	end function
	integer function CreateChromCacheEx
	end function


	subroutine InsertChromCache( hCache, arrChrom, nChromLen, arrFits, nFits )
		integer :: hCache
		integer, dimension(*) :: arrChrom
		double precision, dimension(*):: arrFits
		integer, intent(in) :: nChromLen, nFits
	end subroutine
	subroutine InsertChromCacheEx( hCache, arrChrom, nChromLen, arrFits, nFits )
		integer :: hCache
		integer, dimension(*) :: arrChrom
		double precision, dimension(*):: arrFits
		integer, intent(in) :: nChromLen, nFits
	end subroutine

	subroutine ReplaceChromCache( hCache, arrChrom, nChromLen, arrFits, nFits )
		integer :: hCache
		integer, dimension(*) :: arrChrom
		double precision, dimension(*):: arrFits
		integer, intent(in) :: nChromLen, nFits
	end subroutine
	subroutine ReplaceChromCacheEx( hCache, arrChrom, nChromLen, arrFits, nFits )
		integer :: hCache
		integer, dimension(*) :: arrChrom
		double precision, dimension(*):: arrFits
		integer, intent(in) :: nChromLen, nFits
	end subroutine

	integer function SearchChromCache( hCache, arrChrom, nChromLen, arrFits, nFits )
		integer :: hCache
		integer, dimension(*) :: arrChrom
		double precision, dimension(*), intent(out) :: arrFits
		integer, intent(in) :: nChromLen, nFits
	end function
	integer function SearchChromCacheEx( hCache, arrChrom, nChromLen, arrFits, nFits )
		integer :: hCache
		integer, dimension(*) :: arrChrom
		double precision, dimension(*), intent(out) :: arrFits
		integer, intent(in) :: nChromLen, nFits
	end function
                                                                                                        
	subroutine ReleaseChromCache( hCache )
		integer :: hCache
	end subroutine
	subroutine ReleaseChromCacheEx( hCache )
		integer :: hCache
	end subroutine
end interface

end module chmcache