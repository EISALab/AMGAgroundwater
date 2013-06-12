module chmcache
implicit none                                                                                           

interface                                                                                               
!******************subroutine interface for chromosome cache*********************
	integer function CreateCustomChromCache
	end function CreateCustomChromCache

	integer function CreateChromCache
	end function CreateChromCache

	subroutine InsertChromCache( hCache, arrChrom, nChromLen, arrFits, nFits )
		integer :: hCache
		integer, dimension(*) :: arrChrom
		double precision, dimension(*):: arrFits
		integer, intent(in) :: nChromLen, nFits
	end subroutine InsertChromCache

	subroutine ReplaceChromCache( hCache, arrChrom, nChromLen, arrFits, nFits )
		integer :: hCache
		integer, dimension(*) :: arrChrom
		double precision, dimension(*):: arrFits
		integer, intent(in) :: nChromLen, nFits
	end subroutine ReplaceChromCache

	integer function SearchChromCache( hCache, arrChrom, nChromLen, arrFits, nFits )
		integer :: hCache
		integer, dimension(*) :: arrChrom
		double precision, dimension(*), intent(out) :: arrFits
		integer, intent(in) :: nChromLen, nFits
	end function SearchChromCache
                                                                                                        
	subroutine ReleaseChromCache( hCache )
		integer :: hCache
	end subroutine ReleaseChromCache
end interface

interface
	integer function find( array, count, elem )
	implicit none
		integer, dimension(*), intent(in) :: array
		integer, intent(in) :: count, elem
	end function
	subroutine abs_sort( array, count )
	implicit none
		double precision, dimension(*), intent(in,out) :: array
		integer, intent(in) :: count
	end subroutine
	double precision function norm_cdf( x )
	implicit none
		double precision, intent(in) :: x
	end function
end interface

end module chmcache