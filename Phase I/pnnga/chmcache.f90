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

end module chmcache