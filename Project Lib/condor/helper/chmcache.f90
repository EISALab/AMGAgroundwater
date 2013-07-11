module chmcache
implicit none                                                                                           

interface                                                                                               
!******************subroutine interface for chromosome cache*********************
	subroutine SetChromNormFunc( ChromNormFunc )
	external ChromNormFunc
	end subroutine

	integer function CreateChromCache
	end function
	integer function CreateChromCacheEx
	end function


	subroutine InsertChromCache( hCache, arrChrom, nChromLen, nSampleId, arrFits, nFits )
		integer :: hCache
		integer, dimension(*) :: arrChrom
		double precision, dimension(*):: arrFits
		integer, intent(in) :: nChromLen, nSampleId, nFits
	end subroutine
	subroutine InsertChromCacheEx( hCache, arrChrom, nChromLen, nSampleId, arrFits, nFits )
		integer :: hCache
		integer, dimension(*) :: arrChrom
		double precision, dimension(*):: arrFits
		integer, intent(in) :: nChromLen, nSampleId, nFits
	end subroutine

	subroutine ReplaceChromCache( hCache, arrChrom, nChromLen, nSampleId, arrFits, nFits )
		integer :: hCache
		integer, dimension(*) :: arrChrom
		double precision, dimension(*):: arrFits
		integer, intent(in) :: nChromLen, nSampleId, nFits
	end subroutine
	subroutine ReplaceChromCacheEx( hCache, arrChrom, nChromLen, nSampleId, arrFits, nFits )
		integer :: hCache
		integer, dimension(*) :: arrChrom
		double precision, dimension(*):: arrFits
		integer, intent(in) :: nChromLen, nSampleId, nFits
	end subroutine

	subroutine RemoveChromCache( hCache, arrChrom, nChromLen, nSampleId )
		integer :: hCache
		integer, dimension(*) :: arrChrom
		integer, intent(in) :: nChromLen, nSampleId
	end subroutine

	integer function SearchChromCache( hCache, arrChrom, nChromLen, nSampleId, arrFits, nFits )
		integer :: hCache
		integer, dimension(*) :: arrChrom
		double precision, dimension(*), intent(out) :: arrFits
		integer, intent(in) :: nChromLen, nSampleId, nFits
	end function
	integer function SearchChromCacheEx( hCache, arrChrom, nChromLen, nSampleId, arrFits, nFits )
		integer :: hCache
		integer, dimension(*) :: arrChrom
		double precision, dimension(*), intent(out) :: arrFits
		integer, intent(in) :: nChromLen, nSampleId, nFits
	end function

	integer function GetStatChromCache( hCache, arrChrom, nChromLen, arrMeanFits, arrStdFits )
		integer :: hCache
		integer, dimension(*) :: arrChrom
		double precision, dimension(*), intent(out) :: arrMeanFits, arrStdFits
		integer, intent(in) :: nChromLen
	end function
	integer function GetStatChromCacheEx( hCache, arrChrom, nChromLen, arrMeanFits, arrStdFits )
		integer :: hCache
		integer, dimension(*) :: arrChrom
		double precision, dimension(*), intent(out) :: arrMeanFits, arrStdFits
		integer, intent(in) :: nChromLen
	end function

	subroutine AddSampleChromCache( hCache, arrChrom, nChromLen, nSampleId )
		integer :: hCache
		integer, dimension(*) :: arrChrom
		integer, intent(in) :: nChromLen, nSampleId
	end subroutine
	subroutine AddSampleChromCacheEx( hCache, arrChrom, nChromLen, nSampleId )
		integer :: hCache
		integer, dimension(*) :: arrChrom
		integer, intent(in) :: nChromLen, nSampleId
	end subroutine

	integer function GetSampleCountCache( hCache, arrChrom, nChromLen )
		integer :: hCache
		integer, dimension(*) :: arrChrom
		integer, intent(in) :: nChromLen
	end function
	integer function GetSampleCountCacheEx( hCache, arrChrom, nChromLen )
		integer :: hCache
		integer, dimension(*) :: arrChrom
		integer, intent(in) :: nChromLen
	end function

	subroutine GetSampleIdsCache( hCache, arrChrom, nChromLen, arrSampleIds )
		integer :: hCache
		integer, dimension(*) :: arrChrom
		integer, intent(in) :: nChromLen
		integer, dimension(*), intent(out) :: arrSampleIds
	end subroutine

	double precision function GetMeanSampleCountCache( hCache )
		integer :: hCache
	end function
	double precision function GetMeanSampleCountCacheEx( hCache )
		integer :: hCache
	end function

	subroutine ForEachChromCache( hCache, SaveChrom )
		integer :: hCache
		external :: SaveChrom
	end subroutine

	subroutine ReleaseChromCache( hCache )
		integer :: hCache
	end subroutine
	subroutine ReleaseChromCacheEx( hCache )
		integer :: hCache
	end subroutine

	subroutine DumpChrom( hCache, arrChrom, nChromLen )
		integer :: hCache
		integer, dimension(*) :: arrChrom
		integer, intent(in) :: nChromLen
	end subroutine

	subroutine DUMP( hCache )
		integer :: hCache
	end subroutine
	subroutine DUMPEX( hCache )
		integer :: hCache
	end subroutine

end interface

end module chmcache