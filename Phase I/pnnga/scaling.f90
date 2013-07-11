module scaling
implicit none
type NORMALIZEPARAM
	integer :: prOrigMin, prOrigMax
	integer :: prNormMin, prNormMax
	integer:: nLen
end type

interface
!*****************subroutine interface for neural network**************************                     
	integer function CreateNormalize( arrOrigMin, arrOrigMax, arrNormMin, arrNormMax, nLen )
		double precision, dimension(*) :: arrOrigMin, arrOrigMax, arrNormMin, arrNormMax
		integer, intent(in) :: nLen
	end function

	subroutine ReleaseNormalize( hNormParam )
		integer, intent(in) :: hNormParam
	end subroutine

	subroutine Normalize( hNormParam, arrData, n )
		integer, intent(in) :: hNormParam
		double precision, dimension(*) :: arrData
		integer :: n
	end subroutine

	subroutine UnNormalize( hNormParam, arrData, n )
		integer, intent(in) :: hNormParam
		double precision, dimension(*) :: arrData
		integer :: n
	end subroutine
end interface

end module