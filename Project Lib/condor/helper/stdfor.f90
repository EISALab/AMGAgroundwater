module stdfor
use dfport

!this is the module file which contains the necessary file and directory manipulation subroutines
!to create the necessary directories for file transportation in condor system

interface

	double precision function str_to_dble( str )
		character(*), intent(in) :: str
	end function

	subroutine int_to_str( val, str )
		integer, intent(in) :: val
		character(*), intent(out) :: str
	end subroutine

	subroutine str_cat( str1, str2, strRet )
		character(*), intent(in) :: str1, str2
		character(*), intent(out) :: strRet
	end subroutine

	integer function copy_file( strSrcFile, strDesFile )
		character(*), intent(in) :: strSrcFile, strDesFile
	end function

	integer function remove_file( strFile )
		character(*), intent(in) :: strFile
	end function

	integer function copy_dir( strSrcPath, strDesPath )
		character(*), intent(in) :: strSrcPath, strDesPath
	end function
	
	integer function remove_dir( strPath )
		character(*), intent(in) :: strPath
	end function

	integer function count_nonblank_lines( strFile )
		character(*), intent(in) :: strFile
	end function

	subroutine execute( strExec )
		character(*), intent(in) :: strExec
	end subroutine

	double precision function StrToFC( str )
		character(*), intent(in) :: str
	end function StrToFC

	integer function CopyFileC( strSrcFile, strDesFile )
		character(*), intent(in) :: strSrcFile, strDesFile
	end function CopyFileC

	integer function CopyDirC( strSrcPath, strDesPath )
		character(*), intent(in) :: strSrcPath, strDesPath
	end function CopyDirC
	
	integer function RemoveDirC( strPath )
		character(*), intent(in) :: strPath
	end function RemoveDirC

	integer function RemoveFileC( strFile )
		character(*), intent(in) :: strFile
	end function RemoveFileC

	subroutine WaiteForExec( strExec )
		character(*), intent(in) :: strExec
	end subroutine WaiteForExec
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

contains

subroutine ChangeDir( strPath )
implicit none

	character(*), intent(in) :: strPath
	integer :: istatus

	istatus = CHDIR(strPath)
	if( istatus/=0 )then
		print*, 'ERROR:trying to change path to ', strPath, 'failed!'
		stop
	end if
end subroutine ChangeDir

subroutine RemoveFile( strFile )
implicit none
	character(*), intent(in) :: strFile
	integer :: istatus

	istatus = remove_file( strFile )
end subroutine

subroutine CopyFile( strSrc, strDes )
implicit none
	character(*), intent(in) :: strSrc, strDes
	integer :: istatus

	istatus = copy_file(strSrc, strDes)
	if( istatus/=0 )then
		print*, 'ERROR:copy file ', strSrc, ' to ', strDes, 'failed!'
		stop
	end if
end subroutine CopyFile

subroutine MakeDir( strNewPath )
use dflib
implicit none
	character(*), intent(in) :: strNewPath

	logical(4) :: lResult

	lResult = makedirqq(strNewPath)

end subroutine MakeDir

subroutine CopyDir( strSrcPath, strDesPath )
implicit none
	character(*), intent(in) :: strSrcPath, strDesPath
	integer :: istatus

	istatus = copy_dir( strSrcPath, strDesPath )
	if( istatus/=0 )then
		print*, 'ERROR:copy directory ', strSrcPath, ' to ', strDesPath, ' failed!'
		stop
	end if
end subroutine CopyDir
	
subroutine RemoveDir( strPath )
implicit none
	character(*), intent(in) :: strPath
	integer :: istatus

	istatus = remove_dir( strPath )
	if( istatus/=0 )then
		print*, 'ERROR:remove directory ', strPath, ' failed!'
		!stop
	end if
end subroutine RemoveDir

end module