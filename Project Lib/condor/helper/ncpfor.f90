module ncpfor
implicit none

!this is the interface for using ncp framework in GA
interface
	subroutine CopySlaveFiles( strFileList )
		character(*), intent(in) :: strFileList
	end subroutine CopySlaveFiles

	subroutine InitSocket
	end subroutine InitSocket

	subroutine ClearSocket
	end subroutine ClearSocket

	subroutine RefreshHosts
	end subroutine RefreshHosts

	subroutine RouteSlaves( arrJobIds, nJobCount )
		integer, dimension(*), intent(in) :: arrJobIds
		integer, intent(in) :: nJobCount
	end subroutine RouteSlaves
end interface

interface
	subroutine ncp_initalize
	end subroutine

	subroutine ncp_cleanup
	end subroutine

	subroutine ncp_refresh_nodes
	end subroutine

	subroutine ncp_distribute_jobs( arrJobIds, nJobCount )
		integer, dimension(*), intent(in) :: arrJobIds
		integer, intent(in) :: nJobCount
	end subroutine
end interface


end module ncpfor
