module neuro_cluster
implicit none

interface                                                                                               
!*****************subroutine interface for neural network cluster**************************                     
	integer function create_net_cluster()
	end function

	subroutine release_net_cluster( hNets )
	implicit none
		integer :: hNets
	end subroutine

	subroutine train_net_cluster( hNets, strTrainData, arrPtCen, arrNodes, nLayers, arrBias, nMinPts )
	implicit none
		integer, intent(in) :: hNets
		character(*) :: strTrainData
		double precision, dimension(*), intent(in) :: arrPtCen
		integer, dimension(*), intent(in) :: arrNodes
		integer, intent(in) :: nLayers
		double precision, dimension(*), intent(in) :: arrBias
		integer, intent(in) :: nMinPts
	end subroutine

	subroutine sim_net_cluster( hNets, arrInput, arrOutput )
		integer :: hNets
		double precision, dimension(*) :: arrInput
		double precision, dimension(*), intent(out) :: arrOutput
	end subroutine

	double precision function test_net_cluster( hNets, arrInput, arrOutput, nPattern )
		integer, intent(in) :: hNets
		double precision, dimension(*), intent(in) :: arrInput
		double precision, dimension(*), intent(out) :: arrOutput
		integer, intent(in) :: nPattern
	end function

end interface

end module                                                                                              