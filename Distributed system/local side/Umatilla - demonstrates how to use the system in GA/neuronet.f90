module neuronet
implicit none

interface                                                                                               
!*****************subroutine interface for neural network**************************                     
	integer function CreateNet( arrNeuros, nLayers )
		integer, dimension(*), intent(in) :: arrNeuros
		integer, intent(in):: nLayers
	end function

	double precision function TrainMatlabNetC1( strM, strTrain, strNet )
		character(*) :: strM, strTrain, strNet
	end function

	subroutine SimNet( hNet, arrInput, arrOutput )
		integer :: hNet
		double precision, dimension(*) :: arrInput
		double precision, dimension(*), intent(out) :: arrOutput
	end subroutine

	double precision function TestNet( hNet, arrInput, arrOutput, nPattern )
		integer :: hNet
		double precision, dimension(*) :: arrInput
		double precision, dimension(*), intent(out) :: arrOutput
		integer :: nPattern
	end function

	subroutine ReleaseNet( hNet )
		integer :: hNet
	end subroutine

	subroutine LoadMatlabNet( hNet, strNetFile )
		integer :: hNet
		character(*) :: strNetFile
	end subroutine

	double precision function TrainMatlabNetC2( strM, strTrain, strNet, strValid )
		character(*) :: strM, strTrain, strNet, strValid
	end function

	double precision function TrainMatlabNetEx( strM, arrNodes, nLayers, arrNetBias, strTrain, strNet, strValid )
		character(*) :: strM, strTrain, strNet, strValid
		integer, dimension(*), intent(in) :: arrNodes
		integer, intent(in):: nLayers
		double precision, dimension(*), intent(in) :: arrNetBias
	end function

end interface

contains                                                                                          
	double precision function TrainMatlabNet( strM, strTrain, strNet, strValid )                                             
		character(*) :: strM, strTrain, strNet
		character(*), optional :: strValid

		if( present( strValid ) )then
			TrainMatlabNet = TrainMatlabNetC2( strM, strTrain, strNet, strValid )
		else
			TrainMatlabNet = TrainMatlabNetC1( strM, strTrain, strNet )
		end if
	end function
                                                                                                        
end module                                                                                              