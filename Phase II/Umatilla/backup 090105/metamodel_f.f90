module metamodel
implicit none

integer, parameter :: MODEL_NEURAL			= 0
integer, parameter :: MODEL_SVM				= 1
integer, parameter :: MODEL_REGIONAL_NEURAL = 2
integer, parameter :: MODEL_REGIONAL_SVM	= 3

integer, dimension(3) :: arrUmaNet = (/28, 12, 3/)
integer, dimension(3) :: arrCostNet = (/28, 12, 2/)


!character(*), parameter :: strUmaSvmOpt1 = '-s 3 -t 2 -c 100 -g 0.16 -p 0.07'
!character(*), parameter :: strUmaSvmOpt2 = '-s 3 -t 2 -c 100 -g 0.16 -p 0.4'
character(*), parameter :: strUmaSvmOpt1 = '-s 3 -t 2 -c 100 -g 0.16 -p 0.05'
character(*), parameter :: strUmaSvmOpt2 = '-s 3 -t 2 -c 100 -g 0.16 -p 0.04'
character(*), parameter :: strUmaSvmOpt3 = '-s 3 -t 2 -c 100 -g 0.1 -p 0.0001'

character(*), parameter :: strCostSvmOpt1 = '-s 3 -t 2 -c 100 -g 0.1 -p 0.001'
character(*), parameter :: strCostSvmOpt2 = '-s 3 -t 2 -c 100 -g 0.1 -p 0.01'

!*****************subroutine interface for all meta models**************************                     
interface
	integer function create_meta_model( nModelType )
	implicit none
		integer :: nModelType
	end function

	subroutine release_meta_model( hModel )
	implicit none
		integer :: hModel
	end subroutine

	subroutine train_meta_model( hModel, strTrainData, strValidData )
	implicit none
		integer, intent(in) :: hModel
		character(*) :: strTrainData, strValidData
	end subroutine

	subroutine predict_meta_model( hModel, arrInput, arrOutput )
		integer :: hModel
		double precision, dimension(*) :: arrInput
		double precision, dimension(*), intent(out) :: arrOutput
	end subroutine

	double precision function test_meta_model( hModel, arrInput, arrOutput, nPattern )
		integer :: hModel
		double precision, dimension(*), intent(in) :: arrInput
		double precision, dimension(*), intent(out) :: arrOutput
		integer, intent(in) :: nPattern
	end function

	integer function get_model_count( hModel )
		integer :: hModel
	end function

end interface

end module metamodel

module neural_model
use metamodel
implicit none

contains
	integer function create_model()
		create_model = create_meta_model( MODEL_NEURAL );
	end function

	subroutine initialize( hModel, arrNodes, nLayers, arrBias )
	implicit none
		integer, intent(in) :: hModel
		integer, dimension(*), intent(in) :: arrNodes
		integer, intent(in) :: nLayers
		double precision, dimension(*), intent(in) :: arrBias

		call init_neural_model( hModel, arrNodes, nLayers, arrBias )
	end subroutine
end module neural_model

module svm_model
use metamodel
implicit none

contains
	integer function create_model()
		create_model = create_meta_model( MODEL_SVM );
	end function

!	subroutine initialize( hModel, nInputs, nOutputs, strOptions )
	subroutine initialize( hModel, nInputs, nOutputs )
	implicit none
		integer, intent(in) :: hModel
		integer, intent(in) :: nInputs, nOutputs
!		character(*) :: strOptions

!		call init_svm_model( hModel, nInputs, nOutputs, strOptions )
		call init_svm_model( hModel, nInputs, nOutputs )
	end subroutine
	subroutine set_options( hModel, strOptions, id )
	implicit none
		integer, intent(in) :: hModel
		character(*) :: strOptions
		integer, intent(in) :: id
		
		call set_svm_options( hModel, strOptions, id )
	end subroutine
end module svm_model

module regional_neural_model
use metamodel
implicit none

contains
	integer function create_model()
		create_model = create_meta_model( MODEL_REGIONAL_NEURAL );
	end function

	subroutine initialize( hModel, arrPtCen, nMinCutPts, arrNodes, nLayers, arrBias )
	implicit none
		integer, intent(in) :: hModel
		double precision, dimension(*), intent(in) :: arrPtCen
		integer, dimension(*), intent(in) :: arrNodes
		integer, intent(in) :: nMinCutPts, nLayers
		double precision, dimension(*), intent(in) :: arrBias

		call init_regional_neural_model( hModel, arrPtCen, nMinCutPts, arrNodes, nLayers, arrBias )
	end subroutine
end module regional_neural_model

module regional_svm_model
use metamodel
implicit none

contains
	integer function create_model()
		create_model = create_meta_model( MODEL_REGIONAL_SVM );
	end function

!	subroutine initialize( hModel, arrPtCen, nMinCutPts, nInputs, nOutputs, strOptions )
	subroutine initialize( hModel, arrPtCen, nMinCutPts, nInputs, nOutputs )
	implicit none
		integer, intent(in) :: hModel
		double precision, dimension(*), intent(in) :: arrPtCen
		integer, intent(in) :: nInputs, nOutputs, nMinCutPts
!		character(*) :: strOptions

!		call init_regional_svm_model( hModel, arrPtCen, nMinCutPts, nInputs, nOutputs, strOptions )
		call init_regional_svm_model( hModel, arrPtCen, nMinCutPts, nInputs, nOutputs )
	end subroutine

	subroutine set_options( hModel, strOptions, id )
	implicit none
		integer, intent(in) :: hModel
		character(*) :: strOptions
		integer, intent(in) :: id
		
		call set_regional_svm_options( hModel, strOptions, id )
	end subroutine
end module regional_svm_model
