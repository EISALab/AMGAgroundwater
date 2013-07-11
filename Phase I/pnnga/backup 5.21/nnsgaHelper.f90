module nnsgahelper                                                                                      
use neuroio                                                                                             
implicit none                                                                                           
                                                                                                        
interface                                                                                               
!******************subroutine interface for chromosome cache********************* 
	integer function CreateChromCache
	end function CreateChromCache

	subroutine InsertChromCache( hCache, arrChrom, nChromLen, arrFits, nFits )                                        
		integer :: hCache
		integer, dimension(*) :: arrChrom
		double precision, dimension(*):: arrFits
		integer, intent(in) :: nChromLen, nFits
	end subroutine InsertChromCache
                                                                                                        
	integer function SearchChromCache( hCache, arrChrom, nChromLen, arrFits, nFits )
		integer :: hCache
		integer, dimension(*) :: arrChrom
		double precision, dimension(*), intent(out) :: arrFits
		integer, intent(in) :: nChromLen, nFits
	end function SearchChromCache                                                                   
                                                                                                        
	subroutine ReleaseChromCache( hCache )
		integer :: hCache
	end subroutine ReleaseChromCache                                                                
                                                                                                        
!*****************subroutine interface for neural network**************************                     
                                                                                                        
!!!!!!!!!!!!!!!!!!!!!!subrotines for creating, training, and simulating neural network!!!!!!!!!!!!!!!!!!
	integer function CreateNet( netParam )                                                          
	use neuroio                                                                                     
		type(NEURALNETPARAM) :: netParam                                                        
	end function                                                                                    
                                                                                                        
	double precision function TrainMatlabNetC1( strM, strTrain, strNet )                                             
		character(*) :: strM, strTrain, strNet
	end function
                                                                                                        
	double precision function TrainMatlabNetC2( strM, strTrain, strNet, strValid )                                             
		character(*) :: strM, strTrain, strNet, strValid
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
                                                                                                        
	subroutine LoadNetFromMatlab( hNet, strNet )                                                    
		integer :: hNet                                                                         
		character(*) :: strNet                                                                  
	end subroutine                                                                                  
                                                                                                        
	subroutine AssocLayerNetParam( netLayers, netParam )                                            
	use neuroio                                                                                     
		type(NETLAYERPARAM), dimension(*) :: netLayers                                          
		type(NEURALNETPARAM) :: netParam                                                        
	end subroutine                                                                                  
                                                                                                        
	subroutine AssocNormlizeParam( arrOrigMin, arrOrigMax, arrNormMin, arrNormMax, normParam )      
	use neuroio                                                                                     
		double precision, dimension(*) :: arrOrigMin, arrOrigMax, arrNormMin, arrNormMax        
		type(NORMALIZEPARAM) :: normParam                                                       
	end subroutine                                                                                  
                                                                                                        
	subroutine Normalize( normParam, arrData, n )                                                   
	use neuroio                                                                                     
		type(NORMALIZEPARAM) :: normParam                                                       
		double precision, dimension(*) :: arrData                                               
		integer :: n                                                                            
	end subroutine                                                                                  
                                                                                                        
	subroutine UnNormalize( normParam, arrData, n )                                                 
	use neuroio                                                                                     
		type(NORMALIZEPARAM) :: normParam                                                       
		double precision, dimension(*) :: arrData                                               
		integer :: n                                                                            
	end subroutine                                                                                  
                                                                                                        
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