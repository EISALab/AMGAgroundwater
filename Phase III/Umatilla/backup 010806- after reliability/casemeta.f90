module casemeta
use umacase
use scaling
implicit none

integer, parameter :: hGAOut	= 1000
integer, parameter :: hGaStat	= 1001
integer, parameter :: hGaNN		= 1002
integer, parameter :: hGaCount	= 1003
integer, parameter :: hTrainFit = 1004
integer, parameter :: hModelError = 1005
                                                                                                                                                                                                          
character(*), parameter :: strGaOut	= 'ga.out'  
character(*), parameter :: strGaStat = 'ga.stat'
character(*), parameter :: strGaNN = 'gann.out'
character(*), parameter :: strGaCount = 'gacount.out'
character(*), parameter :: strTrainFit = 'trainfit.dat'
character(*), parameter :: strModelError = 'modelerr.dat'

character(*), parameter :: strFourYCTrain = 'umafourc.dat'
character(*), parameter :: strFiveYCTrain = 'umafivec.dat'

contains

subroutine PrepareOutputFiles
implicit none
	
	integer :: ret

	ret=RemoveFileC( strGaOut )
	ret=RemoveFileC( strGaStat )
	ret=RemoveFileC( strFourYCTrain )
	ret=RemoveFileC( strFiveYCTrain )
	ret=RemoveFileC( strGaNN )
	ret=RemoveFileC( strGaCount )
	ret=RemoveFileC( strTrainFit )
	ret=RemoveFileC( strModelError )

	call OpenOutputFile( hGaCount, strGaCount )
	rewind( hGaCount )
	write(hGaCount, * ) 'Gen#	PDE#	CACHE#	NNET#'
	close( hGaCount)

	call OpenOutputFile( hModelError, strModelError )
	rewind( hModelError )
	write(hModelError, * )'Gen#      NN#        ErrMean        sga%ErrMean        ErrStd         sga%ErrStd'
	close( hModelError)

end subroutine

subroutine CreateScalings( hMeanNorm )
implicit none
	!arguments
	integer, intent(out) :: hMeanNorm

	!variables
	double precision, dimension(3*g_nNewWells + g_nOldWells + 5) :: arrOrigMin, arrOrigMax			!(x,y), pumping rates for new wells, old wells
	double precision, dimension(3*g_nNewWells + g_nOldWells + 5) :: arrNormMin, arrNormMax

	integer :: i, nInputs, nOutputs

	nInputs = 3*g_nNewWells + g_nOldWells
	nOutputs = 5

	arrOrigMin = 0
	arrOrigMax = 0
	arrNormMin = (/(-1.0, i=1,nInputs), (-0.9, i=1,nOutputs)/)
	arrNormMax = (/(1.0, i=1,nInputs), (0.9, i=1,nOutputs)/)

	call PrepareScallingRange( arrOrigMin, arrOrigMax, nInputs+nOutputs )
	hMeanNorm = CreateNormalize( arrOrigMin, arrOrigMax, arrNormMin, arrNormMax, nInputs+nOutputs )

end subroutine

subroutine ReleaseScalings( hMeanNorm )
implicit none
	!arguments
	integer, intent(in) :: hMeanNorm

	call ReleaseNormalize( hMeanNorm )
end subroutine

!this function assume the arrMax and arrMin hold the maximum and minimum value of 
!well locations and pumping rates, the subroutine doens't count the heads and risks
subroutine PrepareScallingRange( arrMin, arrMax, nCounts )
use umacase
implicit none
	!arguments 
	integer, intent(in) :: nCounts
	double precision, dimension(nCounts), intent(out) :: arrMin
	double precision, dimension(nCounts), intent(out) :: arrMax
	
	!variables 
	integer :: i, j

	!well locations (x,y) max and min
	do i=1, g_nNewWells
		arrMax(2*i-1) = g_parrNewWells(i)%parrLocs(1)%x
		arrMax(2*i) = g_parrNewWells(i)%parrLocs(1)%y
		arrMin(2*i-1) = arrMax(2*i-1)
		arrMin(2*i) = arrMax(2*i)

		do j=2, g_parrNewWells(i)%nNodes
			arrMax(2*i-1) = max( arrMax(2*i-1), dble(g_parrNewWells(i)%parrLocs(j)%x) )
			arrMax(2*i) = max( arrMax(2*i), dble(g_parrNewWells(i)%parrLocs(j)%y) )
			arrMin(2*i-1) = min( arrMin(2*i-1), dble(g_parrNewWells(i)%parrLocs(j)%x) )
			arrMin(2*i) = min( arrMin(2*i), dble(g_parrNewWells(i)%parrLocs(j)%y) )
		end do
	enddo

	!pumping rates
	arrMin(2*g_nNewWells+1:3*g_nNewWells+g_nOldWells) = (/(0.0, i=1, g_nNewWells), (0.0, i=1, g_nOldWells)/)
	arrMax(2*g_nNewWells+1:3*g_nNewWells+g_nOldWells) = (/(1.0, i=1, g_nNewExtWells), 6.0, 2.5, 2.2, (1.0, i=1, g_nOldExtWells), 5.0, 2.0, 3.1/)

	!rReliab, VCG, rdxC, tntC
	arrMin(3*g_nNewWells+g_nOldWells+1:3*g_nNewWells+g_nOldWells+4) = (/0.0, -1.0, -0.3, -0.3/)
	arrMax(3*g_nNewWells+g_nOldWells+1:3*g_nNewWells+g_nOldWells+4) = (/1.0, 3.5, 1.4, 1.9/)

	!max rdx c, max tnt c and dummy VCG cost
!	arrMin(3*g_nNewWells+g_nOldWells+1:3*g_nNewWells+g_nOldWells+5) = (/0.5, 0.5, -1.0, -0.3, -0.3/)
!	arrMax(3*g_nNewWells+g_nOldWells+1:3*g_nNewWells+g_nOldWells+5) = (/1.0, 1.0, 3.5, 1.4, 1.9/)
!	arrMin(3*g_nNewWells+g_nOldWells+1:3*g_nNewWells+g_nOldWells+4) = (/0.0, 0.0, 0.0, -1.0/)
!	arrMin(3*g_nNewWells+g_nOldWells+1:3*g_nNewWells+g_nOldWells+5) = (/-1.0, -1.0, -1.0, -1.0, -1.0/)
!	arrMax(3*g_nNewWells+g_nOldWells+1:3*g_nNewWells+g_nOldWells+5) = (/2.5, 2.5, 3.5, 2.5, 2.5/)
!	arrMax(3*g_nNewWells+g_nOldWells+1:3*g_nNewWells+g_nOldWells+4) = (/100.0, 100.0, 3.5, 1.0/)
!	arrMax(3*g_nNewWells+g_nOldWells+1:3*g_nNewWells+g_nOldWells+4) = (/100.0, 100.0, 25.0, 1.0/)
!	arrMin(3*g_nNewWells+g_nOldWells+1:3*g_nNewWells+g_nOldWells+3) = (/0.0, 0.0, 0.0/)
!	arrMax(3*g_nNewWells+g_nOldWells+1:3*g_nNewWells+g_nOldWells+3) = (/100.0, 100.0, 25.0/)
!	arrMin(3*g_nNewWells+g_nOldWells+1:3*g_nNewWells+g_nOldWells+3) = (/0.0, 0.0, 3.0/)
!	arrMax(3*g_nNewWells+g_nOldWells+1:3*g_nNewWells+g_nOldWells+3) = (/100.0, 100.0, 4.5/)
end subroutine PrepareScallingRange

subroutine CleanTrainingFiles
implicit none
	integer :: ret

	ret=RemoveFileC( strFourYCTrain )
	ret=RemoveFileC( strFiveYCTrain )
end subroutine


subroutine AppendToFile( strFile, arrVars, n )                                                                                                                                                    
implicit none                                                                                                                                                                                     
	!arguments                                                                                                                                                                                
	character(*) :: strFile                                                                                                                                                                   
	double precision, dimension(:) :: arrVars                                                                                                                                                 
	integer :: n                                                                                                                                                                              
                                                                                                                                                                                                      
	!variable                                                                                                                                                                                 
	integer, parameter :: hFile = 89                                                                                                                                                          
	integer :: i                                                                                                                                                                              
                                                                                                                                                                                                      
	open( unit=hFile, file=strFile, status='unknown', position='append' )                                                                                                                     
	do i=1, n                                                                                                                                                                                 
		write( hFile, fmt=1001, advance="NO")arrVars(i)                                                                                                                                   
	enddo                                                                                                                                                                                     
                                                                                                                                                                                                      
	write(hFile, fmt=1002, advance="YES")                                                                                                                                                     
                                                                                                                                                                                                      
	close(hFile)                                                                                                                                                                              
	                                                                                                                                                                                              
	1001 format(2x,f10.3)                                                                                                                                                                     
	1002 format(1x)                                                                                                                                                                           
end subroutine                                                                                                                                                                                    

end module