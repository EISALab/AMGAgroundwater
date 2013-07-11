module casemeta
use CASEWELL
use stdfor
use scaling
use neuronet
implicit none

integer, parameter :: hGAOut	= 1000
integer, parameter :: hPopStat	= 1001
integer, parameter :: hGaNN		= 1002
integer, parameter :: hGaCount	= 1003

integer, parameter :: hHeadTrain	= 2000
integer, parameter :: hRiskTrain	= 2002

character(*), parameter :: strGaOut	= 'ga.out'  
character(*), parameter :: strPopStat = 'pop.stat'
character(*), parameter :: strGaNN = 'gann.out'
character(*), parameter :: strGaCount = 'gacount.out'

character(*), parameter :: strHeadTrain = 'headtrain.dat'
character(*), parameter :: strHeadTest = 'headtest.dat'

character(*), parameter :: strRiskTrain = 'risktrain.dat'
character(*), parameter :: strRiskTest = 'risktest.dat'

character(*), parameter :: strHeadM = 'univnet'
character(*), parameter :: strRiskM = 'univnet'

character(*), parameter :: strHeadNN = 'headnneu.dat'
character(*), parameter :: strRiskNN = 'risknneu.dat'

integer, dimension(3) :: arrHeadNet = (/9, 12, 3/)
integer, dimension(3) :: arrRiskNet = (/12, 15, 2/)

contains

subroutine CleanTrainingFiles
implicit none
	integer :: ret

	ret=RemoveFileC( strHeadTrain )
	ret=RemoveFileC( strHeadTest )
	ret=RemoveFileC( strRiskTrain )
	ret=RemoveFileC( strRiskTest )

end subroutine

subroutine PrepareOutputFiles
implicit none
	
	integer :: ret

	ret=RemoveFileC( strGaOut )
	ret=RemoveFileC( strPopStat )
	ret=RemoveFileC( strHeadNN )
	ret=RemoveFileC( strRiskNN )
	ret=RemoveFileC( strGaNN )
	ret=RemoveFileC( strGaCount )

	call CleanTrainingFiles

!	call OpenOutputFile( hGaCount, strGaCount )
!	rewind( hGaCount )
!	write(hGaCount, * ) 'Gen#	PDE#	CACHE#	NNET#'
!	close( hGaCount)

	open( unit = hGaOut, file=strHeadTest, status='unknown' )
	rewind( hGaOut )
	close( hGaOut )
	open( unit = hGaOut, file=strRiskTest, status='unknown' )
	rewind( hGaOut )
	close( hGaOut )
end subroutine

subroutine CreateScalings( normHead, normRisk )
implicit none
	!arguments
	integer, intent(out) :: normHead, normRisk

	!variables
	double precision, dimension(4*g_nRemWells+2) :: arrOrigMin, arrOrigMax			!(x,y), head, riskb, riskw.
	double precision, dimension(4*g_nRemWells) :: arrHeadNormMin, arrHeadNormMax
	double precision, dimension(4*g_nRemWells+2) :: arrRiskNormMin, arrRiskNormMax
	integer :: i

	arrOrigMin = 0
	arrOrigMax = 0
	arrHeadNormMin = (/(-1.0, i=1,3*g_nRemWells), (-0.9, i=1,g_nRemWells)/)
	arrHeadNormMax = (/(1.0, i=1,3*g_nRemWells), (0.9, i=1,g_nRemWells)/)
	arrRiskNormMin = (/(-1.0, i=1,4*g_nRemWells), -0.9, -0.9/)
	arrRiskNormMax = (/(1.0, i=1,4*g_nRemWells), 0.9, 0.9/)

	call PrepareScallingRange(arrOrigMin, arrOrigMax, 4*g_nRemWells+2)
	normHead = CreateNormalize( arrOrigMin, arrOrigMax, arrHeadNormMin, arrHeadNormMax, 4*g_nRemWells )
	normRisk = CreateNormalize( arrOrigMin, arrOrigMax, arrRiskNormMin, arrRiskNormMax, 4*g_nRemWells+2 )
end subroutine

subroutine ReleaseScalings( normHead, normRisk )
implicit none
	!arguments
	integer, intent(in) :: normHead, normRisk

	call ReleaseNormalize( normHead )
	call ReleaseNormalize( normRisk )
end subroutine

subroutine CreateANNs( hHeadAnn, hRiskAnn )
implicit none
	!arguments
	integer, intent(out) :: hHeadAnn, hRiskAnn

	hHeadAnn = CreateNet( arrHeadNet, 3 )
	hRiskAnn = CreateNet( arrRiskNet, 3 )
end subroutine

subroutine LoadMatlabAnns( hHeadAnn, hRiskAnn )
implicit none
	!arguments
	integer, intent(out) :: hHeadAnn, hRiskAnn

	call LoadMatlabNet( hHeadAnn, strHeadNN )
	call LoadMatlabNet( hRiskAnn, strRiskNN )
end subroutine

subroutine ReleaseANNs( hHeadAnn, hRiskAnn )
implicit none
	integer, intent(in) :: hHeadAnn, hRiskAnn

	call ReleaseNet( hHeadAnn )
	call ReleaseNet( hRiskAnn )
end subroutine


!this function assume the arrMax and arrMin hold the maximum and minimum value of 
!well locations and pumping rates, the subroutine doens't count the heads and risks
subroutine PrepareScallingRange( arrMin, arrMax, nCounts )
implicit none
	!arguments 
	integer, intent(in) :: nCounts
	double precision, dimension(nCounts), intent(out) :: arrMin
	double precision, dimension(nCounts), intent(out) :: arrMax
	
	!variables 
	integer :: i, j
!	call assert( nCounts >= GetHeadNNInpCount()+GetHeadNNOutCount() )

	!well locations (x,y) max and min
	do i=1, g_nRemWells
		arrMax(2*i-1) = g_parrWellInfos(i)%parrLocs(1)%x
		arrMax(2*i) = g_parrWellInfos(i)%parrLocs(1)%y
		arrMin(2*i-1) = arrMax(2*i-1)
		arrMin(2*i) = arrMax(2*i)

		do j=2, g_parrWellInfos(i)%nNodes
			arrMax(2*i-1) = max( arrMax(2*i-1), dble(g_parrWellInfos(i)%parrLocs(j)%x) )
			arrMax(2*i) = max( arrMax(2*i), dble(g_parrWellInfos(i)%parrLocs(j)%y) )
			arrMin(2*i-1) = min( arrMin(2*i-1), dble(g_parrWellInfos(i)%parrLocs(j)%x) )
			arrMin(2*i) = min( arrMin(2*i), dble(g_parrWellInfos(i)%parrLocs(j)%y) )
		end do
	enddo

	!pumping rates
	do i=1, g_nRemWells
		arrMax(2*g_nRemWells+i) = g_parrWellInfos(i)%rMaxPumpRate
		arrMin(2*g_nRemWells+i) = -g_parrWellInfos(i)%rMaxPumpRate
	enddo

	!hydraulic heads
	do i=1, g_nRemWells
		arrMax(3*g_nRemWells+i) = 24.0
		arrMin(3*g_nRemWells+i) = 16.0
	enddo

	!-log10(riskb) and -log10(riskw)
	arrMax(4*g_nRemWells+1) = 12.0
	arrMin(4*g_nRemWells+1) = 1.0
	arrMax(4*g_nRemWells+2) = 8.0
	arrMin(4*g_nRemWells+2) = 0.0
end subroutine PrepareScallingRange

!the subroutine put the locations(x,y), Q, Heads, riskb and riskw into array arrVars.
!arrHeads, rRiskB, rRiskW are optional parameters. They are put into arrVars only when they are present.
subroutine ParseVars( arrVars, arrHeads, rRiskB, rRiskW )
implicit none
	!arguments                                                                                                                                                                                
	double precision, dimension(:) :: arrVars
	double precision, dimension(:), optional, intent(in) :: arrHeads
	double precision, optional, intent(in) :: rRiskB, rRiskW

	!variables
	integer :: i
	double precision :: rB, rW

	do i=1, g_nRemWells
		arrVars(2*i-1) = g_parrWellInfos(i)%parrLocs(g_arrWellLocs(i))%x
		arrVars(2*i) = g_parrWellInfos(i)%parrLocs(g_arrWellLocs(i))%y
	enddo

	do i=1, g_nRemWells
		arrVars(2*g_nRemWells+i)= g_arrPumpRates(i)
	enddo

	if( present(arrHeads) )then
		do i=1, g_nRemWells
			arrVars(3*g_nRemWells+i) =arrHeads(i)
		enddo
	endif

	if( present(rRiskB ) ) then
		rB = rRiskB
		rW = rRiskW
		if( rRiskB <=1e-12 )rB = 1e-12
		if( rRiskW <=1e-8 )rW = 1e-8
		arrVars(4*g_nRemWells+1) = -log10(rB)
		arrVars(4*g_nRemWells+2) = -log10(rW)
	endif
end subroutine

subroutine OrderVars( arrVars, arrWellIndex )
implicit none
	!arguments
	double precision, dimension(:) :: arrVars
	integer, dimension(:) :: arrWellIndex

	double precision, dimension( ubound(arrVars,1)-lbound(arrVars,1)+1 ) :: arrBuf
	integer :: i, id

	arrBuf = arrVars
	do i=1, g_nRemWells
		id = arrWellIndex(i)
		arrBuf( 2*i-1 ) = arrVars(2*id-1)
		arrBuf( 2*i ) = arrVars(2*id)
	enddo
                                                                                                                                                                                                      
	do i=1, g_nRemWells
		id = arrWellIndex(i)
		arrBuf(2*g_nRemWells+i) = arrVars(2*g_nRemWells+id)
	enddo
                                                                                                                                                                                                      
	do i=1, g_nRemWells
		id = arrWellIndex(i)
		arrBuf(3*g_nRemWells+i) = arrVars(3*g_nRemWells+id)
	enddo

	arrVars = arrBuf
end subroutine
                                                                                                                                                                                                          
subroutine UnOrderVars( arrVars, arrWellIndex )
implicit none
	!arguments
	double precision, dimension(:) :: arrVars
	integer, dimension(:) :: arrWellIndex

	double precision, dimension( ubound(arrVars,1)-lbound(arrVars,1)+1 ) :: arrBuf
	integer :: i, id

	arrBuf = arrVars
	do i=1, g_nRemWells
		id = arrWellIndex(i)
		arrBuf( 2*id-1 ) = arrVars(2*i-1)
		arrBuf( 2*id ) = arrVars(2*i)
	enddo
                                                                                                                                                                                                      
	do i=1, g_nRemWells
		id = arrWellIndex(i)
		arrBuf(2*g_nRemWells+id) = arrVars(2*g_nRemWells+i)
	enddo
                                                                                                                                                                                                      
	do i=1, g_nRemWells
		id = arrWellIndex(i)
		arrBuf(3*g_nRemWells+id) = arrVars(3*g_nRemWells+i)
	enddo

	arrVars = arrBuf
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

subroutine AppendToFileEx( hFile, arrVars, n )                                                                                                                                                    
implicit none                                                                                                                                                                                     
	!arguments
	integer, intent(in) :: hFile
	double precision, dimension(:) :: arrVars
	integer :: n

	!variable
	integer :: i
                                                                                                                                                                                                      
	do i=1, n
		write( hFile, fmt=1001, advance="NO")arrVars(i)
	enddo

	write(hFile, fmt=1002, advance="YES")

	1001 format(2x,f10.3)
	1002 format(1x)
end subroutine
                                                                                                                                                                                                          
                                                                                                                                                                                           
end module