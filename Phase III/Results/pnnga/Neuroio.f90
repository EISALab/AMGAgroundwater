module NEUROIO
use CASEWELL
implicit none

character(*), parameter :: strHeadTrain = 'headtrain.dat'
character(*), parameter :: strHeadTest = 'headtest.dat'
character(*), parameter :: strHeadPred = 'headpred.dat'

character(*), parameter :: strRiskTrain = 'risktrain.dat'
character(*), parameter :: strRiskTest = 'risktest.dat'
character(*), parameter :: strRiskPred = 'riskpred.dat'

!character(*), parameter :: strHeadM = 'trheadnet'
!character(*), parameter :: strRiskM = 'trrisknet'
character(*), parameter :: strHeadM = 'univnet'
character(*), parameter :: strRiskM = 'univnet'

character(*), parameter :: strHeadNN = 'headnneu.dat'
character(*), parameter :: strRiskNN = 'risknneu.dat'

character(*), parameter :: strHeadScale = 'headscale.dat'
character(*), parameter :: strRiskScale = 'riskscale.dat'

character(*), parameter :: strDoScaleBat = 'scale.bat'
character(*), parameter :: strDoTrainBat = 'train.bat'

character(*), parameter :: strHeadPredOut = 'headpred.out'
character(*), parameter :: strRiskPredOut = 'riskpred.out'

character(*), parameter :: strDoPredHead = 'predhead.bat'
character(*), parameter :: strDoPredRisk = 'predrisk.bat'

type NETLAYERPARAM
	integer :: nNeuros
	double precision :: rLearnCoef, rMomenCoef
end type

type NEURALNETPARAM
	integer :: nLayers
	integer :: pLayerParam
!	type(NETLAYERPARAM), dimension(:), pointer :: pLayerParam
	integer funNeuro
	integer funDNeuro
end type

type NORMALIZEPARAM
	integer :: prOrigMin, prOrigMax
	integer :: prNormMin, prNormMax
	integer:: nLen
end type

contains

integer function GetHeadNNInpCount
implicit none
	!2*coordinate + pumping rates
	GetHeadNNInpCount = 2*g_nRemWells + g_nRemWells
end function GetHeadNNInpCount

integer function GetHeadNNOutCount
implicit none
	!the hydraulic heads
	GetHeadNNOutCount = g_nRemWells
end function GetHeadNNOutCount

integer function GetRiskNNInpCount
implicit none
	GetRiskNNInpCount = 2*g_nRemWells + g_nRemWells + g_nRemWells
end function GetRiskNNInpCount

integer function GetRiskNNOutCount
implicit none
	GetRiskNNOutCount = 2
end function GetRiskNNOutCount

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
	call assert( nCounts >= GetHeadNNInpCount()+GetHeadNNOutCount() )

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

subroutine WriteForHeadNN(strFile, arrWellIds, arrPumpRates, arrHeads)
implicit none
	character(*), intent(in) :: strFile
	integer, dimension(:), intent(in) :: arrWellIds
	double precision, dimension(:), intent(in) :: arrPumpRates
	double precision, dimension(:), optional, intent(in) :: arrHeads

	!variable
	integer, parameter :: hFile = 89
	integer :: i

	if( present(arrHeads) )then
		open( unit=hFile, file=strFile, status='unknown', position='append' )
	else
		call OpenOutputFile( hFile, strFile )
	endif

	do i=1, g_nRemWells
		write( hFile, fmt=1000, advance="NO" )g_parrWellInfos(i)%parrLocs(arrWellIds(i))%x, g_parrWellInfos(i)%parrLocs(arrWellIds(i))%y
	enddo
	do i=1, g_nRemWells
		write( hFile, fmt=1001, advance="NO")arrPumpRates(i)
	enddo
	if( present(arrHeads) )then
		do i=1, g_nRemWells
			write( hFile, fmt=1001, advance="NO")arrHeads(i)
		enddo
	endif
	write(hFile, fmt=1002, advance="YES")

	close(hFile)
	
	1000 format(i4,2x,i4,2x)
	1001 format(2x,f10.3)
	1002 format(1x)
end subroutine WriteForHeadNN

subroutine WriteForRiskNN(strFile, arrWellIds, arrPumpRates, arrHeads, rRiskB, rRiskW)
implicit none
	character(*), intent(in) :: strFile
	integer, dimension(:), intent(in) :: arrWellIds
	double precision, dimension(:), intent(in) :: arrPumpRates, arrHeads
	double precision, optional, intent(in) :: rRiskB, rRiskW

	!variable
	integer, parameter :: hFile = 89
	integer :: i
	double precision :: rB, rW

	if( present(rRiskB) )then
		open( unit=hFile, file=strFile, status='unknown', position='append' )
	else
		call OpenOutputFile( hFile, strFile )
	endif

	print*, 'writting well locs'
	do i=1, g_nRemWells
		write( hFile, fmt=1000, advance="NO" )g_parrWellInfos(i)%parrLocs(arrWellIds(i))%x, g_parrWellInfos(i)%parrLocs(arrWellIds(i))%y
	enddo

	print*, 'writting pumping rates'
	do i=1, g_nRemWells
		write( hFile, fmt=1001, advance="NO")arrPumpRates(i)
	enddo

	print*, 'writting heads'
	do i=1, g_nRemWells
		write( hFile, fmt=1001, advance="NO")arrHeads(i)
	enddo

	if( present(rRiskB ) ) then
		rB = rRiskB
		rW = rRiskW
		if( rRiskB <=1e-12 )rB = 1e-12
		if( rRiskW <=1e-8 )rW = 1e-8
		write(hFile, fmt=1002, advance="YES")-log10(rB), -log10(rW)
	else
		write(hFile, fmt=1003, advance="YES")
	endif

	close(hFile)
	
	1000 format(i4,2x,i4,2x)
	1001 format(2x,f10.3)
	1002 format(2x, f20.12, 2x, f20.12)
	1003 format(1x)
end subroutine WriteForRiskNN

subroutine WriteHeadScaleParam( strFile, arrMin, arrMax )
implicit none
	character(*), intent(in) :: strFile
	double precision, dimension(:) :: arrMin, arrMax
	!variable
	integer, parameter :: hFile = 89
	integer :: i

	call assert( ubound(arrMin,1)>=GetHeadNNInpCount()+GetHeadNNOutCount() )

	call OpenOutputFile( hFile, strFile )
	!default
	write( hFile, fmt=1000 )-0.9, 0.9
	!for all inputs
	do i=1, GetHeadNNInpCount() 
		write( hFile, fmt=1001 )i, -0.9, 0.9
	end do
	!for outputs
	do i=i, GetHeadNNInpCount()+GetHeadNNOutCount()
		write( hFile, fmt=1001 )i, 0.05, 0.9
	enddo
	!for all the in_column min and max
	do i=1, GetHeadNNInpCount()+GetHeadNNOutCount()
		write( hFile, fmt=1002 )i, arrMin(i), arrMax(i)
	end do

	1000 format( 'default=', f6.2, 1x, f6.2 )
	1001 format( 'column', i2, '=', f6.2, 1x, f6.2 )
	1002 format( 'in_column', i2, '=', f10.4, 2x, f10.4 ) 
end subroutine WriteHeadScaleParam

subroutine WriteRiskScaleParam( strFile, arrMin, arrMax )
implicit none
	character(*), intent(in) :: strFile
	double precision, dimension(:) :: arrMin, arrMax
	!variable
	integer, parameter :: hFile = 89
	integer :: i

	call assert( ubound(arrMin,1)>=GetRiskNNInpCount()+GetRiskNNOutCount() )

	call OpenOutputFile( hFile, strFile )
	!default
	write( hFile, fmt=1000 )-0.9, 0.9
	!all inputs
	do i=1, GetRiskNNInpCount() 
		write( hFile, fmt=1001 )i, -0.9, 0.9
	end do
	!all outputs
	do i=i, GetRiskNNInpCount()+GetRiskNNOutCount()
		write( hFile, fmt=1001 )i, 0.05, 0.9
	enddo
	!for all the in_column
	do i=1, GetRiskNNInpCount()+GetRiskNNOutCount()
		write( hFile, fmt=1002 )i, arrMin(i), arrMax(i)
	end do

	1000 format( 'default=', f6.2, 1x, f6.2 )
	1001 format( 'column', i2, '=', f6.2, 1x, f6.2 )
	1002 format( 'in_column', i2, '=', f10.4, 2x, f10.4 ) 
end subroutine WriteRiskScaleParam

subroutine ReadHeadNNPred( arrMin, arrMax, nRemWells, arrHeads )
implicit none
	integer, intent(in) :: nRemWells
	double precision, dimension(:), intent(in) :: arrMin, arrMax
	double precision, dimension(:), intent(out) :: arrHeads

	!arguments
	integer :: i
	integer, parameter :: hFile = 99

	call OpenInputFile( hFile, strHeadPredOut )
	read(hFile, *)(arrHeads(i), i=1,nRemWells)
	close(hFile)

	do i=1, nRemWells
		arrHeads(i) = LinearScale( arrHeads(i), dble(0.05), dble(0.9), arrMin(3*nRemWells+i), arrMax(3*nRemWells+i) )
	enddo

end subroutine ReadHeadNNPred

subroutine ReadRiskNNPred( arrMin, arrMax, nRemWells, rRiskB, rRiskW )
implicit none
	integer, intent(in) :: nRemWells
	double precision, dimension(:), intent(in) :: arrMin, arrMax
	double precision, intent(out) :: rRiskB, rRiskW

	!arguments
	integer, parameter :: hFile = 99

	call OpenInputFile( hFile, strRiskPredOut )
	read(hFile, *)rRiskB, rRiskW
	close(hFile)

	rRiskB = LinearScale( rRiskB, dble(0.05), dble(0.9), arrMin(4*nRemWells+1), arrMax(4*nRemWells+1) )
	rRiskW = LinearScale( rRiskW, dble(0.05), dble(0.9), arrMin(4*nRemWells+2), arrMax(4*nRemWells+2) )

	rRiskB = 10**(-rRiskB)
	rRiskW = 10**(-rRiskW)

end subroutine ReadRiskNNPred

double precision function LinearScale( var, rMin, rMax, rOutMin, rOutMax )
implicit none
	double precision, intent(in) :: var, rMin, rMax, rOutMin, rOutMax

	LinearScale = (var-rMin)/(rMax-rMin) * (rOutMax-rOutMin) + rOutMin
end function LinearScale

end module NEUROIO