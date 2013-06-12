module sgafit
use condorio
implicit none

integer, parameter :: hGAOut	= 1000
character(*), parameter :: strGaOut	= 'ga.out'

integer :: hChromCache

contains

subroutine ParallelFitEval(sgaParam)                                                                                                                                                                              
use sga
use fmt_helper
use chmcache
use umacase
use condor_helper
USE DFPORT
implicit none                                                                                                                                                                                             
	!argument
	type(SGAPARAMSTRUCT), intent(inout) :: sgaParam

	!variable
	integer :: i, k, id
	integer :: arrJobIds( sgaParam%nPopSize ), arrCachedIds( sgaParam%nPopSize )
	integer :: nJobCount, nCachedCount
	logical :: bCachePde = .false.

	double precision :: rFitness, rTotalCost, rPenTnt, rPenRdx, rMaxTntC, rMaxRdxC, rDummyVCFCost, MB1, MB2
	integer :: iDryCellC, iDryCellH, nRemYear
	double precision :: rFourMaxRdxC, rFourMaxTntC

	if( sgaParam%iCurGen==1 )then                                                                                                                                                                     
		i = RemoveFileC( strGaOut )
		call MakeStaticFolder
	endif

	open(unit=hGaOut, file=strGaOut, status='unknown', position='append' )
	write( hGaOut, 1111 ) sgaParam%iCurGen                                                                                                                                                            
		                                                                                                                                                                                          
	nJobCount = 0
	nCachedCount = 0
	do i = 1, sgaParam%nPopSize
		call DecodeOne( sgaParam, i )
		call PrepareFitness( i, sgaParam, MB1, MB2 )

		if( SearchChromCache( hChromCache, sgaParam%arrPop(i,:), sgaParam%nChromBits, sgaParam%arrFit(i), 1 )==0 )then
			arrJobIds( nJobCount+1 ) = i
			nJobCount = nJobCount + 1
			call BroadcastSlave(i, sgaParam%iCurGen, g_nGridFlagX, g_nGridFlagY )

			call InsertChromCache( hChromCache, sgaParam%arrPop(i,:), sgaParam%nChromBits, sgaParam%arrFit(i), 1 )
		else
			arrCachedIds( nCachedCount+1 )=i
			nCachedCount = nCachedCount + 1
		endif
	end do

	!route the jobs
	if( nJobCount>0 )then 
		call WaitForSlaves( arrJobIds, nJobCount )
	endif

	bCachePde = .false.
	do i = 1, nJobCount
		id = arrJobIds(i)
		call DecodeOne( sgaParam, id )
		call PrepareFitness( id, sgaParam, MB1, MB2 )

		call CollectSlave( id, sgaParam%iCurGen, rFitness, rTotalCost, rPenRdx, rPenTnt, rMaxRdxC, rMaxTntC, rDummyVCFCost, nRemYear, rFourMaxRdxC, rFourMaxTntC, iDryCellC, iDryCellH )
		sgaParam%arrFit(id) = rFitness + MB1 + MB2
		sgaParam%arrFlags(id) = PDE_EVAL

		call ReplaceChromCache( hChromCache, sgaParam%arrPop(id,:), sgaParam%nChromBits, sgaParam%arrFit(id), 1 )
		call SaveIndInfo(sgaParam, id, bCachePde, rTotalCost, rPenRdx, rPenTnt, rMaxRdxC, rMaxTntC, rDummyVCFCost)
	end do

	bCachePde = .true.
	do i=1, nCachedCount
		id = arrCachedIds(i)
		call DecodeOne( sgaParam, id )
		call PrepareFitness( id, sgaParam, MB1, MB2 )

		if( SearchChromCache( hChromCache, sgaParam%arrPop(id,:), sgaParam%nChromBits, sgaParam%arrFit(id), 1 )==0 )then
			print*, 'error caching'
			stop
		endif
		sgaParam%arrFlags(id) = CACHE_EVAL

		call SaveIndInfo(sgaParam, id, bCachePde, rTotalCost, rPenRdx, rPenTnt, rMaxRdxC, rMaxTntC, rDummyVCFCost)
	enddo
	close( hGaOut )                                                                                                                                                                                   

	call RefreshHosts

	1111 format(/'#################  Generation',i5,'  #################')
end subroutine ParallelFitEval                                                                                                                                                                                    

subroutine SaveIndInfo(sgaParam, id, bCachePde, rTotalCost, rPenRdx, rPenTnt, rMaxRdxC, rMaxTntC, rDummyVCFCost)
use sga                                                                                                                                                                                           
use umacase                                                                                                                                                                                      
implicit none
	!arguments
	type(SGAPARAMSTRUCT), intent(inout) :: sgaParam
	logical, intent(in) :: bCachePde
	integer, intent(in) :: id
	double precision, intent(in) :: rTotalCost, rPenRdx, rPenTnt, rMaxRdxC, rMaxTntC, rDummyVCFCost

	!variables
	integer :: j
                                                                                                                                                                                                      
	if( bCachePde )then
		write( hGaOut, fmt=1075, advance="NO" ) id, sgaParam%arrFit(id), rTotalCost, rPenRdx, rPenTnt, rMaxRdxC, rMaxTntC, rDummyVCFCost
	else
		write( hGaOut, fmt=1076, advance="NO" ) id, sgaParam%arrFit(id), rTotalCost, rPenRdx, rPenTnt, rMaxRdxC, rMaxTntC, rDummyVCFCost
	endif
	!well locations
	do j=1, g_nNewWells
		write( hGaOut, fmt=1078, advance="NO" )nint(sgaParam%arrVars(id, j))
	enddo
	!well pumping rates                                                                                                                                                                       
	do j=j, g_nNewWells+g_nNewExtWells
		write( hGaOut, fmt=1077, advance="NO" )sgaParam%arrVars(id, j)
	enddo
	!new well flags
	do j=j, 2*g_nNewWells+g_nNewExtWells
		write( hGaOut, fmt=1078, advance="NO" )nint(sgaParam%arrVars(id, j))
	enddo

	!old well pumping rates
	do j=j, 2*g_nNewWells+g_nNewExtWells+g_nOldExtWells
		write( hGaOut, fmt=1077, advance="NO" )sgaParam%arrVars(id, j)
	enddo
	!old well pumping flags
	do j=j, 2*g_nNewWells+g_nNewExtWells+g_nOldExtWells+g_nOldWells
		write( hGaOut, fmt=1078, advance="NO" )nint(sgaParam%arrVars(id, j))
	enddo
	!just print the catridge                                                                                                                                                                  
	write(hGaOut, fmt=1081, advance="YES")                                                                                                                                                    
                                                                                                                                                                                                      
	1075 format('*',i4,2x,2(f17.6,2x),5(f20.10,2x))                                                                                                                                           
	1076 format(i4,2x,2(f17.6,2x),5(f20.10,2x))                                                                                                                                               
	1077 format(2x,f7.4)
	1078 format(2x,i2)
	1079 format(2x,f2.0)
	1081 format(1x)
end subroutine SaveIndInfo                                                                                                                                                                        
                                                                                                                                                                                                      

end module sgafit