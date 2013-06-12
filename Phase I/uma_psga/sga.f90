module SGA
use STD_HELPER
use condorio
!comments. this code is similar to Felipe's code. But the data structure is redefined to make it more straightforward.
!the following table are the corresponding variable names of the two version
!sgaParam%arrVarMin		<=>		g0
!sgaParam%arrVarStride	<=>		g1
double precisIon, parameter :: EPS		= 1E-10

integer, parameter :: WELL_INJECTION	= 1
integer, parameter :: WELL_EXTRACTION	= 2
integer, parameter :: WELL_DONTCARE		= 3

integer, parameter :: SELECT_TOURNAMENT = 1
integer, parameter :: TOURNAMENT_SIZE	= 3

integer, parameter :: CROSS_RANDPOINT	= 0
integer, parameter :: CROSS_UNIFORM		= 1

integer, parameter :: MUTATE_CREEP		= 1

integer, parameter :: NN_EVAL			= 1
integer, parameter :: PDE_EVAL			= 2
integer, parameter :: CACHE_EVAL		= 3

double precision, parameter :: TOL_MINVAL		= 1.d-5
double precision, parameter :: TOL_RATIO		= 1.d-5
double precision, parameter :: CONVERGE_RATIO	= 0.9

character(*), parameter :: strRestartFile = 'ga.restart'
integer, parameter :: hRestartFile = 1900
character(*), parameter :: strGaGen = 'ga.gen'
integer, parameter :: hGaGen = 1901

type SGAPARAMSTRUCT
	integer, allocatable, dimension(:,:) :: arrPopInj			!the best chrosomes evaluated by PDE so far
	double precision, allocatable, dimension(:) :: arrFitInj	!the best fitness evaluated by PDE so far

	integer, allocatable, dimension(:,:) :: arrPop				!the current chrosome pool arrPop(nPopSize, nChromBits)
	double precision, allocatable, dimension(:) :: arrFit		!the fitness value Fitness(nPopSize)
	integer, allocatable, dimension(:) :: arrFlags				!flag=0, this is evaluated by NN, flag=1, this is evaluated by PDE, flag=2, this is from chromose cache.

	double precision, allocatable, dimension(:,:) :: arrVars	!the decision variables arrDecVar(nPopSize, nVarCount)
	integer, allocatable, dimension(:) :: arrVarIndex			!the beginning index in the chromsome of each decision variavbels arrVarIndex(nVarCount)

	!the next three array defines the decretization parameterions for coding the chrosome.
	!for each decision variables, first convert it's chromosome to a number, then the decision varable is,
	!varMin + varStride * number. 
	!The value might be bigger than varMax. The subroutine CheckChrom will trancate chromosome to its maximum value 
	double precision, allocatable, dimension(:) :: arrVarMin	!the minimum value of each decision variable arrVarMin(nVarCount)
	double precision, allocatable, dimension(:) :: arrVarMax	!the maximum value of each decision variable
	double precision, allocatable, dimension(:) :: arrVarStride !the coding stride of each binary one

	integer :: nVarCount

	integer :: nChromBits							!chromosome bits number
	integer :: nPopSize								!population size
	integer :: nMaxGens								!maxima generation
	integer :: iStartGen, iCurGen					!starting generation and current generation

	logical :: bNicheFlag							!if do niching.
	logical :: bInjecFlag							!if do island injection
	logical :: bEliteFlag
	logical :: bMicroFlag

	integer :: nSelectOpt							!0 : normal selection, 1 : tournment selection
	integer :: nCrossOpt
	integer :: nMutateOpt

	double precision :: rCrossProb, rMutateProb

	logical :: bRestarted							!flag of restarting .false.--no, .true. restarted
														
!	logical :: iCrossIndex							!0 : do uniform single point, 1 : 

end type SGAPARAMSTRUCT

contains

subroutine Decode( sgaParam )
implicit none
	!argument
	type(SGAPARAMSTRUCT), intent(inout) :: sgaParam

	!variable
	integer :: i

	do i=1, sgaParam%nPopSize
		call DecodeOne( sgaParam, i )
	enddo
end subroutine Decode

subroutine DecodeOne( sgaParam, id )
	!argument
	type(SGAPARAMSTRUCT), intent(inout) :: sgaParam
	integer :: id		!the individual id

	!variable
	integer :: i, j, nBits, index
	double precision :: rVal

	do i=1, sgaParam%nVarCount
		if( i < sgaParam%nVarCount )then
			nBits = sgaParam%arrVarIndex(i+1) - sgaParam%arrVarIndex(i)
		else
			nBits = sgaParam%nChromBits - sgaParam%arrVarIndex(i) + 1
		endif
		
		index = sgaParam%arrVarIndex(i)
		rVal = 0
		do j = 1, nBits
			rVal = rVal + sgaParam%arrPop( id, index + j -1 ) * 2**(nBits-j)
		enddo
		rVal = sgaParam%arrVarMin(i) + sgaParam%arrVarStride(i) * rVal
		sgaParam%arrVars(id, i) = rVal
	enddo
end subroutine DecodeOne

subroutine DecodeOneEx( sgaParam, arrChrom, arrVars )
implicit none
	!argument
	type(SGAPARAMSTRUCT), intent(inout) :: sgaParam
	integer, dimension(*) :: arrChrom
	double precision, dimension(*), intent(out) :: arrVars

	!variable
	integer :: i, j, nBits, index
	double precision :: rVal

	do i=1, sgaParam%nVarCount
		if( i < sgaParam%nVarCount )then
			nBits = sgaParam%arrVarIndex(i+1) - sgaParam%arrVarIndex(i)
		else
			nBits = sgaParam%nChromBits - sgaParam%arrVarIndex(i) + 1
		endif
		
		index = sgaParam%arrVarIndex(i)
		rVal = 0
		do j = 1, nBits
			rVal = rVal + arrChrom( index + j -1 ) * 2**(nBits-j)
		enddo
		rVal = sgaParam%arrVarMin(i) + sgaParam%arrVarStride(i) * rVal
		arrVars(i) = rVal
	enddo
end subroutine DecodeOneEx

subroutine Encode( sgaParam )
implicit none
	!argument
	type(SGAPARAMSTRUCT), intent(inout) :: sgaParam

	!variable
	integer :: i

	do i=1, sgaParam%nPopSize
		call EncodeOne( sgaParam, i )
	enddo
end subroutine Encode

subroutine EncodeOne( sgaParam, id )
	!argument
	type(SGAPARAMSTRUCT), intent(inout) :: sgaParam
	integer :: id		!the individual id

	!variable
	integer :: i, j, nBits, index, nVal

	do i=1, sgaParam%nVarCount
		if( i < sgaParam%nVarCount )then
			nBits = sgaParam%arrVarIndex(i+1) - sgaParam%arrVarIndex(i)
		else
			nBits = sgaParam%nChromBits - - sgaParam%arrVarIndex(i) + 1
		endif
		
		index = sgaParam%arrVarIndex(i)
		nVal = nint( (sgaParam%arrVars(id, i) - sgaParam%arrVarMin(i)) / sgaParam%arrVarStride(i) )
		do j = 1, nBits
			if( nVal - 2**(nBits-j) >= 0 )then
				sgaParam%arrPop( id, index + j -1 ) = 1
				nVal = nVal - 2**(nBits-j)
			else
				sgaParam%arrPop( id, index + j -1 ) = 0
			endif
		enddo
		call assert( nVal==0 )
	enddo
end subroutine EncodeOne

	
subroutine CheckChrom( sgaParam )
implicit none
	!argument
	type(SGAPARAMSTRUCT), intent(in) :: sgaParam

	!variable
	integer :: i

	do i=1, sgaParam%nPopSize
		call CheckIndiv( sgaParam, i )
	enddo

contains 
	subroutine CheckIndiv( sgaParam, id )
		!argument
		type(SGAPARAMSTRUCT), intent(in) :: sgaParam
		integer :: id		!the individual id

		!variable
		integer :: i, j, nBits, index
		double precision :: rVal

		do i=1, sgaParam%nVarCount
			if( i < sgaParam%nVarCount )then
				nBits = sgaParam%arrVarIndex(i+1) - sgaParam%arrVarIndex(i)
			else
				nBits = sgaParam%nChromBits - - sgaParam%arrVarIndex(i) + 1
			endif
			
			index = sgaParam%arrVarIndex(i)
			rVal = 0
			do j = 1, nBits
				rVal = rVal + sgaParam%arrPop( id, index + j -1 ) * 2**(nBits-j)
			enddo
			rVal = sgaParam%arrVarMin(i) + sgaParam%arrVarStride(i) * rVal

			call assert( rVal <= sgaParam%arrVarMax(i)+EPS )
		enddo
	end subroutine CheckIndiv
end subroutine CheckChrom

subroutine InitPopulation( sgaParam )
implicit none
	!arguments
	type(SGAPARAMSTRUCT), intent(inout) :: sgaParam

	integer i, j

	do i=1, sgaParam%nPopSize
		do j=1, sgaParam%nChromBits
			if( UniRand()<0.5 )then
				sgaParam%arrPop(i,j) = 0
			else
				sgaParam%arrPop(i,j) = 1
			endif
		enddo
	enddo
end subroutine InitPopulation

subroutine DoSGA(sgaParam, FitEval)
implicit none
	!arguments
	type(SGAPARAMSTRUCT), intent(inout) :: sgaParam
	external FitEval

	!variables
	integer :: i, iStart, iCur, iEnd, nPopSize
	integer, dimension( sgaParam%nPopSize, sgaParam%nChromBits ) :: arrChild
	logical :: bRestarted

	sgaParam%iCurGen = sgaParam%iStartGen

	inquire( FILE=strRestartFile, EXIST=bRestarted )
	if( bRestarted )then
		call DoRestart( sgaParam )
	else
		call InitPopulation( sgaParam )
	endif

	nPopSize = sgaParam%nPopSize
	iStart = sgaParam%iCurGen
	iEnd = sgaParam%iStartGen + sgaParam%nMaxGens - 1

	!Main processing loop for the simple genetic algorithm
	do iCur = iStart, iEnd

		sgaParam%iCurGen = iCur
		call FitEval( sgaParam )

		call assert( .not. sgaParam%bInjecFlag )		!don't support island injection now
		if( sgaParam%bInjecFlag )then
			!island injection code here
		endif


		call assert( iCur<=iStart + sgaParam%nMaxGens - 1 )
		if( sgaParam%bNicheFlag )call Niche

		!Perform selection & then perform crossover between the randomly selected pair.
		do i = 1, nPopSize, 2
			!selection may shuffle parent array, each call generate two children
			call Selection( sgaParam%nSelectOpt, sgaParam%arrPop, arrChild, sgaParam%arrFit, &
						&	sgaParam%nPopSize, sgaParam%nChromBits, i )
			call Crossover( sgaParam%nCrossOpt, arrChild, sgaParam%nPopSize, sgaParam%nChromBits, i, i+1, sgaParam%rCrossProb )
		enddo 

		call assert( .not. sgaParam%bMicroFlag )
		!Now perform random mutations. If running micro-GA, skip mutation.
		if( .not. sgaParam%bMicroFlag )call Mutate( sgaParam%nMutateOpt, arrChild, sgaParam%nPopSize, sgaParam%nChromBits, sgaParam%rMutateProb )

		!Write child array back into parent array for new generation.  
		!Check to see if the best parent was replicated.
		call NewGen( sgaParam%bEliteFlag, sgaParam%arrPop, arrChild, sgaParam%arrFit, sgaParam%nPopSize, sgaParam%nChromBits )

		if( sgaParam%bMicroFlag )call Micro

		!Write to restart file.
		call SaveRestart(sgaParam)
	enddo

	i=RemoveFileC( strRestartFile )
end subroutine DoSGA

!DoSGAEx implents u&l stragegy
subroutine DoSGAEx(sgaParam, FitEval)
implicit none
	!arguments
	type(SGAPARAMSTRUCT), intent(inout) :: sgaParam
	external FitEval

	!variables
	integer :: i, iStart, iCur, iEnd, nPopSize
	integer, dimension( sgaParam%nPopSize, sgaParam%nChromBits ) :: arrChild
	integer, dimension( sgaParam%nPopSize*2, sgaParam%nChromBits ) :: arrChromPool
	double precision, dimension( sgaParam%nPopSize*2 ) :: arrFitPool
	integer, dimension( sgaParam%nPopSize*2 ) :: arrFlagsPool
	integer, dimension( sgaParam%nPopSize*2 ) :: arrIndex
	logical :: bRestarted, bConverged

	sgaParam%iCurGen = sgaParam%iStartGen
	sgaParam%bEliteFlag = .false.		!u+l is already doing elite

	inquire( FILE=strRestartFile, EXIST=bRestarted )
	if( bRestarted )then
		call DoRestart( sgaParam )
	else
		call InitPopulation( sgaParam )
	endif

	nPopSize = sgaParam%nPopSize
	iStart = sgaParam%iCurGen
	iEnd = sgaParam%iStartGen + sgaParam%nMaxGens - 1

	if( sgaParam%iCurGen==1 )then
		i=RemoveFileC( strGaGen )
	endif
	open(unit=hGaGen, file=strGaGen, status='unknown', position='append' )

	!assign a very big value to arrFitPool(nPopSize,2*nPopSize) so that it won't be choosed at first generation.
	arrFitPool = 1d100
	arrFlagsPool = NN_EVAL
	bConverged = .false.

	!Main processing loop for the simple genetic algorithm
	do iCur = iStart, iEnd

		sgaParam%iCurGen = iCur
		call FitEval( sgaParam )

		call assert( .not. sgaParam%bInjecFlag )		!don't support island injection now
		if( sgaParam%bInjecFlag )then
			!island injection code here
		endif

		!put the current generation into the pool to sort it with it last generation
		do i=1, sgaParam%nPopSize
			arrFitPool(sgaParam%nPopSize+i) = sgaParam%arrFit(i)
			arrFlagsPool(sgaParam%nPopSize+i) = sgaParam%arrFlags(i)
			arrChromPool(sgaParam%nPopSize+i, :) = sgaParam%arrPop(i,:)
		enddo
			
!		arrFitPool(sgaParam%nPopSize:sgaParam%nPopSize*2) = sgaParam%arrFit(1:sgaParam%nPopSize)
!		arrChromPool(sgaParam%nPopSize:sgaParam%nPopSize*2, :) = sgaParam%arrPop
		arrIndex = (/(i, i=1,sgaParam%nPopSize*2)/)

		call qsort( arrIndex, 2*sgaParam%nPopSize, sizeof(arrIndex(1)), FitCompareEx )

		!for debug
		if( iCur==1 )arrIndex(1:sgaParam%nPopSize) = (/(i, i=1+sgaParam%nPopSize, sgaParam%nPopSize*2)/)

		!this is the real parent, the first nPopSize individuals
		do i = 1, sgaParam%nPopSize
			sgaParam%arrPop(i, :) = arrChromPool( arrIndex(i), : )
			sgaParam%arrFit(i) = arrFitPool( arrIndex(i) )
			sgaParam%arrFlags(i) = arrFlagsPool( arrIndex(i) )
		enddo

		call SaveInd( hGaGen, sgaParam )

		!Write to restart file.
		call SaveRestart( sgaParam )

		if( IsConverged( sgaParam ) )then
			bConverged = .true.
			exit
		endif

		!since the fitness value is already there, now prepare the dummy children
		call assert( iCur<=iStart + sgaParam%nMaxGens - 1 )
		if( sgaParam%bNicheFlag )call Niche

		!Perform selection & then perform crossover between the randomly selected pair.
		do i = 1, nPopSize, 2
			!selection may shuffle parent array, each call generate two children
			call Selection( sgaParam%nSelectOpt, sgaParam%arrPop, arrChild, sgaParam%arrFit, &
						&	sgaParam%nPopSize, sgaParam%nChromBits, i )
			call Crossover( sgaParam%nCrossOpt, arrChild, sgaParam%nPopSize, sgaParam%nChromBits, i, i+1, sgaParam%rCrossProb )
		enddo 

		call assert( .not. sgaParam%bMicroFlag )
		!Now perform random mutations. If running micro-GA, skip mutation.
		if( .not. sgaParam%bMicroFlag )call Mutate( sgaParam%nMutateOpt, arrChild, sgaParam%nPopSize, sgaParam%nChromBits, sgaParam%rMutateProb )

		!OK, I have the dummy children, I first save the parent to the sorting pool,
		!and copy the dummp children to parent to prepare the fitness evaluation of the children
		arrFitPool(1:sgaParam%nPopSize) = sgaParam%arrFit
		arrFlagsPool(1:sgaParam%nPopSize) = sgaParam%arrFlags
		arrChromPool(1:sgaParam%nPopSize, :) = sgaParam%arrPop
		sgaParam%arrPop = arrChild
		
		!Write child array back into parent array for new generation.  
		!Check to see if the best parent was replicated.
		!call NewGen( sgaParam%bEliteFlag, sgaParam%arrPop, arrChild, sgaParam%arrFit, sgaParam%nPopSize, sgaParam%nChromBits )

		if( sgaParam%bMicroFlag )call Micro
	enddo

	if( bConverged )then
		sgaParam%iCurGen = iEnd
		call FitEval( sgaParam )
		call SaveInd( hGaGen, sgaParam )
	endif


	close( hGaGen )
!	i=RemoveFileC( strRestartFile )
contains
integer(2) function FitCompare(a1, a2)
	integer :: a1, a2

	double precision :: r1, r2
	r1 = arrFitPool(a1)
	r2 = arrFitPool(a2)

	if( (r1 - r2) == 0 )then
		FitCompare = 0
	else if( r1>r2 )then
		FitCompare = 1
	else
		FitCompare = -1
	endif
end function FitCompare

integer(2) function FitCompareEx(a1, a2)
	integer :: a1, a2
	double precision :: rPenalty = 300.0

	double precision :: r1, r2
	r1 = arrFitPool(a1)
	r2 = arrFitPool(a2)
	if( arrFlagsPool(a1)/=arrFlagsPool(a2) )then
		if( arrFlagsPool(a1)==NN_EVAL )then
			rPenalty = min(100.0, 0.02*r1 )
			r1 = r1 + rPenalty
		endif
		if( arrFlagsPool(a2)==NN_EVAL )then
			rPenalty = min(100.0, 0.02*r2 )
			r2 = r2 + rPenalty
		endif
	endif

	if( (r1 - r2) == 0 )then
		FitCompareEx = 0
	else if( r1>r2 )then
		FitCompareEx = 1
	else
		FitCompareEx = -1
	endif
end function FitCompareEx


end subroutine DoSGAEx


subroutine Niche
implicit none
	call assert( .false. )	!do not support niche now
end subroutine Niche

subroutine Micro
implicit none
	call assert( .false. )
end subroutine Micro

subroutine TournSelect( arrParent, arrChild, arrFits, nPopSize, nChromBits, iChildIndex )
implicit none
	!argument
	integer, intent(in) :: nPopSize, nChromBits, iChildIndex
	integer, dimension( nPopSize, nChromBits ), intent(inout) :: arrParent, arrChild
	double precision, dimension( nPopSize ), intent(inout) :: arrFits
	
	!variable
	integer :: i, iPick, iMate

	!if the picking number is at the beginning of population, shuffle the population
	iPick = mod( 2*(iChildIndex-1), nPopSize ) + 1
	if( iPick==1 )call Shuffle( arrParent, arrFits, nPopSize, nChromBits )

	call assert( (iPick>=1) .and. (iPick<=nPopSize-1) )
	call assert( mod(nPopSize, 2)==0 )

	if( arrFits(iPick) .lt. arrFits(iPick+1) )then
		iMate = iPick
	else
		iMate = iPick+1
	endif

	arrChild( iChildIndex, : ) = arrParent( iMate, : )

contains
	!this is the rewritten shuffle function for a single objective function
	!the function shuffle both chrosome bits and the fitness array
	subroutine Shuffle( arrPop, arrFit, nPopSize, nChromBits )
	implicit none
		!argument
		integer :: nPopSize, nChromBits
		integer, dimension( nPopSize, nChromBits ), intent(inout) :: arrPop
		double precision, dimension( nPopSize ), intent(inout) :: arrFit

		!variables
		integer :: i, j, index
		
		do i = 1, nPopSize - 1
			index = i + 1 + dint( dble(nPopSize-i) * UniRand() )	!find a random swap position
			call assert( (index>i) .and. (index<=nPopSize) )

			!swap chrosome bits
			do j=1, nChromBits
				call SwapInt( arrPop(i, j), arrPop(index, j) )
			end do
			!swap fitness
			call SwapDouble( arrFit(i), arrFit(index) )
		end do
	end subroutine Shuffle
end subroutine TournSelect

subroutine TournSelectEx( arrParent, arrChild, arrFits, nPopSize, nChromBits, iChildIndex )
implicit none
	!argument
	integer, intent(in) :: nPopSize, nChromBits, iChildIndex
	integer, dimension( nPopSize, nChromBits ), intent(inout) :: arrParent, arrChild
	double precision, dimension( nPopSize ), intent(inout) :: arrFits
	
	!variable
	integer :: iPick, iMate, i
!	integer :: iPick, iMate1, iMate2

	iPick = 1+dint(dble(nPopSize-1)*UniRand())	!this is not correct, now for debug only
	do i=2, TOURNAMENT_SIZE
		iMate = 1+dint(dble(nPopSize-1)*UniRand())
		if( arrFits(iMate)<arrFits(iPick) )then
			iPick = iMate
		endif
	enddo
!	iMate1 = 1+dint(dble(nPopSize-1)*UniRand())	!this is not correct, now for debug only
!	iMate2 = 1+dint(dble(nPopSize-1)*UniRand())

!	if( arrFits(iMate1) < arrFits(iMate2) )then
!		iPick = iMate1
!	else
!		iPick = iMate2
!	endif

	arrChild( iChildIndex, : ) = arrParent( iPick, : )
end subroutine TournSelectEx

!Subroutine for selection operator.
subroutine Selection( nOpt, arrParent, arrChild, arrFit, nPopSize, nChromBits, iChildIndex )
implicit none
	!argument
	integer, intent(in) :: nOpt			!the selection method option
	integer, intent(in) :: nPopSize, nChromBits
	integer, dimension( nPopSize, nChromBits ), intent(inout) :: arrParent, arrChild
	double precision, dimension( nPopSize ) :: arrFit
	integer :: iChildIndex

	!argument
	call assert( nOpt==SELECT_TOURNAMENT )

	if( nOpt == SELECT_TOURNAMENT )then
		call TournSelectEx( arrParent, arrChild, arrFit, nPopSize, nChromBits, iChildIndex )
		call TournSelectEx( arrParent, arrChild, arrFit, nPopSize, nChromBits, iChildIndex+1 )
	end if
end subroutine Selection


!Subroutine for selection operator.
subroutine Selection2( sgaParam, arrChild, iChildIndex )
implicit none
	!argument
	type(SGAPARAMSTRUCT), intent(inout) :: sgaParam
	integer, dimension( sgaParam%nPopSize, sgaParam%nChromBits ), intent(inout) :: arrChild
	integer, intent(in) :: iChildIndex

	!argument
	call assert( sgaParam%nSelectOpt==SELECT_TOURNAMENT )

	if( sgaParam%nSelectOpt == SELECT_TOURNAMENT )then
		call TournSelect( sgaParam%arrPop, arrChild, sgaParam%arrFit, sgaParam%nPopSize, sgaParam%nChromBits, iChildIndex )
		call TournSelect( sgaParam%arrPop, arrChild, sgaParam%arrFit, sgaParam%nPopSize, sgaParam%nChromBits, iChildIndex+1 )
	end if
end subroutine Selection2


!Single-point crossover at a random chromosome point.
subroutine RandPointCross( arrPop, nPopSize, nChromBits, index1, index2, rCrossProb )
implicit none
	!argument
	integer, intent(in) :: nPopSize, nChromBits
	integer, dimension( nPopSize, nChromBits ), intent(inout) :: arrPop
	integer, intent(in) :: index1, index2
	double precision :: rCrossProb

	!variable
	integer :: i, id

	if( UniRand() .le. rCrossProb ) then
		id = 2 + dint( (dble(nChromBits-1)*UniRand() )  ) !select a random point
		call assert( (id>=2) .and. (id<=nChromBits) )
		!swap the bits since the random point
		do i=id, nChromBits
			call SwapInt( arrPop(index1, i), arrPop(index2, i) )
		enddo
	endif
end subroutine RandPointCross

!Perform uniform crossover between the randomly selected pair.
subroutine UniformCross( arrPop, nPopSize, nChromBits, index1, index2, rCrossProb )
implicit none
	!argument
	integer, intent(in) :: nPopSize, nChromBits
	integer, dimension( nPopSize, nChromBits ), intent(inout) :: arrPop
	integer, intent(in) :: index1, index2
	double precision :: rCrossProb

	!variable
	integer :: i

	do i=1, nChromBits
		if( UniRand() .le. rCrossProb )then
			call SwapInt( arrPop(index1, i), arrPop(index2, i) )
		endif
	enddo
end subroutine UniformCross

subroutine Crossover( nOpt, arrPop, nPopSize, nChromBits, index1, index2, rCrossProb )
implicit none
	!argument
	integer, intent(in) :: nOpt			!the crossover method option
	integer, intent(in) :: nPopSize, nChromBits
	integer, dimension( nPopSize, nChromBits ), intent(inout) :: arrPop
	integer, intent(in) :: index1, index2
	double precision, intent(in) :: rCrossProb

	call assert( (nOpt==CROSS_UNIFORM) .or. (nOpt==CROSS_RANDPOINT) )

	if( nOpt==CROSS_RANDPOINT )then
		call RandPointCross( arrPop, nPopSize, nChromBits, index1, index2, rCrossProb )
	else if( nOpt==CROSS_UNIFORM )then
		call UniformCross( arrPop, nPopSize, nChromBits, index1, index2, rCrossProb )
	else
		call assert( .false. )
	end if

end subroutine Crossover


subroutine Mutate( nOpt, arrPop, nPopSize, nChromBits, rMutateProb )
implicit none
	!argument
	integer, intent(in) :: nOpt			!the crossover method option
	integer, intent(in) :: nPopSize, nChromBits
	integer, dimension( nPopSize, nChromBits ), intent(inout) :: arrPop
	double precision :: rMutateProb

	!variables
	integer :: i, j

	do i=1, nPopSize
		do j=1, nChromBits
			if( UniRand() .le. rMutateProb )then
				arrPop(i, j) = mod( arrPop(i,j)+1, 2 )	!flip a binary value
			endif
		enddo
	enddo

	call assert( nOpt/=MUTATE_CREEP )	!do not support creep mutate now
	if( nOpt==MUTATE_CREEP )then
		call assert( .false. )
	endif
		
end subroutine Mutate


!##################################################################################
!Write child array back into parent array for new generation. if bElite is .true.  and 
!the best parent is not there, the best paretent is always replicated into a random slot 
!in the new population
subroutine NewGen( bEliteFlag, arrParent, arrChild, arrFit, nPopSize, nChromBits )
implicit none
	logical :: bEliteFlag										!if do elite 
	integer, intent(in) :: nPopSize, nChromBits
	integer, dimension( nPopSize, nChromBits ), intent(inout) :: arrParent, arrChild
	double precision, dimension( nPopSize ), intent(in) :: arrFit

	!variable
	integer :: i, j, id, kElite, bestIndex
	integer, dimension( nChromBits ) :: bestParent
	logical :: bFoundElite

	!this is the simplest method, just copy children to parenent array
	if( .not. bEliteFlag )then
		arrParent = arrChild
		return
	endif

	!Elitist reproduction for SGA

	bestIndex = FindBest( arrFit, nPopSize )
	bestParent = arrParent( bestIndex, : )

	!copy child to parent and search if the best parent is already in children
	bFoundElite = .false.
	do i=1, nPopSize
		kElite = 0
		do j=1, nChromBits
			arrParent(i,j) = arrChild(i,j)
			if( arrParent(i,j) .eq. bestParent(j) )kElite=kElite+1
		enddo
		if( kElite==nChromBits )bFoundElite = .true.
	end do

	if( .not. bFoundElite )then
		id = 1 + dint( dble(nPopSize)*UniRand() )
		arrParent( id, : ) = bestParent(:)
	endif

contains
	!##################################################################################
	!This subroutine find the best individual index (with minimum fitness value).
	integer function FindBest( arrFit, nPopSize )
	implicit none
		!argument
		integer, intent(in) :: nPopSize
		double precision, dimension(nPopSize), intent(in) :: arrFit

		!variable
		integer i, index
		double precision :: minFit

		index = 1
		minFit = arrFit(1)
		do i=2, nPopSize 
			if( arrFit(i) < minFit )then
				minFit = arrFit(i)
				index = i
			endif
		enddo

		FindBest = index
	end function FindBest
end subroutine NewGen

subroutine DoRestart( sgaParam )
implicit none
	!arguments
	type(SGAPARAMSTRUCT), intent(out) :: sgaParam
	!variables
	integer :: i, j

	call OpenInputFile( hRestartFile, strRestartFile )
	read( hRestartFile, * )sgaParam%iCurGen
	do i = 1, sgaParam%nPopSize
		read( hRestartFile, * )( sgaParam%arrPop(i,j), j=1, sgaParam%nChromBits )
	end do
	call LoadRand( hRestartFile )

	!set restart flag
	sgaParam%bRestarted = .true.
	close( hRestartFile)
end subroutine DoRestart
	
subroutine SaveRestart( sgaParam )
implicit none
	!arguments
	type(SGAPARAMSTRUCT), intent(in) :: sgaParam

	!variables
	integer :: i, j

	call OpenOutputFile( hRestartFile, strRestartFile )
	write( hRestartFile, * ) sgaParam%iCurGen
	do i = 1, sgaParam%nPopSize
		write( hRestartFile, '(I5, 3x, 100I2)' )( sgaParam%arrPop(i,j), j=1, sgaParam%nChromBits )
	end do
	call SaveRand( hRestartFile )

	close( hRestartFile)
end subroutine SaveRestart

logical function IsConverged( sgaParam )
implicit none
	!arguments
	type(SGAPARAMSTRUCT), intent(in) :: sgaParam

	!variables
	integer :: i, nConverged
	double precision :: rMinFit, rDif, rTol

	rMinFit = minval( sgaParam%arrFit )
	nConverged = 0
	do i = 1, sgaParam%nPopSize
		if( dabs(rMinFit)<TOL_MINVAL )then
			rDif = dabs( rMinFit-sgaParam%arrFit(i) )
			rTol = TOL_MINVAL
		else
			rDif = dabs( sgaParam%arrFit(i)/rMinFit - 1.d0 )
			rTol = TOL_RATIO
		endif
		if( rDif < rTol )nConverged = nConverged + 1
	enddo

	print *, nConverged
	if( dble(nConverged)/dble(sgaParam%nPopSize) < CONVERGE_RATIO )then
		IsConverged = .false.
	else
		IsConverged = .true.
	endif
end function IsConverged

subroutine SaveInd(hFile, sgaParam)
implicit none
	!arguments
	type(SGAPARAMSTRUCT), intent(inout) :: sgaParam
	integer, intent(in) :: hFile

	!variables
	integer :: j, id

	write( hFile, fmt=1000 ) sgaParam%iCurGen 
	do id = 1,sgaParam%nPopSize
		if( sgaParam%arrFlags(id)==NN_EVAL )then		!neural network
			write( hFile, fmt=1074, advance="NO" ) id, sgaParam%arrFit(id)
		else if( sgaParam%arrFlags(id)==CACHE_EVAL )then	!from cache
			write( hFile, fmt=1075, advance="NO" ) id, sgaParam%arrFit(id)
		else if( sgaParam%arrFlags(id)==PDE_EVAL )then
			write( hFile, fmt=1076, advance="NO" ) id, sgaParam%arrFit(id)
		else
			call assert( .false. )
		endif
                                
		!well locations
		do j=1, sgaParam%nVarCount
			write( hFile, fmt=1078, advance="NO" )sgaParam%arrVars(id, j)
		enddo
		!just print the catridge                                                                                                                                                                  
		write(hFile, fmt=1081, advance="YES")                                                                                                                                                    
	enddo
                                                                                                                                                                                                      
	1000 format(/'#################  Generation',i5,'  #################')
	1074 format('-',i4,2x,f17.4)
	1075 format('*',i4,2x,f17.4)
	1076 format(' ',i4,2x,f17.4)
	1078 format(2x,f7.4)
	1081 format(1x)
end subroutine SaveInd

end module SGA

