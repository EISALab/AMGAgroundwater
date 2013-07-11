module SGA
use STD_HELPER
use statcomp

double precisIon, parameter :: EPS		= 1E-10

integer, parameter :: WELL_INJECTION	= 1
integer, parameter :: WELL_EXTRACTION	= 2
integer, parameter :: WELL_DONTCARE		= 3

integer, parameter :: SELECT_TOURNAMENT = 1

integer, parameter :: CROSS_RANDPOINT	= 0
integer, parameter :: CROSS_UNIFORM		= 1

integer, parameter :: MUTATE_CREEP		= 1

integer, parameter :: NN_EVAL			= 0
integer, parameter :: PDE_EVAL			= 1
integer, parameter :: CACHE_EVAL		= 2

integer :: hChromCache

type SGAPARAMSTRUCT
	integer, allocatable, dimension(:,:) :: arrPopInj			!the best chrosomes evaluated by PDE so far
	double precision, allocatable, dimension(:) :: arrFitInj, arrFitStdsInj	!the best fitness evaluated by PDE so far
	double precision, allocatable, dimension(:) :: arrCostsInj, arrCostStdsInj, arrViolsInj, arrViolStdsInj, arrReliabsInj

	integer, allocatable, dimension(:,:) :: arrPop				!the current chrosome pool arrPop(nPopSize, nChromBits)
	double precision, allocatable, dimension(:) :: arrFit		!the fitness value Fitness(nPopSize)
	double precision, allocatable, dimension(:) :: arrFitStds	!the fitness standard deviation value Fitness(nPopSize)
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

	double precision, allocatable, dimension(:) :: arrCosts, arrCostStds
	double precision, allocatable, dimension(:) :: arrViols, arrViolStds
	double precision, allocatable, dimension(:) :: arrReliabs	!the reliabilities arrReliabs(nPopSize)

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
														
!	logical :: iCrossIndex							!0 : do uniform single point, 1 : 

	integer :: nSamples								!the number of samples
	integer, allocatable, dimension(:, :) :: arrRealizations		!the sample realization ids.

	integer, allocatable, dimension(:) :: arrMomentIds
	double precision, allocatable, dimension(:) :: arrMoments

end type SGAPARAMSTRUCT

type(SGAPARAMSTRUCT), pointer :: g_sgaParam

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
			nBits = sgaParam%nChromBits - sgaParam%arrVarIndex(i) + 1
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

subroutine EncodeOneEx( sgaParam, arrVars, arrChrom )
	!argument
	type(SGAPARAMSTRUCT), intent(in) :: sgaParam
	double precision, dimension(*), intent(in) :: arrVars
	integer, dimension(*), intent(out) :: arrChrom

	!variable
	integer :: i, j, nBits, index, nVal

	do i=1, sgaParam%nVarCount
		if( i < sgaParam%nVarCount )then
			nBits = sgaParam%arrVarIndex(i+1) - sgaParam%arrVarIndex(i)
		else
			nBits = sgaParam%nChromBits - sgaParam%arrVarIndex(i) + 1
		endif
		
		index = sgaParam%arrVarIndex(i)
		nVal = nint( (arrVars(i) - sgaParam%arrVarMin(i)) / sgaParam%arrVarStride(i) )
		do j = 1, nBits
			if( nVal - 2**(nBits-j) >= 0 )then
				arrChrom( index + j -1 ) = 1
				nVal = nVal - 2**(nBits-j)
			else
				arrChrom( index + j -1 ) = 0
			endif
		enddo
		call assert( nVal==0 )
	enddo
end subroutine EncodeOneEx

	
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


	nPopSize = sgaParam%nPopSize
	iStart = sgaParam%iStartGen
	iEnd = iStart + sgaParam%nMaxGens - 1

	call InitPopulation(sgaParam)

	!Main processing loop for the simple genetic algorithm
	do iCur = iStart, iEnd

		sgaParam%iCurGen = iCur

		call FitEval( sgaParam )

!		call assert( .not. sgaParam%bInjecFlag )		!don't support island injection now
		if( sgaParam%bInjecFlag )then
			call BestInjection( sgaParam )
		endif


		call assert( iCur<=iStart + sgaParam%nMaxGens - 1 )
		if( sgaParam%bNicheFlag )call Niche

		!Perform selection & then perform crossover between the randomly selected pair.
		do i = 1, nPopSize, 2
			!selection may shuffle parent array, each call generate two children
			call SelectionWithUncertainty( sgaParam, arrChild, i )
!			call Selection( sgaParam%nSelectOpt, sgaParam%arrPop, arrChild, sgaParam%arrFit, &
!						&	sgaParam%nPopSize, sgaParam%nChromBits, i )
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
		call SaveRestart

	enddo
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
	integer, dimension( sgaParam%nPopSize*2 ) :: arrIndex


	nPopSize = sgaParam%nPopSize
	iStart = sgaParam%iStartGen
	iEnd = iStart + sgaParam%nMaxGens - 1

	call InitPopulation(sgaParam)

	!assign a very big value to arrFitPool(nPopSize,2*nPopSize) so that it won't be choosed at first generation.
	arrFitPool = 1d100

	!Main processing loop for the simple genetic algorithm
	do iCur = iStart, iEnd

		sgaParam%iCurGen = iCur

!		call ChooseRealizations( sgaParam, sgaParam%arrRealizations )
		call FitEval( sgaParam )

		call assert( .not. sgaParam%bInjecFlag )		!don't support island injection now
		if( sgaParam%bInjecFlag )then
			!island injection code here
		endif

		!put the current generation into the pool to sort it with it last generation
		do i=1, sgaParam%nPopSize
			arrFitPool(sgaParam%nPopSize+i) = sgaParam%arrFit(i)
			arrChromPool(sgaParam%nPopSize+i, :) = sgaParam%arrPop(i,:)
		enddo
			
!		arrFitPool(sgaParam%nPopSize:sgaParam%nPopSize*2) = sgaParam%arrFit(1:sgaParam%nPopSize)
!		arrChromPool(sgaParam%nPopSize:sgaParam%nPopSize*2, :) = sgaParam%arrPop
		arrIndex = (/(i, i=1,sgaParam%nPopSize*2)/)

		call qsort( arrIndex, 2*sgaParam%nPopSize, sizeof(arrIndex(1)), FitCompare )

		!for debug
		if( iCur==1 )arrIndex(1:sgaParam%nPopSize) = (/(i, i=1+sgaParam%nPopSize, sgaParam%nPopSize*2)/)

		!this is the real parent, the first nPopSize individuals
		do i = 1, sgaParam%nPopSize
			sgaParam%arrPop(i, :) = arrChromPool( arrIndex(i), : )
			sgaParam%arrFit(i) = arrFitPool( arrIndex(i) )
		enddo

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
		arrChromPool(1:sgaParam%nPopSize, :) = sgaParam%arrPop
		sgaParam%arrPop = arrChild
		
		!Write child array back into parent array for new generation.  
		!Check to see if the best parent was replicated.
		!call NewGen( sgaParam%bEliteFlag, sgaParam%arrPop, arrChild, sgaParam%arrFit, sgaParam%nPopSize, sgaParam%nChromBits )

		if( sgaParam%bMicroFlag )call Micro

		!Write to restart file.
!		call SaveRestart

	enddo
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
	integer :: iPick, iMate1, iMate2

	iMate1 = 1+dint(dble(nPopSize-1)*UniRand())	!this is not perfect, now for debug only
	iMate2 = 1+dint(dble(nPopSize-1)*UniRand())

	if( arrFits(iMate1) < arrFits(iMate2) )then
		iPick = iMate1
	else
		iPick = iMate2
	endif

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

!Do tournament selection with uncertainty
subroutine TournSelectWithUncertainty( sgaParam, arrChild, iChildIndex )
implicit none
	!argument
	type(SGAPARAMSTRUCT), intent(inout) :: sgaParam
	integer, dimension( sgaParam%nPopSize, sgaParam%nChromBits ), intent(inout) :: arrChild
	integer, intent(in) :: iChildIndex

	!variable
	integer :: iPick, iMate1, iMate2, nPopSize

	nPopSize = sgaParam%nPopSize
	iMate1 = 1+dint(dble(nPopSize-1)*UniRand())	!this is not perfect, now for debug only
	iMate2 = 1+dint(dble(nPopSize-1)*UniRand())

	if( CompareByCostViolTest(iMate1, iMate2) <=0 )then
		iPick = iMate1
	else
		iPick = iMate2
	endif

	arrChild( iChildIndex, : ) = sgaParam%arrPop( iPick, : )
end subroutine

!Subroutine for selection operator.
subroutine SelectionWithUncertainty( sgaParam, arrChild, iChildIndex )
implicit none
	!argument
	type(SGAPARAMSTRUCT), intent(inout) :: sgaParam
	integer, dimension( sgaParam%nPopSize, sgaParam%nChromBits ), intent(inout) :: arrChild
	integer :: iChildIndex

	!argument
	call assert( sgaParam%nSelectOpt==SELECT_TOURNAMENT )

	if( sgaParam%nSelectOpt == SELECT_TOURNAMENT )then
		call TournSelectWithUncertainty( sgaParam, arrChild, iChildIndex )
		call TournSelectWithUncertainty( sgaParam, arrChild, iChildIndex+1 )
	end if
end subroutine


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

subroutine BestInjection(sgaParam)
implicit none
	!arguments
	type(SGAPARAMSTRUCT), intent(inout) :: sgaParam

	!variables
	integer, dimension(sgaParam%nPopSize) :: arrIndex
	integer :: i, k, id

	!the following logic do the injection. injection the best saved pde individuals to the population

	!Inject the cached PDEs into the population, the worest individuals are replaced
	arrIndex = (/(i, i=1,sgaParam%nPopSize)/)
	call qsort( arrIndex, sgaParam%nPopSize, sizeof(arrIndex(1)), CompareByCostViolTestPdeFirst )
	if( sgaParam%iCurGen>1 )then
		do i=lbound(sgaParam%arrFitInj, 1), ubound(sgaParam%arrFitInj,1)
			if( sgaParam%arrFitInj(i)>=1d80 )cycle

			id = arrIndex( sgaParam%nPopSize-i+1 )
			sgaParam%arrPop(id,:) = sgaParam%arrPopInj(i,:)
			sgaParam%arrFit(id) = sgaParam%arrFitInj(i)
			sgaParam%arrFitStds(id) = sgaParam%arrFitStdsInj(i)
			sgaParam%arrCosts(id) = sgaParam%arrCostsInj(i)
			sgaParam%arrCostStds(id) = sgaParam%arrCostStdsInj(i)
			sgaParam%arrViols(id) = sgaParam%arrViolsInj(i)
			sgaParam%arrViolStds(id) = sgaParam%arrViolStdsInj(i)
			sgaParam%arrFlags(id) = CACHE_EVAL
		enddo
	endif

	!save the best individuals of the population to the small injection cache
	do i=lbound(sgaParam%arrFitInj, 1), ubound(sgaParam%arrFitInj,1)
		id = arrIndex(i)
		if( sgaParam%arrFlags(id)==NN_EVAL )exit
		do k=lbound(sgaParam%arrFitInj, 1), ubound(sgaParam%arrFitInj,1)
			if( sgaParam%arrFit(id)<sgaParam%arrFitInj(k) )then
				sgaParam%arrPopInj(k,:)=sgaParam%arrPop(id,:)
				sgaParam%arrFitInj(k) = sgaParam%arrFit(id)
				sgaParam%arrFitStdsInj(k) = sgaParam%arrFitStds(id)
				sgaParam%arrCostsInj(k) = sgaParam%arrCosts(id)
				sgaParam%arrCostStdsInj(k) = sgaParam%arrCostStds(id)
				sgaParam%arrViolsInj(k) = sgaParam%arrViols(id)
				sgaParam%arrViolStdsInj(k) = sgaParam%arrViolStds(id)
				exit
			endif
		enddo
	enddo

end subroutine

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
			if( arrFit(i) .le. minFit )then
				minFit = arrFit(i)
				index = i
			endif
		enddo

		FindBest = index
	end function FindBest
end subroutine NewGen

subroutine SaveRestart
end subroutine SaveRestart

integer(2) function CompareByFitness(i1, i2)
implicit none
	integer :: i1, i2

	double precision :: r1, r2
	r1 = g_sgaParam%arrFit(i1)
	r2 = g_sgaParam%arrFit(i2)

	CompareByFitness = CompareDouble( r1, r2 )
end function


integer(2) function CompareByCostViolTest(i1, i2)
use chmcache
implicit none
	integer :: i1, i2

	double precision, dimension(2) :: arrCost1, arrCost2, arrViol1, arrViol2
	logical :: bPde1, bPde2
	integer :: nc, mc
	arrCost1 = (/ g_sgaParam%arrCosts(i1), g_sgaParam%arrCostStds(i1) /)
	arrCost2 = (/ g_sgaParam%arrCosts(i2), g_sgaParam%arrCostStds(i2) /)
	arrViol1 = (/ g_sgaParam%arrViols(i1), g_sgaParam%arrViolStds(i1) /)
	arrViol2 = (/ g_sgaParam%arrViols(i2), g_sgaParam%arrViolStds(i2) /)
	bPde1 = g_sgaParam%arrFlags(i1)/=NN_EVAL
	bPde2 = g_sgaParam%arrFlags(i2)/=NN_EVAL

!for debuging
	CompareByCostViolTest = CompareByFitnessTest(i1,i2)
!	CompareByCostViolTest = CompareByFitness(i1,i2)
	return

	if( bPde1 .and. bPde2 )then
		nc = GetSampleCountCache( hChromCache, g_sgaParam%arrPop(i1,:), g_sgaParam%nChromBits )
		mc = GetSampleCountCache( hChromCache, g_sgaParam%arrPop(i2,:), g_sgaParam%nChromBits )
		CompareByCostViolTest = CompareTwoTCostViol( arrCost1, arrViol1, nc, arrCost2, arrViol2, mc )
	else if( bPde1 )then
		nc = GetSampleCountCache( hChromCache, g_sgaParam%arrPop(i1,:), g_sgaParam%nChromBits )
		CompareByCostViolTest = CompareOneTCostViol( arrCost1, arrViol1, nc, arrCost2, arrViol2 )
	else if( bPde2 )then
		mc = GetSampleCountCache( hChromCache, g_sgaParam%arrPop(i2,:), g_sgaParam%nChromBits )
		CompareByCostViolTest = -CompareOneTCostViol( arrCost2, arrViol2, mc, arrCost1, arrViol1 )
	else
		CompareByCostViolTest = CompareCostViol( arrCost1(1), arrViol1(1), arrCost2(1), arrViol2(1) )
	endif
end function

integer(2) function CompareByFitnessTest(i1, i2)
use chmcache
implicit none
	integer :: i1, i2

	double precision, dimension(2) :: arrFit1, arrFit2
	logical :: bPde1, bPde2
	integer :: nc, mc
	arrFit1 = (/ g_sgaParam%arrFit(i1), g_sgaParam%arrFitStds(i1) /)
	arrFit2 = (/ g_sgaParam%arrFit(i2), g_sgaParam%arrFitStds(i2) /)
	bPde1 = g_sgaParam%arrFlags(i1)/=NN_EVAL
	bPde2 = g_sgaParam%arrFlags(i2)/=NN_EVAL

	if( bPde1 .and. bPde2 )then
		nc = GetSampleCountCache( hChromCache, g_sgaParam%arrPop(i1,:), g_sgaParam%nChromBits )
		mc = GetSampleCountCache( hChromCache, g_sgaParam%arrPop(i2,:), g_sgaParam%nChromBits )
		CompareByFitnessTest = CompareByTwoSampleT( arrFit1(1), arrFit1(2), nc, arrFit2(1), arrFit2(2), mc, g_rTestAlpha )
	else if( bPde1 )then
		nc = GetSampleCountCache( hChromCache, g_sgaParam%arrPop(i1,:), g_sgaParam%nChromBits )
		CompareByFitnessTest = CompareByOneSampleT( arrFit1(1), arrFit1(2), nc, arrFit2(1), g_rTestAlpha )
	else if( bPde2 )then
		mc = GetSampleCountCache( hChromCache, g_sgaParam%arrPop(i2,:), g_sgaParam%nChromBits )
		CompareByFitnessTest = -CompareByOneSampleT( arrFit2(1), arrFit2(2), mc, arrFit1(1), g_rTestAlpha )
	else
		CompareByFitnessTest = CompareDouble( arrFit1(1), arrFit2(1) )
	endif
end function

integer(2) function CompareByCostViolTestPdeFirst(i1, i2)
implicit none
	integer :: i1, i2

	if( g_sgaParam%arrFlags(i1)/=g_sgaParam%arrFlags(i2) )then
		if( g_sgaParam%arrFlags(i1)==NN_EVAL )then
			CompareByCostViolTestPdeFirst = 1
			return
		else if( g_sgaParam%arrFlags(i2)==NN_EVAL )then
			CompareByCostViolTestPdeFirst = -1
			return
		endif
	endif

	CompareByCostViolTestPdeFirst = CompareByCostViolTest(i1, i2)
end function

end module

!return : 0 - the two chromosomes are different. 1 - the two chromosomes are the same.
integer function CompareChroms( arrChrom1, arrChrom2 )
end function

