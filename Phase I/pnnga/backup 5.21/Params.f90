!Author:      Rachel Arst & Felipe Espinoza
!Date:        July 2001
!Filename:    Params.f90
!Desciption:  Contains parameters that are used in all GAs in this driver.

! Params1: Params for all GAs
! Params2: Params for SGA only
! Params3: Pamars for NSGAI only
! Params4: Params for SGA and NSGAI
! Params5: Params for ECGA
! Params6: Params for HGA
! Params7: Pamars for NSGAII only
!
! The parameters in this file always must be set -- 
! no matter which GA you would like to use (but the only ones important for you are the ones
! for the code you are worknig with).
!
! If you would like to run the SGA you must set the parameters in:
! Params1, Params2 and Params4.
!
! If you would like to run the NSGA you must set the parameters in:
! Params1, Params3 and Params4.
!
! If you would like to run the ECGA you must set the parameters in:
! Params1 and Params5.
!
! If you would like to run the HGA you must set the parameters in:
! Params1, Params2, Params4 and Params6.
!
! For noisy GAs in general you need to set the files for SGA or NSGA plus specific input
! files for the particular problem (noise information).

module Params1

   IMPLICIT NONE

!*********************************************************************************************

  integer :: gaflag = 1
   ! GA flag 
   ! = 1 for SGA 
   ! = 2 for NSGAI
   ! = 3 for ECGA
   ! = 4 for BOA
   ! = 5 for HGA 
   ! = 6 for NSGAII 
  integer, parameter :: indmax = 5000
   ! maximum # of individuals, i.e. max population size
   ! if using Stochastic Remainder Selection should make
   ! indmax equal to approximately 2 times larger than
   ! the population size

  integer :: npopsize 
   ! The population size of a GA run 
   ! For a single calculation, set equal to 1
   ! For the ecga, this must be >= 16 and an even #.

!*********************************************************************
! Please DO NOT  change anything (in this file) below this line.
!*********************************************************************

   integer, parameter :: nchrmax = 48
   ! maximum # of chromosomes (binary bits) per individual

   integer :: nparam
    ! Number of parameters (groups of bits) of each individual.
    ! Make sure that nparam matches the number of values in the 
    ! parmin, parmax and nposibl input arrays.

   double precision, dimension(100) :: parmin
    ! array of the minimum allowed values of the parameters

   double precision, dimension(100) :: parmax
    ! array of the maximum allowed values of the parameters
    
  integer :: nparmax
   ! maximum # of parameters which the chromosomes make up
   ! must equal nparam

!*********************************************************************************************
   
end module Params1

!=============================================================================================


module Params2

   IMPLICIT NONE

!*********************************************************************************************

   logical :: iniche = .false.
    ! = .false. for no niching
    ! = .true.  for niching; niching is recommended.

   logical :: microga = .false.
    ! = .false. for normal conventional GA operation
    ! = .true.  for micro-GA operation (this will automatically reset 
    !           some of the other input flags). I recommend using 
    !           npopsiz=5 when microga=1.

  integer, parameter :: optflag = 1
   ! Optimization flag
   ! = 1 for Minimization
   ! = 2 for Maximization

    double precision :: cstop=0.80d0
   ! Proportion of the population took over by the final solution 
   
    double precision :: tol1=1d-3
   ! Tolerance for stopping criterion: diference
   
    double precision :: tol2=1d-3
   ! Tolerance for stopping criterion: ratio

!*********************************************************************************************

end module Params2

!============================================================================================= 
    
    
 module Params3

   IMPLICIT NONE

!*********************************************************************************************

   integer :: pheno = 0
    ! This value allows the user to choose between phenotypic &
    ! genotypic sharing.  Phenotypic sharing uses the distance
    ! between the decoded fitness values for two individuals.
    ! Genotypic sharing uses distance between the parameters for
    ! each of two individuals
    ! pheno = 1, then phenotypic sharing is used, else genotypic 
    ! sharing is used
  
   double precision :: delta_dum = 1.0d0
    ! This value represents the amount the dummy fitness of each
    ! front will decrease from the minimum value of the preceeding
    ! front. Used in NSGA. NOTE: set this value so it is appropriate
    ! for the form of sharing you are using
  
   double precision :: dshare = 0.158d0
    ! This value is the maximum phenotypic/genotypic distance allowed between
    ! two individuals to become members of a niche (see Srinivas 
    ! Deb, 1995).Used in NSGA. NOTE: set this value so it is 
    ! appropriate for the form of sharing you are using
  
   double precision :: dtol = 0.000001d0
    ! This value represents the phenotypic/genotypic distance between two
    ! individuals when they considered to be identical. If the
    ! distance is less than or equal to dtol then the nichecount
    ! for the individuals is increased by one.
    ! Recall dummyfitness = npopsiz/nichecount. Used in NSGA

   double precision :: elite_rad = 0.48d0
    ! specifies the distance required between successive individuals befor
    ! an additional point can be added to current elite non-dominated set


!*********************************************************************************************

end module Params3

!============================================================================================= 

module Params4

  IMPLICIT NONE
  
!*********************************************************************************************

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Variables constants for array bounds !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  integer, parameter :: maxobj = 1
  ! maximum # of objectives: if 1 then Simple GA
  !		             if > 1 use NSGA
  ! if > 1 must formulate problem assuming minimization of 
  ! all objectives

  !!!!!!!!!!!!!!!!!!!!!
  ! Control variables !
  !!!!!!!!!!!!!!!!!!!!!

  integer, parameter :: maxgen = 96
   ! The maximum number of generations to run by the GA.  
   ! For a single function evaluation, set equal to 1.

  integer :: itourny = 1
   ! = 1  for tournament selection 
   ! = 0  for stochastic remainder selection (used only with NSGA) 

  logical :: ielite = .true.
   ! = .false. for no elitism (best individual(s) not necessarily 
   !           replicated from one generation to the next).
   ! = .true.  for elitism to be invoked (best individual(s) replicated 
   !           into next generation); elitism is recommended.

  integer :: idum =-43003
   ! The initial random number seed for the GA run. Must equal 
   ! a negative integer, e.g. idum=-1000.(-26532)

  logical :: irestrt = .false.
   ! = .false. for a new GA run, or for a single function evaluation
   ! = .true.  for a restart continuation of a GA run.
  
  logical :: nowrite = .true.
   ! = .false. to write detailed mutation and parameter adjustments
   ! = .true.  to not write detailed mutation and parameter adjustments
  
  integer :: kountmx = 5
   ! the maximum value of kount before a new restart file is
   ! written; presently set to write every fifth generation.  
   ! Increasing this value will reduce I/O time requirements
   ! and reduce wear and tear on your storage device 

  logical :: iunifrm = .false.
   ! = .false. for single-point crossover
   ! = .true.  for uniform crossover; uniform crossover is 
   !           recommended.

  double precision :: pcross = 0.5d0
  ! The crossover probability.  For single-point crossover, a 
  ! value of 0.6 or 0.7 is recommended. For uniform crossover,
  ! a value of 0.5 is suggested.

  integer :: nchild = 2
   ! = 1 for one child per pair of parents (this is what I 
   !     typically use).
   ! = 2 for two children per pair of parents (2 is more common 
   !     in GA work).

   logical :: mutation = .true.
   ! = .true. set pmutate to 1/npopsiz
   ! = .false. set by user

  double precision :: pmutate = 0.001d0
   ! The jump mutation probability.  Typically set = 1/npopsiz.

  logical :: icreep = .false.
   ! = .false. for no creep mutations
   ! = .true.  for creep mutations; creep mutations are recommended.
  
  double precision :: pcreep = 0.02d0
   ! The creep mutation probability.  Typically set this
   ! = (nchrome/nparam)/npopsiz.
  
  integer, dimension(100) :: nichflg
   ! array of 1/0 flags for whether or not niching occurs on
   ! a particular parameter.  Set to 0 for no niching on
   ! a parameter, set to 1 for niching to operate on parameter.
   ! The default value is 1, but the implementation of niching 
   ! is still controlled by the flag iniche.

  integer, dimension(100) :: nposibl
   ! array of integer number of possibilities per parameter.
   ! For optimal code efficiency set nposibl=2**n, i.e. 2, 4,
   ! 8, 16, 32, 64, etc.
 
!*********************************************************************************************
    
end module Params4

!============================================================================================= 

module Params5

   IMPLICIT NONE

!*********************************************************************************************

  double precision :: seed_x = 0.254534d0
   ! This is the seed that will be fed to the random number generator
   ! It must be between 0 and 1.

  double precision :: probability_crossover = 1.0d0
   ! This is the probability of crossover. 1 means that the whole
   ! population is regenerated after each generation cycle

  integer :: tournament_size = 16
   ! The size of the tournament

  character( LEN = 5 ) :: learn_MPM = "on"
   !  Can be set to "on" or "off"
   ! "on" means that the ga will learn the MPM every generation
   ! "off" means that the regular compact ga will run

  character( LEN = 18 ) :: stop_criteria = "allele_convergence"
   ! Can be set to "allele_convergence" or "maximu_generations"
   ! 80% is used for allele_convergence

  double precision :: stop_criteria_argument = 1.0d0
   ! This should be set to 0 if the "allele_convergence" criteria was used above
   ! If "max_generations" is the criteria, specify the max # of generations here

  character ( LEN = 10 ) :: outputfile = "outputfile"
   ! Please specify the name of the output file here
   ! It has to be 10 characters or less.


   ! The following options are reporting flags screen, everything will be written to output file

  character ( LEN = 5 ) :: report_pop = "off"
   ! set to "on" or "off"
   ! shows the population on the screen when set to "on"

  character ( LEN = 5 ) :: report_string = "on"
   ! set to "on" or "off"
   ! shows the string on the screen when set to "on"

  character ( LEN = 5 ) :: report_fitness = "on"
   ! set to "on" or "off"
   ! shows the fitness on the screen when set to "on"

  character ( LEN = 5 ) :: report_MPM = "on"
   ! set to "on" or "off"
   ! shows the MPM on the screen when set to "on"

!********************************************************************
! DO NOT change any of the parameters below this line
!********************************************************************

  integer, dimension(8) :: nbits
   ! This is the number of bits for each parameter
   ! The number of possibilities per parameter is calculated by 2^nbits.
   ! This value must correspond with nposib in Params_sga_nsga.f90
   !  if the sga or nsga are being used.
   ! For example, when nbits = 16, nposibl = 2^16 = 65,536.
   
  integer :: chromosome_length
   ! This number of binary bits in the problem

!*********************************************************************************************
   
end module Params5

!============================================================================================= 

module Params6

  IMPLICIT NONE
  
!*********************************************************************************************

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Variables constants for array bounds !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  integer, parameter :: maxlsind = 510
  ! maximum # of individuals undergoing local search
  ! the parameter must be lower or equal to population size

  !!!!!!!!!!!!!!!!!!!!!
  ! Control variables !
  !!!!!!!!!!!!!!!!!!!!!

  integer :: hgaflag 
   ! Hybrid GA flag
   ! = 1 for Non-Adaptive  (NAHGA)
   ! = 2 for Self-Adaptive (SAHGA)
   
  integer :: lsflag 
   ! Hybrid GA flag
   ! = 1 for Uniform Local Search
   ! = 2 for (1+1)-ES
   ! = 3 for Random Derivative
   ! = 4 for Gradient
   
  integer :: maxlsiter = 5
   ! The maximum number of local serch iterations 

  integer :: idumls = -123456
   ! The initial random number seed for the individual selection in local search. 
   ! Must equal a negative integer, e.g. idum=-1000.

  integer :: idumlsi = -123456
   ! The initial random number seed for local search iterations. 
   ! Must equal a negative integer, e.g. idum=-1000.

  integer :: idumb = -652625
   ! The initial random number seed for Baldwinian evolution 
   ! Must equal a negative integer, e.g. idum=-1000.
   
  integer :: idumn = -254125
   ! The initial random number seed for Normal number generation 
   ! Must equal a negative integer, e.g. idum=-1000.
      
  double precision :: probls = 0.10d0
   ! Probability of local search. 
   ! Proportion of individual undergoing local search.
   
  double precision :: propBald = 0.25d0
   ! Proportion of individual undergoing Baldwinian evolution.

  double precision :: alpha = 0.35d0
   ! Uniform searh space parameter.

  double precision :: CVmax = 0.35d0
   ! Coefficient of variation for (1+1)-ES

  integer :: DeltaGen = 3
   ! Frequency of local search to global search. 
   ! Parameter for NAHGA 

  double precision :: LGfac = 0.5d0
   ! Local/Global switch factor

  double precision :: eps = 0.1d0
   ! # of iterations adaptive parameter

   logical :: online = .true.
   ! = .true.  on-line performance on
   ! = .false. on-line performance off

   logical :: offline = .true.
   ! = .true.  off-line performance on
   ! = .false. off-line performance off

!*********************************************************************************************
    
end module Params6
