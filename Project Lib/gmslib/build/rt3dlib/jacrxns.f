c
c Evaluation of Jacobian for the
c Reaction package discussed in Example-2 (method-1)
c Please refer to the RT3D user manual for further details
c
	SUBROUTINE jrxns(ncomp,nvrxndata,j,i,k,ml,mu,y,pd,
     &         poros,rhob,reta,rc,nlay,nrow,ncol,vrc)
C*Block 1:**************************************************************
c List of calling arguments
c ncomp - Total number of components
c nvrxndata - Total number of variable reaction parameters to be input via RCT file
c J, I, K - node location (used if reaction parameters are spatially variable)
c ml,mu - are for banded jacobian (not used)
c y - Concentration value of all component at the node [array variable y(ncomp)]
c pd - jacobian matrix [(ncomp x ncomp) array]
c poros - porosity of the node
c reta -  Retardation factor [ignore dummy reta values of immobile species]
c rhob -  bulk density of the node
c rc - Stores spatially constant reaction parameters (can dimension upto 100 values)
c nlay, nrow, ncol - Grid size (used only for dimensioning purposes)
c vrc - Array variable that stores spatially variable reaction parameters
C*End of Block 1********************************************************

C*Block 2:**************************************************************
c*    *Please do not modify this standard interface block*
      IMPLICIT NONE
      INTEGER ncol,nrow,nlay
      INTEGER ncomp,nvrxndata,j,i,k,ml,mu
      INTEGER, SAVE :: First_time=1
      DOUBLE PRECISION y,pd,poros,rhob,reta
      DOUBLE PRECISION rc,vrc
      DIMENSION y(ncomp),pd(ncomp,ncomp),rc(100)
      DIMENSION vrc(ncol,nrow,nlay,nvrxndata),reta(ncomp)
C*End of block 2********************************************************

C*Block 3:************************************************************** 
c     *Declare your problem-specific new variables here*
c     INTEGER 
      DOUBLE PRECISION pce,tce,dce,vc,kpce,ktce,kdce,kvc 
      DOUBLE PRECISION ytcepce,ydcetce,yvcdce
C*End of block 3********************************************************

C*Block 4:**************************************************************
c      *Initilize reaction parameters here, if required*
      IF (First_time .EQ. 1) THEN
         kpce = 0.005  !PCE first-order degradation rate
         ktce = 0.003  !TCE first-order degradation rate
         kdce = 0.002  !DCE first-order degradation rate
         kvc = 0.001  !VC first-order degradation rate
	 ytcepce = 131.36/165.8
	 ydcetce = 96.9/131.36
	 yvcdce = 62.45/96.9
         First_time = 0 !reset First_time to skip this block later
      END IF
C*End of block 4********************************************************

C*Block 5:**************************************************************
c      *Assign or compute values for new variables, if required*
       pce = y(1)
       tce = y(2)
       dce = y(3)
       vc = y(4)
C*End of block 5********************************************************

C*Block 6:**************************************************************
c      *Definition of full (n x n) analytical jacobian matrix*
         pd(1,1) = -kpce/reta(1)
	 pd(1,2) = 0.0d0
	 pd(1,3) = 0.0d0
	 pd(1,4) = 0.0d0
         pd(2,1) = kpce*ytcepce/reta(2)
         pd(2,2) = -ktce/reta(2)
	 pd(2,3) = 0.0d0
	 pd(2,4) = 0.0d0
         pd(3,1) = ktce*ydcetce/reta(3)
         pd(3,2) = -kdce/reta(3)
	 pd(3,3) = 0.0d0
	 pd(3,4) = 0.0d0
         pd(4,1) = kdce*yvcdce/reta(4)
         pd(4,2) = -kvc/reta(4)
	 pd(4,3) = 0.0d0
	 pd(4,4) = 0.0d0
C*End of block 6********************************************************

      RETURN
      END

