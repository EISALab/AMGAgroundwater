c
c Reaction package discussed in Example-2 (method-1)
c Please refer to the RT3D user manual for further details
c
	SUBROUTINE rxns(ncomp,nvrxndata,j,i,k,y,dydt,
     &         poros,rhob,reta,rc,nlay,nrow,ncol,vrc)
C*Block 1:**************************************************************
c List of calling arguments
c ncomp - Total number of components
c nvrxndata - Total number of variable reaction parameters to be input via RCT file
c J, I, K - node location (used if reaction parameters are spatially variable)
c y - Concentration value of all component at the node [array variable y(ncomp)]
c dydt - Computed RHS of your differential equation [array variable dydt(ncomp)]
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
      INTEGER ncomp,nvrxndata,j,i,k
      INTEGER, SAVE :: First_time=1
      DOUBLE PRECISION y,dydt,poros,rhob,reta
      DOUBLE PRECISION rc,vrc
      DIMENSION y(ncomp),dydt(ncomp),rc(100)
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
c      *Differential Reaction Equations*
       dydt(1) = -kpce*pce/reta(1)
       dydt(2) = (-ktce*tce + kpce*pce*ytcepce)/reta(2)
       dydt(3) = (-kdce*dce + ktce*tce*ydcetce)/reta(3) 
       dydt(4) = (-kvc*vc + kdce*dce*yvcdce)/reta(4)
C*End of block 6********************************************************

      RETURN
      END

