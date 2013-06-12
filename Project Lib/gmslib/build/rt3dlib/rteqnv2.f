
C*****Reaction model#1***********************************************C
c     Two-species instantaneous reactions (BIOPLUME-II type of rxn model)
c     To use this option ncomp=mcomp=2, nrxndata=1 and 
c     rxnarray1(1)=3.14 (to simulate BTEX and oxygen reaction)
c     (This routine is also a general purpose routine to simulate any
c     any number of instataneously depleted electron acceptors in 
c     a sequential fashion)
C*********************************************************************C		   
      SUBROUTINE rxneqn1(ncomp,conc_old,conc_new,reta)
      IMPLICIT NONE
      INTEGER ncomp,istore,i
      REAL*4 conc_old,conc_new,p,xx,yy,reta
      DIMENSION conc_old(ncomp),conc_new(ncomp),p(100)
      DOUBLE PRECISION rxnarray1
      COMMON /rxnconst1/rxnarray1(100)
c**conc_old is the input to subroutine and conc_new is the reacted concentrations
c**conc_old(1) has carbon, conc_old(2) oxy, conc_old(3) no3 and so on...
c**p is local array used to store accumulated carbon equivalents for electron acceptors
c**note p(1)=0.0, p(2) is used upto oxy, p(3) upto no3 and so on..
c**The factor for oxygen is in rxnarray1(1), for no3 in rxnarray1(2) and so on..
        p(1) = 0.0
        p(2)=conc_old(2)/(reta*rxnarray1(1))
        DO i=3, ncomp
           p(i)=p(i-1)+conc_old(i)/(reta*rxnarray1(i-1))
        END DO
c**case when carbon available is greater than all electron acceptors
        IF (conc_old(1) .GT. p(ncomp)) THEN
           conc_new(1) = conc_old(1)-p(ncomp)!amount of carbon left
           DO i = 2, ncomp
              conc_new(i) = 0.0 !set all electron acceptors to zero since they are consumed
           END DO
	   RETURN
        END IF
c**case when carbon is less than available electron acceptor
        DO i =2, ncomp 
c**        Locate the electron acceptor box where cut occurs and store the location in "istore"
           IF (conc_old(1).LT.p(i)) THEN
              istore = i
	      GOTO 10
	   END IF
        END DO
10      CONTINUE
        DO i= 2, istore-1 !elec acceptor concs below the cut box is zero
           conc_new(i)=0.0
        END DO
        DO i = istore+1, ncomp !elec acceptor concs beyond the box is unaffected
           conc_new(i)= conc_old(i)
        END DO
        xx = conc_old(1)-p(istore-1) !amount of elec accetor used from the cut box (in carbon units)
        yy = (conc_old(istore)/(reta*rxnarray1(istore-1))) - xx !unused amount of elec acceptor at the cut box
        conc_new(istore) = yy*reta*rxnarray1(istore-1) !convert it back to elec acceptor units
        conc_new(1) = conc_old(1)- (p(istore-1)+xx) ! compute the amount of carbon left
       RETURN
       END

C*****Reaction model#2***********************************************C
c     Six-species (BTEX,O2,NO3,Fe3,S04,methanogenesis) sequential, instantaneous reactions***
c     To use this option ncomp=mcomp=6, nrxndata=5, and input appropriate
c     mass ratio of reactions in rxnarray1(5).  
c     (This routine is also a general purpose routine to simulate any
c     number of instataneously depleted electron acceptors in a sequential fashion)
C*********************************************************************C		   
      SUBROUTINE rxneqn2(ncomp,conc_old,conc_new,reta)
      IMPLICIT NONE
      INTEGER ncomp,istore,i
      REAL*4 conc_old,conc_new,p,xx,yy,reta
      DIMENSION conc_old(ncomp),conc_new(ncomp),p(100)
      DOUBLE PRECISION rxnarray1
      COMMON /rxnconst1/rxnarray1(100)
c**conc_old is the input to subroutine and conc_new is the reacted concentrations
c**conc_old(1) has carbon, conc_old(2) oxy, conc_old(3) no3 and so on...
c**p is local array used to store accumulated carbon equivalents for electron acceptors
c**note p(1)=0.0, p(2) is used upto oxy, p(3) upto no3 and so on..
c**The mass ratio for oxygen is in rxnarray1(1), for no3 in rxnarray1(2) and so on..
        p(1) = 0.0
        p(2)=conc_old(2)/(reta*rxnarray1(1))
        DO i=3, ncomp
           p(i)=p(i-1)+conc_old(i)/(reta*rxnarray1(i-1))
        END DO
         
c**case when carbon available is greater than all electron acceptors
        IF (conc_old(1) .GT. p(ncomp)) THEN
           conc_new(1) = conc_old(1)-p(ncomp)!amont of carbon left
           DO i = 2, ncomp
              conc_new(i) = 0.0 !set all electron acceptors to zero since they are consumed
           END DO
	   RETURN !exit subroutine
        END IF
c**case when carbon is less than available electron acceptor
        DO i =2, ncomp 
c**        Locate the electron acceptor box where cut occurs and store the location in "istore"
           IF (conc_old(1).LT.p(i)) THEN
              istore = i
	      GOTO 10
	   END IF
        END DO
10      CONTINUE
        DO i= 2, istore-1 !elec acceptor concs below the cut box is zero
           conc_new(i)=0.0
        END DO
        DO i = istore+1, ncomp !elec acceptor concs beyond the box is unaffected
           conc_new(i)= conc_old(i)
        END DO
        xx = conc_old(1)-p(istore-1) !amount of elec accetor used from the cut box (in carbon units)
        yy = (conc_old(istore)/(reta*rxnarray1(istore-1))) - xx !unused amount of elec acceptor at the cut box
        conc_new(istore) = yy*reta*rxnarray1(istore-1) !convert it back to elec acceptor units
        conc_new(1) = conc_old(1)- (p(istore-1)+xx) ! compute the amount of carbon left

       RETURN
       END
	   
C*****Reaction model#3***********************************************C
c     Six-species (BTEX,O2,NO3,Fe3,S04,methanogenesis) sequential, 
c     first-order, rate-limited, electron acceptor reactions***
c     To use this option ncomp=mcomp=6, nrxndata=21, and input appropriate
c     reaction rate parameters in rc(21).  Note, out of these 21 
c     reaction parameters only the first seven are critical values that 
c     need to be adjusted
C*********************************************************************C		
        SUBROUTINE rxneqn3(ncomp,t,y,dydt)
        USE ARRAY_CONFIG		
        IMPLICIT NONE
        INTEGER ncomp,nvrxndata,j,i,k
        INTEGER nlay,nrow,ncol
	DOUBLE PRECISION t,y,dydt,dpporos,dprhob,dpreta
	DIMENSION y(ncomp),dydt(ncomp)
	DOUBLE PRECISION rc
    	COMMON /rxnconst1/rc(100)
        COMMON /rxnshare/dpporos,dprhob,dpreta(100),nvrxndata,j,i,k
        COMMON /rxnshare1/nlay,nrow,ncol
        CALL rxns3(ncomp,nvrxndata,j,i,k,y,dydt,
     &        dpporos,dprhob,dpreta(1),rc(1),nlay,nrow,ncol,rxnarray2)
        RETURN
        END

 	SUBROUTINE rxns3(ncomp,nvrxndata,j,i,k,y,dydt,
     &             poros,rhob,reta,rc,nlay,nrow,ncol,vrc)
C*********************************************************************C	
c****Definition of values in the rc(21) (when constant option is used)**
c****and definition of y(6) arrays********
c	rc(1)  = maximum amount of Fe2+ observed in the field
c	rc(2)  = maximum amount of Methane observed in the field
c	rc(3)  = Hydrocarbon decay rate via aerobic processes (khco)
c	rc(4)  = Hydrocarbon decay rate via denitrification (khcn)
c	rc(5)  = Hydrocarbon decay rate via iron reduction (khcFe)
c	rc(6)  = Hydrocarbon decay rate via sufate reduction (khcs)
c	rc(7)  = Hydrocarbon decay rate via methanogenesis (khcme)
c	rc(8)  = Oxygen switching factor (ko, assume 0.5 mg/L)
c	rc(9)  = Nitrate switching factor (kn, assume (0.5 mg/L)
c	rc(10) = Iron switching factor (kfe, assume 0.5 mg/L)
c	rc(11) = Sulfate switching factor (ks, assume 0.5 mg/L)
c	rc(12) = Methane switching factor (kme, assume 0.5 mg/L)
c	rc(13) = Oxygen inhibition factor (kio, assume 0.01 mg/L)
c	rc(14) = Nitrate inhibition factor (kin, assume 0.01 mg/L)
c	rc(15) = Iron inhibition factor (kife, assume 0.01 mg/L)
c	rc(16) = Sulfate inhibition factor (kis, assume 0.01 mg/L)
c	rc(17) = Mass ratio of oxygen to BTEX (Y[o/hc], 3.125)
c	rc(18) = Mass ratio of Nitrate to BTEX (Y[no3/hc], 4.9)
c	rc(19) = Mass ratio of Fe2+ produced to BTEX (Y[Fe2/hc] 21.8)
c	rc(20) = Mass ratio of Sulfate to BTEX (Y[so4/hc], 4.7)
c	rc(21) = Mass ratio of Methane produced to BTEX (Y[Me/hc], 0.78)
c	  y(1) = Hydrocarbon concentration
c	  y(2) = Oxygen concentration
c	  y(3) = Nitrate concentration
c	  y(4) = Fe2+ concentration
c	  y(5) = Sulfate concentration
c	  y(6) = Methane concentration
C*Block 1: Comments block*
c23456789012345678901234567890123456789012345678901234567890123456789012
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
C*End of Block 1*

C*    Block 2: Please do not modify this standard interface block*
      IMPLICIT NONE
      INTEGER ncol,nrow,nlay
      INTEGER ncomp,nvrxndata,j,i,k
      INTEGER First_time
      DATA First_time/1/
      DOUBLE PRECISION y,dydt,poros,rhob,reta
      DOUBLE PRECISION rc,vrc
      DIMENSION y(ncomp),dydt(ncomp),rc(50)
      DIMENSION vrc(ncol,nrow,nlay,nvrxndata),reta(ncomp)
C*    End of block 2*	
C*    Block 3: Declare your problem-specific new variables here*
c     INTEGER
      DOUBLE PRECISION excessFe,excessMe
C*    End of Block 3*

C*     Block 4: Initilize reaction parameters here, if required*
c      No parameters are initialized
C*     End of Block 4*

C*     Block 5: Definition of other variable names*
c      None defined
C*     End of Block 5 

C*     Block 6: Definition of Differential Equations*
	IF (nvrxndata .EQ. 0) THEN
c        Minor instabilities in advec, disper, or rxn calculations might
c        force the value of Fe2+ or methane value above its maximum 
c        observed level.  Check for this condition and discard the
c        instabilities at 10% tolerance level, else warn the user...
	 excessFe = rc(1)-y(4)
         IF (excessFe .LT. 0.0) THEN
	    IF (ABS(excessFe*100./rc(1)) .GT. 10.) THEN
             WRITE (*,*) "Warning...."
             WRITE (*,*) "Fe2+ is greater than maxFe at node",j,i,k
	     WRITE (*,*) "Excess amount of Fe is ", excessFe
	    END IF
	    y(4) = rc(1)
	 END IF
	 excessMe = rc(2)-y(6)
         IF (excessMe .LT. 0.0) THEN
	    IF (ABS(excessMe*100./rc(6)) .GT. 10) THEN
             WRITE (*,*) "Warning...."
             WRITE (*,*) "Methane is greater than maxMe at node",j,i,k
	     WRITE (*,*) "Excess amount of Me is ", excessMe
	    END IF
	    y(6) = rc(2)
	 END IF
         dydt(1) = ( -rc(3)*y(1)*(y(2)/(rc(8)+y(2)))  -
     &       rc(4)*y(1)*(y(3)/(rc(9)+y(3)))*(rc(13)/(rc(13)+y(2))) -
     &       rc(5)*y(1)*((rc(1)-y(4))/((rc(1)-y(4))+rc(10)))*(rc(13)/
     &       (rc(13)+y(2)))*(rc(14)/(rc(14)+y(3))) -
     &       rc(6)*y(1)*(y(5)/(rc(11)+y(5)))*(rc(13)/(rc(13)+y(2)))*
     &       (rc(14)/(rc(14)+y(3)))*(rc(15)/((rc(1)-y(4))+ rc(15))) -
     &       rc(7)*y(1)*((rc(2)-y(6))/((rc(2)-y(6))+rc(12)))*(rc(13)/
     &       (rc(13)+y(2)))*(rc(14)/(rc(14)+y(3)))*(rc(15)/
     &       ((rc(1)-y(4))+rc(15)))*(rc(16)/(rc(16)+y(5))) )/reta(1)
         dydt(2)= ( -rc(17)*rc(3)*y(1)*(y(2)/(rc(8)+y(2))) )/reta(2)
         dydt(3)= ( -rc(18)*rc(4)*y(1)*(y(3)/(rc(9)+y(3)))*
     &             (rc(13)/(rc(13)+y(2))) )/reta(3)
         dydt(4)= ( +rc(19)*rc(5)*y(1)*((rc(1)-y(4))/((rc(1)-y(4))+
     &            rc(10)))*(rc(13)/(rc(13)+y(2)))*(rc(14)/
     &            (rc(14)+y(3))) )/reta(4)
         dydt(5)= ( -rc(20)*rc(6)*y(1)*(y(5)/(rc(11)+y(5)))*(rc(13)/
     &             (rc(13)+y(2)))*(rc(14)/(rc(14)+y(3)))*(rc(15)/
     &             ((rc(1)-y(4))+ rc(15))) )/reta(5)
         dydt(6)= ( +rc(21)*rc(7)*y(1)*((rc(2)-y(6))/((rc(2)-y(6))+
     &             rc(12)))*(rc(13)/(rc(13)+y(2)))*(rc(14)/(rc(14)+
     &             y(3)))*(rc(15)/((rc(1)-y(4))+ rc(15)))*
     &             (rc(16)/(rc(16)+y(5))) )/reta(6)
	ELSE
            rc(1) = vrc(j,i,k,1)
            rc(2) = vrc(j,i,k,2)
            rc(3) = vrc(j,i,k,3)
            rc(4) = vrc(j,i,k,4) 
            rc(5) = vrc(j,i,k,5)  
            rc(6) = vrc(j,i,k,6) 
            rc(7) = vrc(j,i,k,7)
            rc(8) = vrc(j,i,k,8)
            rc(9) = vrc(j,i,k,9)
            rc(10) = vrc(j,i,k,10)
            rc(11) = vrc(j,i,k,11)
            rc(12) = vrc(j,i,k,12)
            rc(13) = vrc(j,i,k,13) 
            rc(14) = vrc(j,i,k,14)  
            rc(15) = vrc(j,i,k,15) 
            rc(16) = vrc(j,i,k,16) 
            rc(17) = vrc(j,i,k,17)
            rc(18) = vrc(j,i,k,18)
            rc(19) = vrc(j,i,k,19)
            rc(20) = vrc(j,i,k,20)
            rc(21) = vrc(j,i,k,21)
c        Minor instabilities in advec, disper, or rxn calculations might
c        force the value of Fe2+ or methane value above its maximum 
c        observed level.  Check for this condition and discard the
c        instabilities at 10% tolerance level, else warn the user...
	 excessFe = rc(1)-y(4)
         IF (excessFe .LT. 0.0) THEN
	    IF (ABS(excessFe*100./rc(1)) .GT. 10.0) THEN
             WRITE (*,*) "Warning...."
             WRITE (*,*) "Fe2+ is greater than maxFe at node",j,i,k
	     WRITE (*,*) "Excess amount of Fe is ", excessFe
	    END IF
	    y(4) = rc(1)
	 END IF
	 excessMe = rc(2)-y(6)
         IF (excessMe .LT. 0.0) THEN
	    IF (ABS(excessMe*100./rc(6)) .GT. 10.0) THEN
             WRITE (*,*) "Warning...."
             WRITE (*,*) "Methane is greater than maxMe at node",j,i,k
	     WRITE (*,*) "Excess amount of Me is ", excessMe
	    END IF
	    y(6) = rc(2)
	 END IF
         dydt(1) = ( -rc(3)*y(1)*(y(2)/(rc(8)+y(2)))  -
     &       rc(4)*y(1)*(y(3)/(rc(9)+y(3)))*(rc(13)/(rc(13)+y(2))) -
     &       rc(5)*y(1)*((rc(1)-y(4))/((rc(1)-y(4))+rc(10)))*(rc(13)/
     &       (rc(13)+y(2)))*(rc(14)/(rc(14)+y(3))) -
     &       rc(6)*y(1)*(y(5)/(rc(11)+y(5)))*(rc(13)/(rc(13)+y(2)))*
     &       (rc(14)/(rc(14)+y(3)))*(rc(15)/((rc(1)-y(4))+ rc(15))) -
     &       rc(7)*y(1)*((rc(2)-y(6))/((rc(2)-y(6))+rc(12)))*(rc(13)/
     &       (rc(13)+y(2)))*(rc(14)/(rc(14)+y(3)))*(rc(15)/
     &       ((rc(1)-y(4))+rc(15)))*(rc(16)/(rc(16)+y(5))) )/reta(1)
         dydt(2)= ( -rc(17)*rc(3)*y(1)*(y(2)/(rc(8)+y(2))) )/reta(2)
         dydt(3)= ( -rc(18)*rc(4)*y(1)*(y(3)/(rc(9)+y(3)))*
     &             (rc(13)/(rc(13)+y(2))) )/reta(3)
         dydt(4)= ( +rc(19)*rc(5)*y(1)*((rc(1)-y(4))/((rc(1)-y(4))+
     &            rc(10)))*(rc(13)/(rc(13)+y(2)))*(rc(14)/
     &            (rc(14)+y(3))) )/reta(4)
         dydt(5)= ( -rc(20)*rc(6)*y(1)*(y(5)/(rc(11)+y(5)))*(rc(13)/
     &             (rc(13)+y(2)))*(rc(14)/(rc(14)+y(3)))*(rc(15)/
     &             ((rc(1)-y(4))+ rc(15))) )/reta(5)
         dydt(6)= ( +rc(21)*rc(7)*y(1)*((rc(2)-y(6))/((rc(2)-y(6))+
     &             rc(12)))*(rc(13)/(rc(13)+y(2)))*(rc(14)/(rc(14)+
     &             y(3)))*(rc(15)/((rc(1)-y(4))+ rc(15)))*
     &             (rc(16)/(rc(16)+y(5))) )/reta(6)
        ENDIF
C*     End of Block 6 
	RETURN
	END

        SUBROUTINE jrxneqn3(ncomp,t,y,ml,mu,pd,nrowpd)

        USE ARRAY_CONFIG		
        IMPLICIT NONE
        INTEGER ncomp,nvrxndata,j,i,k,ml,mu,nrowpd
        INTEGER nlay,nrow,ncol
	DOUBLE PRECISION t,y,pd,dpporos,dprhob,dpreta
	DIMENSION y(ncomp),pd(nrowpd,ncomp)
	DOUBLE PRECISION rc
    	COMMON /rxnconst1/rc(100)
        COMMON /rxnshare/dpporos,dprhob,dpreta(100),nvrxndata,j,i,k
        COMMON /rxnshare1/nlay,nrow,ncol
	INTEGER*4 RESULT
c
c**dummy, need to build jrxn3****
c
c        CALL jrxns3(ncomp,nvrxndata,j,i,k,ml,mu,y,pd,
c     &        dpporos,dprhob,dpreta(1),rc(1),nlay,nrow,ncol,rxnarray2)
        write(*,*) "Use solver option#1, 3 or 5"
        write(*,*) "Jacobian routine is not available for module-3"
C-------EMRL JIG
        call STOPFILE_RT
C-------EMRL JIG
        stop
        RETURN
        END

C***********************Reaction Model#4****************************C
c     Two species model for predicting mass-transfer limited exchange 
c     of contaminants between sorbed-phase contaminant
c     To use this option ncomp=2, mcomp=1, nrxndata=2, and input appropriate
c     mass-transfer coefficients in rc().  The mass-transfer model assumed
c     is given in Haggerty and Gorelick, Water Resources Research, 30(2),
c     page 435-446, Design of multipel contaminant remediation: sensitivity
c     to rate-limited mass-transfer.  The model assumed is: 
c     rhob*ds/dt = eta*(c - s/kd) where "eta" is the first-order masss 
c     transfer coefficient [1/T]; kd is the distribution coefficient [L3/M];
c     s is solid-phase concentration, c is aqueous concentration, and
c     rhob is the bulk density.
C*********************************************************************C	
        SUBROUTINE rxneqn4(ncomp,t,y,dydt)
        USE ARRAY_CONFIG		
        IMPLICIT NONE
        INTEGER ncomp,nvrxndata,j,i,k
        INTEGER nlay,nrow,ncol
	  DOUBLE PRECISION t,y,dydt,dpporos,dprhob,dpreta
	  DIMENSION y(ncomp),dydt(ncomp)
	  DOUBLE PRECISION rc
    	  COMMON /rxnconst1/rc(100)
        COMMON /rxnshare/dpporos,dprhob,dpreta(100),nvrxndata,j,i,k
        COMMON /rxnshare1/nlay,nrow,ncol
        CALL rxns4(ncomp,nvrxndata,j,i,k,y,dydt,
     &        dpporos,dprhob,dpreta(1),rc(1),nlay,nrow,ncol,rxnarray2)
        RETURN
        END

 	SUBROUTINE rxns4(ncomp,nvrxndata,j,i,k,y,dydt,
     &             poros,rhob,reta,rc,nlay,nrow,ncol,vrc)
C*Block 1: Comments block*
c23456789012345678901234567890123456789012345678901234567890123456789012
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
C*End of Block 1*

C*    Block 2: Please do not modify this standard interface block*
      IMPLICIT NONE
      INTEGER ncol,nrow,nlay
      INTEGER ncomp,nvrxndata,j,i,k
      INTEGER First_time
      DATA First_time/1/
      DOUBLE PRECISION y,dydt,poros,rhob,reta
      DOUBLE PRECISION rc,vrc
      DIMENSION y(ncomp),dydt(ncomp),rc(50)
      DIMENSION vrc(ncol,nrow,nlay,nvrxndata),reta(ncomp)
C*    End of block 2*	

C*     Block 3: Declare your problem-specific new variables here*
c      INTEGER
c       DOUBLE PRECISION
C*     End of Block 3*

C*     Block 4: Initilize reaction parameters here, if required*
c       IF (First_time .EQ. 1) THEN
c         None
c         First_time = 0 !reset First_time to skip this block later
c       END IF
C*     End of Block 4*

C*     Block 5: Define other variable names, if required*
c      None 
C*     End of Block 5*

c       Definition of values in the rc(4) and y(4) arrays
c	rc(1)  = First order mass transfer coefficient "eta" [1/T]
c	rc(2)  = Distribution coefficient Kd [L3/M]
c        y(1)  = Aqueous concentration [M/L3]
c        y(2)  = Solid-phase NAPL or adsorbed contaminant concentration [M/M]  

C*     Block 6: Definition of Differential Equations*
	IF (nvrxndata .EQ. 0) THEN
         dydt(1) = -rc(1)*(y(1) - (y(2)/rc(2)))
         dydt(2) = (poros*rc(1)/rhob)*(y(1) - (y(2)/rc(2)))
	ELSE
         dydt(1) = -vrc(j,i,k,1)*(y(1) - 
     &              (y(2)/vrc(j,i,k,2)))
         dydt(2) = (poros*vrc(j,i,k,1)/rhob)*(y(1) - 
     &             (y(2)/vrc(j,i,k,2)))
	END IF
C*     End of Block 6*
       RETURN
       END

        SUBROUTINE jrxneqn4(ncomp,t,y,ml,mu,pd,nrowpd)

        USE ARRAY_CONFIG		
        IMPLICIT NONE
        INTEGER ncomp,nvrxndata,j,i,k,ml,mu,nrowpd
        INTEGER nlay,nrow,ncol
	  DOUBLE PRECISION t,y,pd,dpporos,dprhob,dpreta
	  DIMENSION y(ncomp),pd(nrowpd,ncomp)
	  DOUBLE PRECISION rc
    	  COMMON /rxnconst1/rc(100)
        COMMON /rxnshare/dpporos,dprhob,dpreta(100),nvrxndata,j,i,k
        COMMON /rxnshare1/nlay,nrow,ncol
        CALL jrxns4(ncomp,nvrxndata,j,i,k,ml,mu,y,pd,
     &        dpporos,dprhob,dpreta(1),rc(1),nlay,nrow,ncol,rxnarray2)
        RETURN
        END

 	SUBROUTINE jrxns4(ncomp,nvrxndata,j,i,k,ml,mu,y,pd,
     &             poros,rhob,reta,rc,nlay,nrow,ncol,vrc)
C*Block 1: Comments block*
c23456789012345678901234567890123456789012345678901234567890123456789012
c ncomp - Total number of components
c nvrxndata - Total number of variable reaction parameters to be input via RCT file
c J, I, K - node location (used if reaction parameters are spatially variable)
c ml, mu - are for banded jacobian (not used)
c y - Concentration value of all component at the node [array variable y(ncomp)]
c pd - jacobian matrix [ncomp x ncomp array]
c poros - porosity of the node
c reta -  Retardation factor [ignore dummy reta values of immobile species]
c rhob -  bulk density of the node
c rc - Stores spatially constant reaction parameters (can dimension upto 100 values)
c nlay, nrow, ncol - Grid size (used only for dimensioning purposes)
c vrc - Array variable that stores spatially variable reaction parameters
C*End of Block 1*

C*    Block 2: Please do not modify this standard interface block*
      IMPLICIT NONE
      INTEGER ncol,nrow,nlay
      INTEGER ncomp,nvrxndata,j,i,k,ml,mu
      INTEGER, SAVE :: First_time=1
      DOUBLE PRECISION y,pd,poros,rhob,reta
      DOUBLE PRECISION rc,vrc
      DIMENSION y(ncomp),pd(ncomp,ncomp),rc(50)
      DIMENSION vrc(ncol,nrow,nlay,nvrxndata),reta(ncomp)
C*    End of block 2*	

C*     Block 3: Declare your problem-specific new variables here*
c      INTEGER
c       DOUBLE PRECISION
C*     End of Block 3*

C*     Block 4: Initilize reaction parameters here, if required*
c       IF (First_time .EQ. 1) THEN
c         None
c         First_time = 0 !reset First_time to skip this block later
c       END IF
C*     End of Block 4*
C*     Block 5: Define other variable names, if required*
c      None 
C*     End of Block 5*

c       Definition of values in the rc(4) and y(4) arrays
c	rc(1)  = First order mass transfer coefficient "eta" [1/T]
c	rc(2)  = Distribution coefficient Kd [L3/M]
c      y(1)  = Aqueous concentration [M/L3]
c      y(2)  = Solid-phase NAPL or adsorbed contaminant concentration [M/M]  

C*     Block 6: Definition of full (n x n) analytical jacobian matrix*
	IF (nvrxndata .EQ. 0) THEN  !eqns for constant rxn parameters
         pd(1,1) = -rc(1)
	   pd(1,2) = rc(1)/rc(2)
         pd(2,1) = (poros*rc(1))/rhob
         pd(2,2) = -(poros*rc(1))/(rhob*rc(2))
	ELSE  !eqns for variable rxn parameters
         pd(1,1) = -vrc(j,i,k,1)
	   pd(1,2) = vrc(j,i,k,1)/vrc(j,i,k,2)
         pd(2,1) = (poros*vrc(j,i,k,1))/rhob
         pd(2,2) = -(poros*vrc(j,i,k,1))/(rhob*vrc(j,i,k,2))
	END IF
C*     End of Block 6*
       RETURN
       END

C*****Reaction model#5**************************************************C
c     rxneqn4 routine has reactions discussed in Clement et al's RT3D paper
c     Biomass transport and rate-limited degradation are simulated
c     set ncomp = 4 and mcomp = 3    
c*************************************************************************		   
        SUBROUTINE rxneqn5(ncomp,t,y,dydt)
        USE ARRAY_CONFIG		
        IMPLICIT NONE
        INTEGER ncomp,nvrxndata,j,i,k
        INTEGER nlay,nrow,ncol
	  DOUBLE PRECISION t,y,dydt,dpporos,dprhob,dpreta
	  DIMENSION y(ncomp),dydt(ncomp)
	  DOUBLE PRECISION rc
    	  COMMON /rxnconst1/rc(100)
        COMMON /rxnshare/dpporos,dprhob,dpreta(100),nvrxndata,j,i,k
        COMMON /rxnshare1/nlay,nrow,ncol
        CALL rxns5(ncomp,nvrxndata,j,i,k,y,dydt,
     &        dpporos,dprhob,dpreta(1),rc(1),nlay,nrow,ncol,rxnarray2)
        RETURN
        END

 	SUBROUTINE rxns5(ncomp,nvrxndata,j,i,k,y,dydt,
     &             poros,rhob,reta,rc,nlay,nrow,ncol,vrc)
C*Block 1: Comments block*
c23456789012345678901234567890123456789012345678901234567890123456789012
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
C*End of Block 1*

C*    Block 2: Please do not modify this standard interface block*
      IMPLICIT NONE
      INTEGER ncol,nrow,nlay
      INTEGER ncomp,nvrxndata,j,i,k
      INTEGER First_time
      DATA First_time/1/
      DOUBLE PRECISION y,dydt,poros,rhob,reta
      DOUBLE PRECISION rc,vrc
      DIMENSION y(ncomp),dydt(ncomp),rc(50)
      DIMENSION vrc(ncol,nrow,nlay,nvrxndata),reta(ncomp)
C*    End of block 2*	

C*     Block 3: Declare your problem-specific new variables here*
c      INTEGER
       DOUBLE PRECISION rdonora,rdonors,raccepa,racceps,rgrowa,rgrows
C*     End of Block 3*

C*     Block 4: Initilize reaction parameters here, if required*
c       IF (First_time .EQ. 1) THEN
c         None
c         First_time = 0 !reset First_time to skip this block later
c       END IF
C*     End of Block 4*

C*     Block 5: Define other variable names, if required*
c      None 
C*     End of Block 5*

c       Definition of values in the rc(8) and y(4) arrays
c	rc(1)  = Specific utilization rate, umax
c	rc(2)  = Monod half saturation constant for carbon, Kc 
c	rc(3)  = Monod half saturation constant for oxygen, Ko 
c       rc(4)  = yield (amount of biomass produced to the amount of carbon utilized)
c       rc(5)  = yield (amount of oxygen consummed to the amount of carbon utilized) 
c       rc(6)  = First-order, bacterial death or decay rate
c       rc(7)  = First-order, bacterial attachment rate [1/T]
c       rc(8)  = First-order, bacterial detachment rate [1/T]
c	 y(1)  = Elec Donor e.g. Hydrocarbon [M/L3]
c        y(2)  = Elec Acceptor e.g. Oxygen [M/L3]
c	 y(3)  = Aqueous phase, suspended bacteria [M/L3]
c        y(4)  = Attached bacteria (mass of bacteria per mass of soil [M/M])

C*     Block 6: Definition of Differential Equations*
       IF (nvrxndata .EQ. 0) THEN
c       Aqueous phase kinetic expressions (for constant rxn parameters)
        rgrowa = (rc(1)*y(3)*rc(4)*y(1)*y(2)*
     &           (1.0/(rc(2)+y(1)))*(1.0/(rc(3)+y(2))))-(rc(6)*y(3))
        rdonora = - rc(1)*y(3)*y(1)*y(2)*
     &           (1.0/(rc(2)+y(1)))*(1.0/(rc(3)+y(2)))
        raccepa = - rc(1)*rc(5)*y(3)*y(1)*y(2)*
     &           (1.0/(rc(2)+y(1)))*(1.0/(rc(3)+y(2)))
c
c       Soil phase kinetic expressions (for constant rxn parameters)
        rgrows = (rc(1)*y(4)*rc(4)*y(1)*y(2)*
     &           (1.0/(rc(2)+y(1)))*(1.0/(rc(3)+y(2))))-(rc(6)*y(4))
        rdonors = - rc(1)*y(4)*y(1)*y(2)*
     &           (1.0/(rc(2)+y(1)))*(1.0/(rc(3)+y(2)))
        racceps = - rc(1)*rc(5)*y(4)*y(1)*y(2)*
     &           (1.0/(rc(2)+y(1)))*(1.0/(rc(3)+y(2)))
c
c       Reaction kinetic expressions (for constant rxn parameters)
        dydt(1) = (rdonora + (rhob*rdonors/poros))/reta(1)
        dydt(2) = (raccepa + (rhob*racceps/poros))/reta(2)
        dydt(3) = rgrowa-rc(7)*y(3) +(rc(8)*y(4)*rhob/poros)
        dydt(4) = rgrows-rc(8)*y(4) +(rc(7)*poros*y(3)/rhob)
c
	ELSE
c
c       Aqueous phase kinetic expressions (for variable rxn parameters) 
        rgrowa = (vrc(j,i,k,1)*y(3)*vrc(j,i,k,4)*y(1)*y(2)*
     &           (1.0/(vrc(j,i,k,2)+y(1)))*(1.0/
     &           (vrc(j,i,k,3)+y(2))))-(vrc(j,i,k,6)*y(3))
        rdonora = - vrc(j,i,k,1)*y(3)*y(1)*y(2)*
     &           (1.0/(vrc(j,i,k,2)+y(1)))*(1.0/
     &           (vrc(j,i,k,3)+y(2)))
        raccepa = -vrc(j,i,k,1)*vrc(j,i,k,5)*y(3)*y(1)*y(2)*
     &           (1.0/(vrc(j,i,k,2)+y(1)))*
     &           (1.0/(vrc(j,i,k,3)+y(2)))
c
c      Soil phase kinetic expressions (for variable rxn parameters)
        rgrows = (vrc(j,i,k,1)*y(4)*vrc(j,i,k,4)*y(1)*y(2)*
     &           (1.0/(vrc(j,i,k,2)+y(1)))*(1.0/
     &           (vrc(j,i,k,3)+y(2))))-(vrc(j,i,k,6)*y(4))
        rdonors = - vrc(j,i,k,1)*y(4)*y(1)*y(2)*
     &           (1.0/(vrc(j,i,k,2)+y(1)))*(1.0/
     &           (vrc(j,i,k,3)+y(2)))
        racceps = -vrc(j,i,k,1)*vrc(j,i,k,5)*y(4)*y(1)*y(2)*
     &           (1.0/(vrc(j,i,k,2)+y(1)))*
     &           (1.0/(vrc(j,i,k,3)+y(2)))
c
c      Reaction kinetic expressions (for variable rxn parameters)
        dydt(1) = (rdonora + (rhob*rdonors/poros))/reta(1)
        dydt(2) = (raccepa + (rhob*racceps/poros))/reta(2)   
        dydt(3) = rgrowa-vrc(j,i,k,7)*y(3) +
     &            (vrc(j,i,k,8)*y(4)*rhob/poros)
        dydt(4) = rgrows-vrc(j,i,k,8)*y(4) +
     &            (vrc(j,i,k,7)*poros*y(3)/rhob)
	END IF
C*      End of Block 6*
        RETURN
        END

        SUBROUTINE jrxneqn5(ncomp,t,y,ml,mu,pd,nrowpd)

        USE ARRAY_CONFIG		
        IMPLICIT NONE
        INTEGER ncomp,nvrxndata,j,i,k,ml,mu,nrowpd
        INTEGER nlay,nrow,ncol
	DOUBLE PRECISION t,y,pd,dpporos,dprhob,dpreta
	DIMENSION y(ncomp),pd(nrowpd,ncomp)
	DOUBLE PRECISION rc
    	COMMON /rxnconst1/rc(100)
        COMMON /rxnshare/dpporos,dprhob,dpreta(100),nvrxndata,j,i,k
        COMMON /rxnshare1/nlay,nrow,ncol
	INTEGER*4 RESULT
        CALL jrxns5(ncomp,nvrxndata,j,i,k,ml,mu,y,pd,
     &        dpporos,dprhob,dpreta(1),rc(1),nlay,nrow,ncol,rxnarray2)
        RETURN
        END

c Evaluation of Jacobian for Double Monod Model

	SUBROUTINE jrxns5(ncomp,nvrxndata,j,i,k,ml,mu,y,pd,
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

C*     Block 3: Declare your problem-specific new variables here*
c      INTEGER
       DOUBLE PRECISION AA,BB,CC,BBD,CCA
C*     End of Block 3*
C*     Block 4: Initilize reaction parameters here, if required*
c       IF (First_time .EQ. 1) THEN
c         None
c         First_time = 0 !reset First_time to skip this block later
c       END IF
C*     End of Block 4*

C*     Block 5: Define other variable names, if required*
c      None 
C*     End of Block 5*

c       Definition of values in the rc(8) and y(4) arrays
c	rc(1)  = Specific utilization rate, umax
c	rc(2)  = Monod half saturation constant for carbon, Kc 
c	rc(3)  = Monod half saturation constant for oxygen, Ko 
c       rc(4)  = yield (amount of biomass produced to the amount of carbon utilized)
c       rc(5)  = yield (amount of oxygen consummed to the amount of carbon utilized) 
c       rc(6)  = First-order, bacterial death or decay rate
c       rc(7)  = First-order, bacterial attachment rate [1/T]
c       rc(8)  = First-order, bacterial detachment rate [1/T]
c	   y(1)  = Elec Donor e.g. Hydrocarbon [M/L3]
c        y(2)  = Elec Acceptor e.g. Oxygen [M/L3]
c	   y(3)  = Aqueous phase, suspended bacteria [M/L3]
c        y(4)  = Attached bacteria (mass of bacteria per mass of soil [M/M])

C*     Block 6: Definition of Differential Equations*
      IF (nvrxndata .EQ. 0) THEN
        AA = y(3)+(rhob*y(4))/poros ![X]+ rhob*Xbar/phi 
	  BB = y(1)/(rc(2)+y(1))      ![D]/Kd+[D]
	  CC = y(2)/(rc(3)+y(2))      ![A]/Ka+[A]
	  BBD = rc(2)/(rc(2)+y(1))**2  !Derivative of BB wrt [D]
	  CCA = rc(3)/(rc(3)+y(2))**2  !Derivative of CC wrt [A]

	  pd(1,1) = -rc(1)*AA*BBD*CC/reta(1)
        pd(1,2) = -rc(1)*AA*BB*CCA/reta(1)
	  PD(1,3) = -rc(1)*BB*CC/reta(1)
	  pd(1,4) = -rc(1)*rhob*BB*CC/(poros*reta(1))
          
	  PD(2,1) = - rc(1)*rc(5)*AA*BBD*CC/reta(2) 
	  PD(2,2) = - rc(1)*rc(5)*AA*BB*CCA/reta(2) 
	  PD(2,3) = - rc(1)*rc(5)*BB*CC/reta(2) 
	  PD(2,4) = - rc(1)*rc(5)*rhob*BB*CC/(poros*reta(2))
          
	  PD(3,1) = rc(1)*rc(4)*AA*BBD*CC
	  PD(3,2) = rc(1)*rc(4)*AA*BB*CCA
	  PD(3,3) = rc(1)*rc(4)*BB*CC-rc(7)-rc(6)
	  PD(3,4) = rc(1)*rc(4)*rhob*BB*CC/poros+rc(8)*rhob/poros
          
        PD(4,1) =  rc(1)*rc(4)*y(4)*BBD*CC
        PD(4,2) =  rc(1)*rc(4)*y(4)*BB*CCA
        PD(4,3) =  rc(7)*poros/rhob
        PD(4,4) =  -rc(8)+rc(1)*rc(4)*BB*CC-rc(6)
c
	ELSE
c
        AA = y(3)+(rhob*y(4))/poros               ![X]+ rhob*Xbar/phi 
	  BB = y(1)/(vrc(j,i,k,2)+y(1))             ![D]/Kd+[D]
	  CC = y(2)/(vrc(j,i,k,3)+y(2))             ![A]/Ka+[A]
	  BBD = vrc(j,i,k,2)/(vrc(j,i,k,2)+y(1))**2  !Partial Derivative of BB wrt [D]
	  CCA = vrc(j,i,k,3)/(vrc(j,i,k,3)+y(2))**2  !Partial Derivative of CC wrt [A]

	  pd(1,1) = -vrc(j,i,k,1)*AA*BBD*CC/reta(1)
        pd(1,2) = -vrc(j,i,k,1)*AA*BB*CCA/reta(1)
	  PD(1,3) = -vrc(j,i,k,1)*BB*CC/reta(1)
	  pd(1,4) = -vrc(j,i,k,1)*rhob*BB*CC/(poros*reta(1))
          
	  PD(2,1) = - vrc(j,i,k,1)*vrc(j,i,k,5)*AA*BBD*CC/reta(2) 
	  PD(2,2) = - vrc(j,i,k,1)*vrc(j,i,k,5)*AA*BB*CCA/reta(2) 
	  PD(2,3) = - vrc(j,i,k,1)*vrc(j,i,k,5)*BB*CC/reta(2) 
	  PD(2,4) = - vrc(j,i,k,1)*vrc(j,i,k,5)*rhob*BB*CC/
     &            (poros*reta(2))
          
	  PD(3,1) = vrc(j,i,k,1)*vrc(j,i,k,4)*AA*BBD*CC
	  PD(3,2) = vrc(j,i,k,1)*vrc(j,i,k,4)*AA*BB*CCA
	  PD(3,3) = vrc(j,i,k,1)*vrc(j,i,k,4)*BB*CC-vrc(j,i,k,7)-
     &            vrc(j,i,k,6)
	  PD(3,4) = vrc(j,i,k,1)*vrc(j,i,k,4)*rhob*BB*CC/poros+
     &	        vrc(j,i,k,8)*rhob/poros
          
        PD(4,1) =  vrc(j,i,k,1)*vrc(j,i,k,4)*y(4)*BBD*CC
        PD(4,2) =  vrc(j,i,k,1)*vrc(j,i,k,4)*y(4)*BB*CCA
        PD(4,3) =  vrc(j,i,k,7)*poros/rhob
        PD(4,4) =  -vrc(j,i,k,8)+vrc(j,i,k,1)*vrc(j,i,k,4)*BB*CC-
     &             vrc(j,i,k,6)

	END IF
C*      End of Block 6*

        RETURN
        END

C*****Reaction Model#6************************************************C
c     Four species (chlorinated solvents PCE -> TCE -> DEC -> VC) 
c     sequential, anaerobic, first-order, rate-limited, reductive 
c     dechlorination reactions.
c     To use this option ncomp=mcomp=4, ncrxndata=4, and input appropriate
c     first-order reaction rate rate in rc(4)
C*********************************************************************C		   
        SUBROUTINE rxneqn6(ncomp,t,y,dydt)
        USE ARRAY_CONFIG		
        IMPLICIT NONE
        INTEGER ncomp,nvrxndata,j,i,k
        INTEGER nlay,nrow,ncol
	  DOUBLE PRECISION t,y,dydt,dpporos,dprhob,dpreta
	  DIMENSION y(ncomp),dydt(ncomp)
	  DOUBLE PRECISION rc
    	  COMMON /rxnconst1/rc(100)
        COMMON /rxnshare/dpporos,dprhob,dpreta(100),nvrxndata,j,i,k
        COMMON /rxnshare1/nlay,nrow,ncol
        CALL rxns6(ncomp,nvrxndata,j,i,k,y,dydt,
     &        dpporos,dprhob,dpreta(1),rc(1),nlay,nrow,ncol,rxnarray2)
        RETURN
        END

 	SUBROUTINE rxns6(ncomp,nvrxndata,j,i,k,y,dydt,
     &             poros,rhob,reta,rc,nlay,nrow,ncol,vrc)
C*Block 1: Comments block*
c23456789012345678901234567890123456789012345678901234567890123456789012
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
C*End of Block 1*

C*    Block 2: Please do not modify this standard interface block*
      IMPLICIT NONE
      INTEGER ncol,nrow,nlay
      INTEGER ncomp,nvrxndata,j,i,k
      INTEGER First_time
      DATA First_time/1/
      DOUBLE PRECISION y,dydt,poros,rhob,reta
      DOUBLE PRECISION rc,vrc
      DIMENSION y(ncomp),dydt(ncomp),rc(50)
      DIMENSION vrc(ncol,nrow,nlay,nvrxndata),reta(ncomp)
C*    End of block 2*	

C*     Block 3: Declare your problem-specific new variables here*
c      INTEGER
c       DOUBLE PRECISION
C*     End of Block 3*

C*     Block 4: Initilize reaction parameters here, if required*
c       IF (First_time .EQ. 1) THEN
c         None
c         First_time = 0 !reset First_time to skip this block later
c       END IF
C*     End of Block 4*

C*     Block 5: Define other variable names, if required*
c      None 
C*     End of Block 5*

c      Definition of values in the rc(7) and y(4) arrays
c	rc(1)  = PCE first-order degradation rate
c	rc(2)  = TCE first-order degradation rate 
c	rc(3)  = DCE first-order degradation rate 
c	rc(4)  = VC first-order degradation rate
c       rc(5)  = yield tce/pce (131.36/165.8) -- fixed value
c       rc(6)  = yield dce/tce (96.9/131.36) -- fixed value
c       rc(7)  = yield vc/dce (62.45/96.9) -- fixed value
c        y(1)  = concentration of PCE
c        y(2)  = concentration of TCE
c        y(3)  = concentration of DCE
c        y(4)  = concentration of VC 

C*     Block 6: Definition of Differential Equations*
	IF (nvrxndata .EQ. 0) THEN  !eqns for constant rxn parameters
         dydt(1) = -rc(1)*y(1)/reta(1)
         dydt(2) = (-rc(2)*y(2) + rc(1)*y(1)*rc(5) )/reta(2)
         dydt(3) = (-rc(3)*y(3) + rc(2)*y(2)*rc(6) )/reta(3)
         dydt(4) = ( -rc(4)*y(4) + rc(3)*y(3)*rc(7) )/reta(4)
	ELSE  !eqns for variable rxn parameters
         dydt(1) = -vrc(j,i,k,1)*y(1)/reta(1)
         dydt(2) = ( -vrc(j,i,k,2)*y(2) + 
     &     vrc(j,i,k,1)*y(1)*vrc(j,i,k,5) )/reta(2)
         dydt(3) = ( -vrc(j,i,k,3)*y(3) + 
     &     vrc(j,i,k,2)*y(2)*vrc(j,i,k,6) )/reta(3)
         dydt(4) = ( -vrc(j,i,k,4)*y(4) + 
     &     vrc(j,i,k,3)*y(3)*vrc(j,i,k,7) )/reta(4)
	END IF
C*     End of Block 6*
       RETURN
       END

        SUBROUTINE jrxneqn6(ncomp,t,y,ml,mu,pd,nrowpd)
        USE ARRAY_CONFIG		
        IMPLICIT NONE
        INTEGER ncomp,nvrxndata,j,i,k,ml,mu,nrowpd
        INTEGER nlay,nrow,ncol
	DOUBLE PRECISION t,y,pd,dpporos,dprhob,dpreta
	DIMENSION y(ncomp),pd(nrowpd,ncomp)
	DOUBLE PRECISION rc
    	COMMON /rxnconst1/rc(100)
        COMMON /rxnshare/dpporos,dprhob,dpreta(100),nvrxndata,j,i,k
        COMMON /rxnshare1/nlay,nrow,ncol
        CALL jrxns6(ncomp,nvrxndata,j,i,k,ml,mu,y,pd,
     &        dpporos,dprhob,dpreta(1),rc(1),nlay,nrow,ncol,rxnarray2)
        RETURN
        END

 	SUBROUTINE jrxns6(ncomp,nvrxndata,j,i,k,ml,mu,y,pd,
     &             poros,rhob,reta,rc,nlay,nrow,ncol,vrc)
C*Block 1: Comments block*
c23456789012345678901234567890123456789012345678901234567890123456789012
c ncomp - Total number of components
c nvrxndata - Total number of variable reaction parameters to be input via RCT file
c J, I, K - node location (used if reaction parameters are spatially variable)
c ml, mu - are for banded jacobian (not used)
c y - Concentration value of all component at the node [array variable y(ncomp)]
c pd - jacobian matrix [ncomp x ncomp array]
c poros - porosity of the node
c reta -  Retardation factor [ignore dummy reta values of immobile species]
c rhob -  bulk density of the node
c rc - Stores spatially constant reaction parameters (can dimension upto 100 values)
c nlay, nrow, ncol - Grid size (used only for dimensioning purposes)
c vrc - Array variable that stores spatially variable reaction parameters
C*End of Block 1*

C*    Block 2: Please do not modify this standard interface block*
      IMPLICIT NONE
      INTEGER ncol,nrow,nlay
      INTEGER ncomp,nvrxndata,j,i,k,ml,mu
      INTEGER, SAVE :: First_time=1
      DOUBLE PRECISION y,pd,poros,rhob,reta
      DOUBLE PRECISION rc,vrc
      DIMENSION y(ncomp),pd(ncomp,ncomp),rc(50)
      DIMENSION vrc(ncol,nrow,nlay,nvrxndata),reta(ncomp)
C*    End of block 2*	

C*     Block 3: Declare your problem-specific new variables here*
c      INTEGER
c       DOUBLE PRECISION
C*     End of Block 3*

C*     Block 4: Initilize reaction parameters here, if required*
c       IF (First_time .EQ. 1) THEN
c         None
c         First_time = 0 !reset First_time to skip this block later
c       END IF
C*     End of Block 4*
C*     Block 5: Define other variable names, if required*
c      None 
C*     End of Block 5*

c      Definition of values in the rc(7) and y(4) arrays
c	rc(1)  = PCE first-order degradation rate
c	rc(2)  = TCE first-order degradation rate 
c	rc(3)  = DCE first-order degradation rate 
c	rc(4)  = VC first-order degradation rate
c       rc(5)  = yield tce/pce (131.36/165.8) -- fixed value
c       rc(6)  = yield dce/tce (96.9/131.36) -- fixed value
c       rc(7)  = yield vc/dce (62.45/96.9) -- fixed value
c        y(1)  = concentration of PCE
c        y(2)  = concentration of TCE
c        y(3)  = concentration of DCE
c        y(4)  = concentration of VC 
C*     Block 6: Definition of full (n x n) analytical jacobian matrix*
	IF (nvrxndata .EQ. 0) THEN  !eqns for constant rxn parameters
         pd(1,1) = -rc(1)/reta(1)
	 pd(1,2) = 0.0d0
	 pd(1,3) = 0.0d0
	 pd(1,4) = 0.0d0
         pd(2,1) = rc(1)*rc(5)/reta(2)
         pd(2,2) = -rc(2)/reta(2)
	 pd(2,3) = 0.0d0
	 pd(2,4) = 0.0d0
         pd(3,1) = 0.0d0
         pd(3,2) = rc(2)*rc(6)/reta(3)
	 pd(3,3) = -rc(3)/reta(3)
	 pd(3,4) = 0.0d0
         pd(4,1) = 0.0d0
         pd(4,2) = 0.0d0
	 pd(4,3) = rc(3)*rc(7)/reta(4)
	 pd(4,4) = -rc(4)/reta(4)
	ELSE  !eqns for variable rxn parameters
         pd(1,1) = -vrc(j,i,k,1)/reta(1)
	 pd(1,2) = 0.0d0
	 pd(1,3) = 0.0d0
	 pd(1,4) = 0.0d0
         pd(2,1) = vrc(j,i,k,1)*vrc(j,i,k,5)/reta(2)
         pd(2,2) = -vrc(j,i,k,2)/reta(2)
	 pd(2,3) = 0.0d0
	 pd(2,4) = 0.0d0
         pd(3,1) = vrc(j,i,k,2)*vrc(j,i,k,6)/reta(3)
         pd(3,2) = -vrc(j,i,k,3)/reta(3)
	 pd(3,3) = 0.0d0
	 pd(3,4) = 0.0d0
         pd(4,1) = vrc(j,i,k,3)*vrc(j,i,k,7)/reta(4)
         pd(4,2) = -vrc(j,i,k,4)/reta(4)
	 pd(4,3) = 0.0d0
	 pd(4,4) = 0.0d0
	END IF
C*     End of Block 6*
       RETURN
       END


C*****Reaction Model#7****************************************************C
c     Six species (chlorinated solvents PCE, TCE, DCE, VC, ETH, and Cl
C*************************************************************************C
        SUBROUTINE rxneqn7(ncomp,t,y,dydt)
        USE ARRAY_CONFIG		
        IMPLICIT NONE
        INTEGER ncomp,nvrxndata,j,i,k
        INTEGER nlay,nrow,ncol
	DOUBLE PRECISION t,y,dydt,dpporos,dprhob,dpreta
	DIMENSION y(ncomp),dydt(ncomp)
	DOUBLE PRECISION rc
    	COMMON /rxnconst1/rc(100)
        COMMON /rxnshare/dpporos,dprhob,dpreta(100),nvrxndata,j,i,k
        COMMON /rxnshare1/nlay,nrow,ncol
        CALL rxns7(ncomp,nvrxndata,j,i,k,y,dydt,
     &        dpporos,dprhob,dpreta(1),rc(1),nlay,nrow,ncol,rxnarray2)
        RETURN
        END

 	SUBROUTINE rxns7(ncomp,nvrxndata,j,i,k,y,dydt,
     &             poros,rhob,reta,rc,nlay,nrow,ncol,vrc)
C*Block 1: Comments block*
c23456789012345678901234567890123456789012345678901234567890123456789012
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
C*End of Block 1*

C*    Block 2: Please do not modify this standard interface block*
      IMPLICIT NONE
      INTEGER ncol,nrow,nlay
      INTEGER ncomp,nvrxndata,j,i,k
      INTEGER First_time
      DATA First_time/1/
      DOUBLE PRECISION y,dydt,poros,rhob,reta
      DOUBLE PRECISION rc,vrc
      DIMENSION y(ncomp),dydt(ncomp),rc(50)
      DIMENSION vrc(ncol,nrow,nlay,nvrxndata),reta(ncomp)
C*    End of block 2*	

C*     Block 3: Declare your problem-specific new variables here*
c      INTEGER
       DOUBLE PRECISION mwcl,mwpce,mwtce,mwdce,mwvc,mweth,ytp,ydt,
     &         yvd,yev,kp,kt1,kt2,kd1,kd2,kv1,kv2,ke1,ke2
C*     End of Block 3*

C*     Block 4: Initilize reaction parameters here, if required*
	 IF (First_time .EQ. 1) THEN
            mwcl = 35.453d0
            mwpce = 165.8d0
            mwtce  = 131.389d0		! g/mol
            mwdce  = 96.944d0		! g/mol
            mwvc   = 62.499d0		! g/mol
            mweth  = 28.054d0		! g/mol
            ytp = mwtce/mwpce
            ydt = mwdce/mwtce
            yvd = mwvc/mwdce
            yev = mweth/mwvc
	    IF (nvrxndata .EQ. 0) THEN
               kp = rc(1)
               kt1 = rc(2)
               kt2 = rc(3)
               kd1 = rc(4) 
               kd2 = rc(5)  
               kv1 = rc(6) 
               kv2 = rc(7)
               ke1 = rc(8)
               ke2 = rc(9)
	    END IF
            First_time = 0 !reset First_time to skip this section
	 END IF
C*     End of Block 4*

C*     Block 5: Define other variable names, if required*
c      None 
C*     End of Block 5*

c       Definition of values in the rc() and y() arrays
c	rc(1)  = PCE anaerobic degradation rate
c	rc(2)  = TCE anaerobic degradation rate 
c	rc(3)  = TCE aerobic degradation rate 
c	rc(4)  = DCE anaerobic degradation rate 
c       rc(5)  = DCE Aerobic degradation rate
c       rc(6)  = VC anaerobic degradation rate
c       rc(7)  = VC aerobic degradation rate
c       rc(8)  = ETH anaerobic degradation rate
c       rc(9)  = ETH aerobic degradation rate
c        y(1)  = concentration of PCE
c        y(2)  = concentration of TCE
c        y(3)  = concentration of DCE
c        y(4)  = concentration of VC 
c        y(5)  = Concentration of ETH
c        y(6)  = concentration of Cl

C*     Block 6: Definition of Differential Equations*
       IF (nvrxndata .GT. 0) THEN
          kp = vrc(j,i,k,1)
          kt1 = vrc(j,i,k,2)
          kt2 = vrc(j,i,k,3)
          kd1 = vrc(j,i,k,4) 
          kd2 = vrc(j,i,k,5)  
          kv1 = vrc(j,i,k,6) 
          kv2 = vrc(j,i,k,7)
          ke1 = vrc(j,i,k,8)
          ke2 = vrc(j,i,k,9)
      END IF
      dydt(1) = -kp*y(1)/reta(1)
      dydt(2) = (ytp*kp*y(1)-kt1*y(2)-kt2*y(2))/reta(2)
      dydt(3) = (ydt*kt1*y(2) -kd1*y(3) -kd2*y(3))/reta(3)
      dydt(4) = (yvd*kd1*y(3) -kv1*y(4) -kv2*y(4))/reta(4)
      dydt(5) = (yev*kv1*y(4) - ke1*y(5) - ke2*y(5))/reta(5)
      dydt(6) = (kp*y(1)*(mwcl/mwpce)+kt1*y(2)*(mwcl/mwtce)+
     &          kt2*y(2)*3*(mwcl/mwtce)+kd1*y(3)*(mwcl/mwdce)+
     &          kd2*y(3)*2*(mwcl/mwdce)+(kv1+kv2)*y(4)*
     &          (mwcl/mwvc))/1.0
C*     End of Block 6*
        RETURN
        END

        SUBROUTINE jrxneqn7(ncomp,t,y,ml,mu,pd,nrowpd)

        USE ARRAY_CONFIG		
        IMPLICIT NONE
        INTEGER ncomp,nvrxndata,j,i,k,ml,mu,nrowpd
        INTEGER nlay,nrow,ncol
	DOUBLE PRECISION t,y,pd,dpporos,dprhob,dpreta
	DIMENSION y(ncomp),pd(nrowpd,ncomp)
	DOUBLE PRECISION rc
    	COMMON /rxnconst1/rc(100)
        COMMON /rxnshare/dpporos,dprhob,dpreta(100),nvrxndata,j,i,k
        COMMON /rxnshare1/nlay,nrow,ncol
	INTEGER*4 RESULT
c
c **need to build jrxns7
c
c        CALL jrxns7(ncomp,nvrxndata,j,i,k,ml,mu,y,pd,
c     &        dpporos,dprhob,dpreta(1),rc(1),nlay,nrow,ncol,rxnarray2)
        write(*,*) "Use solver option#1, 3 or 5"
        write(*,*) "Jacobian routine is not available for module-7"
C-------EMRL JIG
        call STOPFILE_RT
C-------EMRL JIG
        stop
        RETURN
        END
C
c-------------------------------------------------------
c     rxneqn8 dummy routine for single species first-order decay
c-------------------------------------------------------
        SUBROUTINE rxneqn8(ncomp,t,y,dydt)
        USE ARRAY_CONFIG		
        IMPLICIT NONE
        INTEGER ncomp,nvrxndata,j,i,k
        INTEGER nlay,nrow,ncol
	DOUBLE PRECISION t,y,dydt,dpporos,dprhob,dpreta
	DIMENSION y(ncomp),dydt(ncomp)
	DOUBLE PRECISION rc
    	COMMON /rxnconst1/rc(100)
        COMMON /rxnshare/dpporos,dprhob,dpreta(100),nvrxndata,j,i,k
        COMMON /rxnshare1/nlay,nrow,ncol
        CALL rxns8(ncomp,nvrxndata,j,i,k,y,dydt,
     &        dpporos,dprhob,dpreta(1),rc(1),nlay,nrow,ncol,rxnarray2)
        RETURN
        END

 	SUBROUTINE rxns8(ncomp,nvrxndata,j,i,k,y,dydt,
     &             poros,rhob,reta,rc,nlay,nrow,ncol,vrc)
C*Block 1: Comments block*
c23456789012345678901234567890123456789012345678901234567890123456789012
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
C*End of Block 1*

C*    Block 2: Please do not modify this standard interface block*
      IMPLICIT NONE
      INTEGER ncol,nrow,nlay
      INTEGER ncomp,nvrxndata,j,i,k
      INTEGER First_time
      DATA First_time/1/
      DOUBLE PRECISION y,dydt,poros,rhob,reta
      DOUBLE PRECISION rc,vrc
      DIMENSION y(ncomp),dydt(ncomp),rc(50)
      DIMENSION vrc(ncol,nrow,nlay,nvrxndata),reta(ncomp)
C*    End of block 2*	

C*     Block 3: Declare your problem-specific new variables here*
c      INTEGER
       DOUBLE PRECISION k1
C*     End of Block 3*

C*     Block 4: Initilize reaction parameters here, if required*
c       IF (First_time .EQ. 1) THEN
c         None
c         First_time = 0 !reset First_time to skip this block later
c       END IF
C*     End of Block 4*

C*     Block 5: Define other variable names, if required*
c      None 
C*     End of Block 5*

C*     Block 6: Definition of Differential Equations*
         k1 = 0.1
         dydt(1) = -k1*y(1)/reta(1)
C*     End of Block 6*	
 
       RETURN
       END

        SUBROUTINE jrxneqn8(ncomp,t,y,ml,mu,pd,nrowpd)

        USE ARRAY_CONFIG		
        IMPLICIT NONE
        INTEGER ncomp,nvrxndata,j,i,k,ml,mu,nrowpd
        INTEGER nlay,nrow,ncol
	DOUBLE PRECISION t,y,pd,dpporos,dprhob,dpreta
	DIMENSION y(ncomp),pd(nrowpd,ncomp)
	DOUBLE PRECISION rc
    	COMMON /rxnconst1/rc(100)
        COMMON /rxnshare/dpporos,dprhob,dpreta(100),nvrxndata,j,i,k
        COMMON /rxnshare1/nlay,nrow,ncol
	INTEGER*4 RESULT
c
c **need to build jrxns8
c
c        CALL jrxns8(ncomp,nvrxndata,j,i,k,ml,mu,y,pd,
c     &        dpporos,dprhob,dpreta(1),rc(1),nlay,nrow,ncol,rxnarray2)
        write(*,*) "Use solver option#1, 3 or 5"
        write(*,*) "Jacobian routine is not available for module-8"
C-------EMRL JIG
        call STOPFILE_RT
C-------EMRL JIG
        stop
        RETURN
        END

c-------------------------------------------------------
c     rxneqn9 dummy routine for single species first-order decay
c-------------------------------------------------------
        SUBROUTINE rxneqn9(ncomp,t,y,dydt)
        USE ARRAY_CONFIG		
        IMPLICIT NONE
        INTEGER ncomp,nvrxndata,j,i,k
        INTEGER nlay,nrow,ncol
	DOUBLE PRECISION t,y,dydt,dpporos,dprhob,dpreta
	DIMENSION y(ncomp),dydt(ncomp)
	DOUBLE PRECISION rc
    	COMMON /rxnconst1/rc(100)
        COMMON /rxnshare/dpporos,dprhob,dpreta(100),nvrxndata,j,i,k
        COMMON /rxnshare1/nlay,nrow,ncol
        CALL rxns9(ncomp,nvrxndata,j,i,k,y,dydt,
     &        dpporos,dprhob,dpreta(1),rc(1),nlay,nrow,ncol,rxnarray2)
        RETURN
        END

 	SUBROUTINE rxns9(ncomp,nvrxndata,j,i,k,y,dydt,
     &             poros,rhob,reta,rc,nlay,nrow,ncol,vrc)
C*Block 1: Comments block*
c23456789012345678901234567890123456789012345678901234567890123456789012
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
C*End of Block 1*

C*    Block 2: Please do not modify this standard interface block*
      IMPLICIT NONE
      INTEGER ncol,nrow,nlay
      INTEGER ncomp,nvrxndata,j,i,k
      INTEGER First_time
      DATA First_time/1/
      DOUBLE PRECISION y,dydt,poros,rhob,reta
      DOUBLE PRECISION rc,vrc
      DIMENSION y(ncomp),dydt(ncomp),rc(50)
      DIMENSION vrc(ncol,nrow,nlay,nvrxndata),reta(ncomp)
C*    End of block 2*	

C*     Block 3: Declare your problem-specific new variables here*
c      INTEGER
       DOUBLE PRECISION k1
C*     End of Block 3*

C*     Block 4: Initilize reaction parameters here, if required*
c       IF (First_time .EQ. 1) THEN
c         None
c         First_time = 0 !reset First_time to skip this block later
c       END IF
C*     End of Block 4*

C*     Block 5: Define other variable names, if required*
c      None 
C*     End of Block 5*

C*     Block 6: Definition of Differential Equations*
         k1 = 0.1
         dydt(1) = -k1*y(1)/reta(1)
C*     End of Block 6*	
 
       RETURN
       END

        SUBROUTINE jrxneqn9(ncomp,t,y,ml,mu,pd,nrowpd)

        USE ARRAY_CONFIG		
        IMPLICIT NONE
        INTEGER ncomp,nvrxndata,j,i,k,ml,mu,nrowpd
        INTEGER nlay,nrow,ncol
	DOUBLE PRECISION t,y,pd,dpporos,dprhob,dpreta
	DIMENSION y(ncomp),pd(nrowpd,ncomp)
	DOUBLE PRECISION rc
    	COMMON /rxnconst1/rc(100)
        COMMON /rxnshare/dpporos,dprhob,dpreta(100),nvrxndata,j,i,k
        COMMON /rxnshare1/nlay,nrow,ncol
	INTEGER*4 RESULT
c
c **need to build jrxns4
c
c        CALL jrxns9(ncomp,nvrxndata,j,i,k,ml,mu,y,pd,
c     &        dpporos,dprhob,dpreta(1),rc(1),nlay,nrow,ncol,rxnarray2)
        write(*,*) "Use solver option#1, 3 or 5"
        write(*,*) "Jacobian routine is not available for module-9"
C-------EMRL JIG
        call STOPFILE_RT
C-------EMRL JIG
        stop
        RETURN
        END
C
c-------------------------------------------------------
c     rxneqn10: Place any general purpose user-defined rxn routine here
c     Remember to set appropriate ncomp and mcomp values
c     Example below is identical to rxneqn6, but coded as a DLL
c-------------------------------------------------------	   
        SUBROUTINE rxneqn10(ncomp,t,y,dydt)
        USE ARRAY_CONFIG		
        IMPLICIT NONE
        INTEGER ncomp,nvrxndata,j,i,k
        INTEGER nlay,nrow,ncol
	DOUBLE PRECISION t,y,dydt,dpporos,dprhob,dpreta
	DIMENSION y(ncomp),dydt(ncomp)
	DOUBLE PRECISION rc
    	COMMON /rxnconst1/rc(100)
        COMMON /rxnshare/dpporos,dprhob,dpreta(100),nvrxndata,j,i,k
        COMMON /rxnshare1/nlay,nrow,ncol
        CALL rxns(ncomp,nvrxndata,j,i,k,y,dydt,
     &        dpporos,dprhob,dpreta(1),rc(1),nlay,nrow,ncol,rxnarray2)
        RETURN
        END

        SUBROUTINE jrxneqn10(ncomp,t,y,ml,mu,pd,nrowpd)
        USE ARRAY_CONFIG		
        IMPLICIT NONE
        INTEGER ncomp,nvrxndata,j,i,k,ml,mu,nrowpd
        INTEGER nlay,nrow,ncol
	DOUBLE PRECISION t,y,pd,dpporos,dprhob,dpreta
	DIMENSION y(ncomp),pd(nrowpd,ncomp)
	DOUBLE PRECISION rc
    	COMMON /rxnconst1/rc(100)
        COMMON /rxnshare/dpporos,dprhob,dpreta(100),nvrxndata,j,i,k
        COMMON /rxnshare1/nlay,nrow,ncol
        CALL jrxns(ncomp,nvrxndata,j,i,k,ml,mu,y,pd,
     &        dpporos,dprhob,dpreta(1),rc(1),nlay,nrow,ncol,rxnarray2)
        RETURN
        END
