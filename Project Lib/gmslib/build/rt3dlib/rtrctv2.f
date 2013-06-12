      SUBROUTINE RCTRTAL(INRCT,IOUT,ISUM,ISUM2,NCOL,NROW,NLAY,ncomp,
     &mcomp,ISOTHM,IREACT,isolver,ncrxndata,nvrxndata,irctop,LCRHOB,
     &LCSP1,LCSP2,LCRC1,LCRC2)
C **********************************************************************
C THIS SUBROUTINE ALLOCATES SPACE FOR ARRAYS NEEDED BY THE CHEMICAL 
C REACTION (RCT) PACKAGE.
C **********************************************************************
C last modified: 10/12/98
C
      IMPLICIT  NONE
      INTEGER   INRCT,IOUT,ISUM,ISUM2,NCOL,NROW,NLAY,ISOTHM,IREACT,
     &          ncrxndata,nvrxndata,LCRHOB,LCSP1,LCSP2,LCRC1,LCRC2,
     &          NODES,ISUMX,ISUMIX,ISOLD,ISOLD2,isolver,ncomp,mcomp,
     &          irctop,ierr

C
C--PRINT PACKAGE NAME AND VERSION NUMBER
      WRITE(IOUT,1000) INRCT
 1000 FORMAT(1X,'RCTRT -- RT3D CHEMICAL REACTIONS PACKAGE,',
     & ' VER 2.0, OCT 1998, INPUT READ FROM UNIT',I3)
C
C Read the first line of RCT data and check for version 1 or 2 compatability
       READ(INRCT,'(6I10)',ERR=100,IOSTAT=IERR)
     & ISOTHM,IREACT,ncrxndata,nvrxndata,isolver,irctop
  100 IF(IERR.NE.0) THEN
        irctop=1
        WRITE(*,110)
        BACKSPACE (INRCT)
        READ(INRCT,'(5I10)') ISOTHM,IREACT,ncrxndata,nvrxndata,isolver
        irctop=1
        WRITE(*,110)
        PAUSE
      ENDIF
  110 FORMAT(/1X,'WARNING: INPUT FILE FOR VER 1 OF [RCT] PACKAGE',
     & ' DETECTED;'/1X,'SORPTION CONSTANTS SHOULD BE ASSIGNED ONE',
     & ' VALUE PER LAYER'/)

      IF(ISOTHM.EQ.1) THEN
        WRITE(IOUT,1022)
      ELSEIF(ISOTHM.EQ.2) THEN
        WRITE(IOUT,1024)
      ELSEIF(ISOTHM.EQ.3) THEN
        WRITE(IOUT,1026)
      ELSE
        WRITE(IOUT,1028)
      ENDIF
      IF(IREACT.EQ.0) THEN
        WRITE(IOUT,1030)
      ELSE
        WRITE(IOUT,1032)
      ENDIF
 1022 FORMAT(1X,'TYPE OF SORPTION SELECTED IS [LINEAR]')
 1024 FORMAT(1X,'TYPE OF SORPTION SELECTED IS [FREUNDLICH]')
 1026 FORMAT(1X,'TYPE OF SORPTION SELECTED IS [LANGMUIR]')
 1028 FORMAT(1X,'NO SORPTION ISOTHERM IS SIMULATED')
 1030 FORMAT(1X,'NO REACTION IS SIMULATED')
 1032 FORMAT(1X,'RATE-LIMITED REACTION',
     & ' [DECAY OR BIODEGRADATION] IS SIMULATED')
C
C--ALLOCATE SPACE FOR ARRAYS
      ISOLD=ISUM
      ISOLD2=ISUM2
      NODES=NCOL*NROW*NLAY
C
C--REAL ARRAYS
      LCRHOB=ISUM
C      IF(ISOTHM.NE.0) ISUM=ISUM+NODES !in RT3D always read rhok
      ISUM=ISUM+NODES
      LCSP1=ISUM
      IF(ISOTHM.NE.0) ISUM=ISUM+NODES * mcomp
      LCSP2=ISUM
      IF(ISOTHM.NE.0) ISUM=ISUM+NODES * mcomp
      LCRC1=ISUM
      IF(IREACT.NE.0) ISUM=ISUM +1  !Allocate one location for holding dummy decay rates
      LCRC2=ISUM
      IF(IREACT.NE.0) ISUM=ISUM +1
C
C--CHECK WHETHER ARRAYS X AND IX ARE DIMENSIONED LARGE ENOUGH
      ISUMX=ISUM-ISOLD
      ISUMIX=ISUM2-ISOLD2
      WRITE(IOUT,1090) ISUMX,ISUMIX
 1090 FORMAT(1X,I8,' ELEMENTS OF THE  X ARRAY USED BY THE RCT PACKAGE'
     & /1X,I8,' ELEMENTS OF THE IX ARRAY USED BY THE RCT PACKAGE'/)
C
C--NORMAL RETURN
      RETURN
      END
C
C
      SUBROUTINE RCTRTRP(IN,IOUT,NCOL,NROW,NLAY,ncomp,mcomp,ICBUND,COLD,
     & PRSITY,ISOTHM,IREACT,isolver,ncrxndata,nvrxndata,irctop,RHOB,
     & SP1,SP2,RC1,RC2,RETA,RFMIN,IFMTRF,DTRCT)
C *******************************************************************
C THIS SUBROUTINE READS AND PREPARES INPUT DATA NEEDED BY
C THE CHEMICAL REACTION (RCT) PACKAGE.
C********************************************************************
C last modified: 10/12/98
C

      USE ARRAY_CONFIG
      IMPLICIT  NONE
      INTEGER   IN,IOUT,NCOL,NROW,NLAY,ICBUND,ISOTHM,IREACT,isolver,
     &  ncrxndata,nvrxndata,IFMTRF,I,j,K,ncomp,mcomp,index,ierr,irctop
      REAL      COLD,PRSITY,RHOB,SP1,SP2,RC1,RC2,RETA,RFMIN,DTRCT,
     &          TR,buff,dummysp1,dummysp2,dummyrhob

      CHARACTER ANAME*24
      DIMENSION ICBUND(NCOL,NROW,NLAY),COLD(NCOL,NROW,NLAY,ncomp),
     &          PRSITY(NCOL,NROW,NLAY),RETA(NCOL,NROW,NLAY,ncomp),
     &          RHOB(NCOL,NROW,NLAY),
     &          SP1(NCOL,NROW,NLAY,NCOMP),SP2(NCOL,NROW,NLAY,NCOMP)
      DIMENSION dummySP1(NLAY,MCOMP),dummySP2(NLAY,MCOMP),
     &          dummyrhob(NLAY,1)
      double precision rxnarray1,atol,rtol, temp1,temp2
      COMMON /rxnconst1/rxnarray1(100)
      COMMON /solver_consts/ atol(100),rtol(100)
	INTEGER*4 RESULT

c** RC1 and RC2 are not used (only dummy values)
	RC1 = 0.0
	RC2 = 0.0
c** DTRCT is not calculated in RT3D; a dummy value is assigned
	DTRCT=9999.0

c*     Initialize R =1 for all the species
	DO index = 1, ncomp 
          DO K=1,NLAY
            DO I=1,NROW
              DO J=1,NCOL
                RETA(J,I,K,INDEX)=1.0
              ENDDO
            ENDDO
          ENDDO
	ENDDO
c
C--PRINT A HEADER
      WRITE(IOUT,1000)
 1000 FORMAT(//1X,'SORPTION AND 1ST ORDER RATE REACTION PARAMETERS',
     & /1X,47('-')/)

c     Reading bulk density values
      ANAME='BULK DENSITY (RHOB)    '
      IF (irctop .LE. 1) THEN  !ver-1 data, read layer-by-layer
        CALL RARRAY(dummyrhob(1,1),ANAME,1,NLAY,0,IN,IOUT)
c       Transfer bulk density into 3-D arrays
	DO index = 1, mcomp 
          DO K=1,NLAY
            DO I=1,NROW
              DO J=1,NCOL
                RHOB(J,I,K) = dummyrhob(K,1)
              ENDDO
            ENDDO
          ENDDO
	ENDDO
       ELSE
        DO K=1,NLAY
          CALL RARRAY(RHOB(1,1,K),ANAME,NROW,NCOL,K,IN,IOUT)
        ENDDO
       ENDIF
C
C--CALL RARRAY TO READ IN SORPTION PARAMETERS
      IF(ISOTHM.LE.0) GOTO 1 !no sorption, hence skip reading sorption constants
      IF (irctop .LE. 1) THEN  !ver-1 data, read layer-by-layer
        ANAME='1ST SORP. COEF. COMP. NO'
        do index=1,mcomp
          write(aname(22:24),'(i3.2)') index
          CALL RARRAY(dummySP1(1,index),ANAME,1,NLAY,0,IN,IOUT)
        enddo
        ANAME='2ND SORP. COEF. COMP. NO'
        do index=1,mcomp
          write(aname(22:24),'(i3.2)') index
          CALL RARRAY(dummySP2(1,index),ANAME,1,NLAY,0,IN,IOUT)
        enddo
c       Transfer sorption parameters into 3-D sorption arrays
	DO index = 1, mcomp 
          DO K=1,NLAY
            DO I=1,NROW
              DO J=1,NCOL
                SP1(J,I,K,INDEX) = dummySP1(K,INDEX)
              ENDDO
            ENDDO
          ENDDO
	ENDDO
	DO index = 1, mcomp 
          DO K=1,NLAY
            DO I=1,NROW
              DO J=1,NCOL
                SP2(J,I,K,INDEX) = dummySP2(K,INDEX)
              ENDDO
            ENDDO
          ENDDO
	ENDDO
      ELSE 
        DO INDEX=1,mcomp
          ANAME='1ST SORP. COEF. COMP. NO'
          WRITE(ANAME(22:24),'(I3.2)') INDEX
          DO K=1,NLAY
           CALL RARRAY(SP1(1,1,K,INDEX),ANAME,NROW,NCOL,K,IN,IOUT)
          ENDDO
        ENDDO
        DO INDEX=1,mcomp
          ANAME='2ND SORP. COEF. COMP. NO'
          WRITE(ANAME(22:24),'(I3.2)') INDEX
          DO K=1,NLAY
            CALL RARRAY(SP2(1,1,K,INDEX),ANAME,NROW,NCOL,K,IN,IOUT)
          ENDDO
        ENDDO
      ENDIF

C--CALL RFCOEF TO CALCULATE RETARDATION FACTOR [RETA] for all mobile components
        rfmin = 1.0e30
        DO index=1, mcomp
           CALL RFCOEF(NCOL,NROW,NLAY,ICBUND,PRSITY,COLD(1,1,1,index),
     &     RETA(1,1,1,index),RFMIN,RHOB,SP1(1,1,1,index),
     &     SP2(1,1,1,index),ISOTHM)
        ENDDO
1	CONTINUE
           IF (isolver == 0) THEN
              CONTINUE 
           ELSEIF ((isolver >= 1 ) .AND. (isolver <= 5 ))THEN
              DO i = 1, ncomp
                 read (IN,*) atol(i),rtol(i)
              END DO
	     ELSE
              Write (*,*) "Unrecognizable input value for ISOLVER"
C-------EMRL JIG
              call STOPFILE_RT
C-------EMRL JIG
              STOP
           ENDIF

           Do i = 1, ncrxndata
              read (IN,*) rxnarray1(i)
           End do
           ALLOCATE(rxnarray2(ncol,nrow,nlay,nvrxndata),STAT=ierr)
           if (ierr.NE.0) then
             write(*,*) 'Not enough memory for variable rxn const array'
C-------EMRL JIG
             call STOPFILE_RT
C-------EMRL JIG
             stop
           end if
           ANAME = 'Rxn Constants'
           DO i = 1, nvrxndata
              DO K = 1, NLAY
                 CALL DPRARRAY(rxnarray2(1,1,K,i),ANAME,NROW,NCOL,K,
     $                         IN,IOUT)
              END DO
           ENDDO 	 
        IF (IREACT.LE.0) GOTO 2 
	   IF (IREACT.EQ.2) THEN !swap maxfe & maxme values into lower locations
	      temp1 = rxnarray1(1)
	      temp2 = rxnarray1(2)
              rxnarray1(1)=rxnarray1(3)
              rxnarray1(2)=rxnarray1(4)
              rxnarray1(3)=rxnarray1(5)
              rxnarray1(4)=rxnarray1(6)
              rxnarray1(5)=rxnarray1(7)
              rxnarray1(6) = temp1
              rxnarray1(7) = temp2
c	   ELSEIF (IREACT.EQ.3) THEN
c****Include any input manipulations for other rxn packages here***
	      CONTINUE
	    END IF     
c
2       CONTINUE
C
C--PRINT OUT RETARDATION FACTOR IF REQUESTED
  100 IF(IFMTRF.EQ.0) GOTO 500

      DO index=1,ncomp
        ANAME='RETARD. FACTOR: COMP. NO'
        write(aname(22:24),'(i3.2)') index
       DO K=1,NLAY
        CALL RPRINT(RETA(1,1,K,index),ANAME,0,1,1,NCOL,NROW,
     &   K,IFMTRF,IOUT)
       END DO
      END DO

  500 RETURN
      END
C
      SUBROUTINE RCTRTSV(NCOL,NROW,NLAY,ncomp,mcomp,ICBUND,PRSITY,DELR,
     & DELC,DH,reta,RFMIN,DTRANS,ISOTHM,kinetics,isolver,nvrxndata,
     & RHOB,SP1,SP2,RC1,RC2,cnew,cold,RMASIO)
C *******************************************************************
C THIS SUBROUTINE CALCULATES THE CHANGES IN CELL CONCENTRATIONS DUE
C TO CHEMICAL REACTIONS--ONLY SORPTION AND FIRST-ORDER DECAY (OR
C BIODEGRADATION) ARE CONSIDERED IN THE CURRENT VERSION.
C *******************************************************************
C last modified: 10/12/98
C

      USE ARRAY_CONFIG
      IMPLICIT  NONE
      INTEGER*4 NCOL,NROW,NLAY,ICBUND,ISOTHM,IREACT,K,I,J,lrw,liw,
     $          nvrxndata,nvrxndata_common,kinetics,isolver,itemp,
     $          ncomp,mcomp,index
      INTEGER nlay1,ncol1,nrow1
      REAL*4    PRSITY,reta,RFMIN,DTRANS,RHOB,SP1,SP2,RC1,RC2,
     &          cnew,cold,DC,TMASIO,RMASIO,DCRCT,DELR,DELC,DH,
     $          conc_old,conc_new,maxfe2,maxmethane,fe2old,
     $          methaneold,RMASSIO
      CHARACTER Err_message*70
      DOUBLE PRECISION dpconc_old,dpdtrans,dpporos,dprhob,
     $                 dpreta,rxnarray1
      DIMENSION ICBUND(NCOL,NROW,NLAY,ncomp),PRSITY(NCOL,NROW,NLAY),
     &          reta(NCOL,NROW,NLAY,ncomp),RHOB(ncol,nrow,nlay),
     &          SP1(ncol,nrow,nlay,mcomp),SP2(ncol,nrow,nlay,mcomp),
     &          cnew(NCOL,NROW,NLAY,ncomp),
     &          cold(NCOL,NROW,NLAY,ncomp),DC(NCOL,NROW,NLAY,ncomp),
     &          DELR(NCOL),DELC(NROW),DH(NCOL,NROW,NLAY)
      DIMENSION conc_old(ncomp),conc_new(ncomp),dpconc_old(ncomp),
     &          RMASIO(20,2,ncomp)
      COMMON /rxnconst1/rxnarray1(100)
      COMMON /rxnshare/dpporos,dprhob,dpreta(100),nvrxndata_common,j,i,k
      COMMON /rxnshare1/nlay1,nrow1,ncol1
      EXTERNAL rxneqn1,rxneqn2,rxneqn3,rxneqn4,rxneqn5,rxneqn6,
     &         rxneqn7,rxneqn8,rxneqn9,rxneqn10
      EXTERNAL jrxneqn3,jrxneqn4,jrxneqn5,jrxneqn6,
     &         jrxneqn7,jrxneqn8,jrxneqn9,jrxneqn10
	INTEGER*4 RESULT
      Err_message="Chk inputs or reduce PERCEL or try diff adv solver"
c     since dummy argument nvrxndata,nlay,ncol,nrow cannot be passed 
c     via common block (a Fortran restriction) we define the following
c     new variables to use it in the common block
      nvrxndata_common = nvrxndata 
      nlay1 = nlay
      ncol1 = ncol
      nrow1 = nrow 
c                     
C--UPDATE RETARDATION FACTOR IF NON-LINEAR SORPTION IS SIMULATED
      IF(ISOTHM.EQ.2.OR.ISOTHM.EQ.3) THEN
         DO index=1, mcomp
          CALL RFCOEF(NCOL,NROW,NLAY,ICBUND,PRSITY,COLD(1,1,1,index),
     &     RETA(1,1,1,index),RFMIN,RHOB,SP1(1,1,1,index),
     &     SP2(1,1,1,index),ISOTHM)
          ENDDO
      ENDIF
C     
      DO 110 k=1,nlay
      DO 115 i=1,nrow
      DO 120 j=1,ncol          
         DO index = 1, ncomp 
	    IF (icbund(j,i,k,index) .eq. 0) goto 120  !skip rxn calculations at inactive nodes
c           Check and get rid of small negative numbers if any(just a precaution)
            IF (cnew(j,i,k,index) .LT. 0.0) THEN
               cnew(j,i,k,index) = 0.0
             END IF
             conc_old(index) = cnew(j,i,k,index)
         END DO

         IF (kinetics .GE. 3) THEN
           DO index = 1, ncomp
              dpconc_old(index) = DBLE(conc_old(index))
           END DO
	   dpdtrans = DBLE(dtrans)
           dpporos = DBLE(prsity(j,i,k))
           DO index = 1, mcomp
              dpreta(index) = DBLE(reta(j,i,k,index))
           END DO
           dprhob = DBLE(rhob(j,i,k))
           lrw=22+ncomp*MAX(16, ncomp+9)
           liw=20+ncomp
         END IF
      SELECT CASE (kinetics)
C*****Kinetics#0*******************************************************
C     ****Tracer transport, therefore no reaction call is required***
      CASE (0)
           DO index = 1, ncomp
              conc_new(index) = conc_old(index)
           END DO

C*****Kinetics#1*******************************************************
C     Two-species instantaneous reactions (BIOPLUME-II type reactions)***
      CASE (1)
        CALL rxneqn1(ncomp,conc_old,conc_new,reta(j,i,k,1))

C*****Kinetics#2*******************************************************
C     Six species sequetial instantaneous BTEX degradation***
      CASE (2)
        IF (nvrxndata .GT. 0) THEN  !this option will never be used.
          maxfe2 = SNGL(rxnarray2(j,i,k,1))   
          maxmethane = SNGL(rxnarray2(j,i,k,2))
	ELSE
          maxfe2 = SNGL(rxnarray1(6)) !Note that after reading inputs we moved
          maxmethane = SNGL(rxnarray1(7)) !these two values to locations 6 & 7 
	END IF 
        fe2old = conc_old(4) !storing fe2old and methold for later use
        methaneold = conc_old(6)
        conc_old(4) = maxfe2 - fe2old !Computing bioavailable Fe3+ and storing in conc_old
        conc_old(6) = maxmethane - methaneold !computing available methanogenesis potential
        CALL rxneqn2(ncomp,conc_old,conc_new,reta(j,i,k,1))
        conc_old(4) = fe2old !trasfering back the old values into conc_old for mass balance
        conc_old(6) = methaneold
        conc_new(4) = maxfe2 - conc_new(4) !Computing new Fe2+
        conc_new(6) = maxmethane - conc_new(6) !Computing new methane

c    In all kinetic routines dpconc_old(ncomp) goes into the Rxnsolver 
c    as initial condition and comes out as new concentrations after dtrans 
C*****Kinetics#3*******************************************************
C     Six species, first-order, rate-limited, BTEX sequential degradation
      CASE (3)
        IF ( (isolver==1). OR. (isolver==2) ) THEN
          CALL Rxnsolver1(ncomp,isolver,dpdtrans,dpconc_old,lrw,liw,
     &                    rxneqn3,jrxneqn3)
        ELSEIF (isolver==3) THEN
           CALL Rxnsolver2(ncomp,dpdtrans,dpconc_old,rxneqn3)
	  ELSE
           CALL Rxnsolver3(ncomp,isolver,dpdtrans,dpconc_old,
     &                     rxneqn3,jrxneqn3)
        END IF
C*****Kinetics#4*******************************************************
c     Sorption Model
      CASE (4)
        IF ( (isolver==1). OR. (isolver==2) ) THEN
          CALL Rxnsolver1(ncomp,isolver,dpdtrans,dpconc_old,lrw,liw,
     &                    rxneqn4,jrxneqn4)
        ELSEIF (isolver==3) THEN
           CALL Rxnsolver2(ncomp,dpdtrans,dpconc_old,rxneqn4)
	  ELSE
           CALL Rxnsolver3(ncomp,isolver,dpdtrans,dpconc_old,
     &                     rxneqn4,jrxneqn4)
        END IF
C*****Kinetics#5*******************************************************
c     Double Monod Model	 
      CASE (5)

        IF ( (isolver==1). OR. (isolver==2) ) THEN
          CALL Rxnsolver1(ncomp,isolver,dpdtrans,dpconc_old,lrw,liw,
     &                    rxneqn5,jrxneqn5)
        ELSEIF (isolver==3) THEN
           CALL Rxnsolver2(ncomp,dpdtrans,dpconc_old,rxneqn5)
	  ELSE
           CALL Rxnsolver3(ncomp,isolver,dpdtrans,dpconc_old,
     &                     rxneqn5,jrxneqn5)
        END IF

c*****Kinetics#6*******************************************************
c     Sequential First order Decay
      CASE (6)
        IF ( (isolver==1). OR. (isolver==2) ) THEN
          CALL Rxnsolver1(ncomp,isolver,dpdtrans,dpconc_old,lrw,liw,
     &                    rxneqn6,jrxneqn6)
        ELSEIF (isolver==3) THEN
           CALL Rxnsolver2(ncomp,dpdtrans,dpconc_old,rxneqn6)
	  ELSE
           CALL Rxnsolver3(ncomp,isolver,dpdtrans,dpconc_old,
     &                     rxneqn6,jrxneqn6)
        END IF
c*****Kinetics#7*******************************************************
C     PCE/TCE aerobic and anaerobic
      CASE (7)

        IF ( (isolver==1). OR. (isolver==2) ) THEN
          CALL Rxnsolver1(ncomp,isolver,dpdtrans,dpconc_old,lrw,liw,
     &                    rxneqn7,jrxneqn7)
        ELSEIF (isolver==3) THEN
           CALL Rxnsolver2(ncomp,dpdtrans,dpconc_old,rxneqn7)
	  ELSE
           CALL Rxnsolver3(ncomp,isolver,dpdtrans,dpconc_old,
     &                     rxneqn7,jrxneqn7)
        END IF
c*****Kinetics#8*******************************************************
c     Open...
      CASE (8)
        write (*,*) "Rxn package#8 is under construction.."
C-------EMRL JIG
        call STOPFILE_RT
C-------EMRL JIG
        stop

        IF ( (isolver==1). OR. (isolver==2) ) THEN
          CALL Rxnsolver1(ncomp,isolver,dpdtrans,dpconc_old,lrw,liw,
     &                    rxneqn8,jrxneqn8)
        ELSEIF (isolver==3) THEN
           CALL Rxnsolver2(ncomp,dpdtrans,dpconc_old,rxneqn8)
	  ELSE
           CALL Rxnsolver3(ncomp,isolver,dpdtrans,dpconc_old,
     &                     rxneqn8,jrxneqn8)
        END IF
c*****Kinetics#9*******************************************************
c     Open....
      CASE (9)
        write (*,*) "Rxn package#9 is under construction.."
C-------EMRL JIG
        call STOPFILE_RT
C-------EMRL JIG
        stop

        IF ( (isolver==1). OR. (isolver==2) ) THEN
          CALL Rxnsolver1(ncomp,isolver,dpdtrans,dpconc_old,lrw,liw,
     &                    rxneqn9,jrxneqn9)
        ELSEIF (isolver==3) THEN
           CALL Rxnsolver2(ncomp,dpdtrans,dpconc_old,rxneqn9)
	  ELSE
           CALL Rxnsolver3(ncomp,isolver,dpdtrans,dpconc_old,
     &                     rxneqn9,jrxneqn9)
        END IF
c*****Kinetics#10*******************************************************
c     USER-DEFINED kinetics 
      CASE (10)
        IF ( (isolver==1). OR. (isolver==2) ) THEN
          CALL Rxnsolver1(ncomp,isolver,dpdtrans,dpconc_old,lrw,liw,
     &                    rxneqn10,jrxneqn10)
        ELSEIF (isolver==3) THEN
           CALL Rxnsolver2(ncomp,dpdtrans,dpconc_old,rxneqn10)
	  ELSE
           CALL Rxnsolver3(ncomp,isolver,dpdtrans,dpconc_old,
     &                     rxneqn10,jrxneqn10)
        END IF
c************************************************************************
      CASE DEFAULT
        write (*,*) kinetics
        write (*,*) "Illegal Reaction package number detected..."
C-------EMRL JIG
        call STOPFILE_RT
C-------EMRL JIG
        stop
c
      END SELECT !end of kinetics selection
c
       IF (kinetics .GE. 3) THEN
         DO index = 1, ncomp
	   conc_new(index) = SNGL(dpconc_old(index))
           IF (conc_new(index) .LT. 0.0) THEN
              conc_new(index) = 0.0
	   END IF
         END DO
       END IF

          DO index = 1, ncomp
c             IF (index .LE. mcomp) THEN
	       IF (icbund(j,i,k,index) .lt. 0) conc_new(index)
     $                                    = conc_old(index)  !const conc species
c             END IF
Cbecause new version has icbund upto ncomp and potentially one can fix solid
Cphase concentration via the ssm package, but RT3Dv2.0 will not allow this option)
             DCRCT = conc_new(index)-conc_old(index)
             cnew(j,i,k,index) = conc_new(index)
C             DC(J,I,K,index)=DC(J,I,K,index)+DCRCT
c(because new version does not require lcdc)
             IF(DCRCT.LT.0) THEN
                 RMASIO(9,2,index)=RMASIO(9,2,index)+DCRCT*
     $           RETA(J,I,K,index)*DELR(J)*DELC(I)*
     $           DH(J,I,K)*PRSITY(J,I,K)
                ELSE
                 RMASIO(9,1,index)=RMASIO(9,1,index)+DCRCT*
     $           RETA(J,I,K,index)*DELR(J)*DELC(I)*
     $           DH(J,I,K)*PRSITY(J,I,K)
              ENDIF
           END DO
  120 CONTINUE
  115 CONTINUE
  110 CONTINUE

200   RETURN
      END
C
c
c2345678911234567892123456789312345678941234567895123456789612345678971
c---------------------------------------------------------------------------
c Solving reactions using non-stiff/ stiff (uses Gear's method) diff eqn solver
c---------------------------------------------------------------------------
      SUBROUTINE Rxnsolver1(neq,isolver,tout,y,lrw,liw,fex,jac)

      IMPLICIT NONE
      external fex,jac
      INTEGER istate_counter,isolver
      INTEGER ic,neq,itol,itask,istate,iopt,lrw,liw,jt,iout,iwork
      double precision atol, rtol, rwork, t, tout, y
      dimension y(neq),rwork(lrw),iwork(liw)
      COMMON /solver_consts/ atol(100),rtol(100)
	INTEGER*4 RESULT
c********rxn const block**********************
c itol   = an indicator for the type of error control.  see
c          description below under atol.  used only for input.
c
c rtol   = a relative error tolerance parameter, either a scalar or
c          an array of length neq.  see description below under atol.
c          input only.
c
c atol   = an absolute error tolerance parameter, either a scalar or
c          an array of length neq.  input only.
c
c             the input parameters itol, rtol, and atol determine
c          the error control performed by the solver.  the solver will
c          control the vector e = (e(i)) of estimated local errors
c          in y, according to an inequality of the form
c                      max-norm of ( e(i)/ewt(i) )   .le.   1,
c          where ewt = (ewt(i)) is a vector of positive error weights.
c          the values of rtol and atol should all be non-negative.
c          the following table gives the types (scalar/array) of
c          rtol and atol, and the corresponding form of ewt(i).
c
c             itol    rtol       atol          ewt(i)
c              1     scalar     scalar     rtol*abs(y(i)) + atol
c              2     scalar     array      rtol*abs(y(i)) + atol(i)
c              3     array      scalar     rtol(i)*abs(y(i)) + atol
c              4     array      array      rtol(i)*abs(y(i)) + atol(i)
c
c          when either of these parameters is a scalar, it need not
c          be dimensioned in the user-s calling program.
c
c          if none of the above choices (with itol, rtol, and atol
c          fixed throughout the problem) is suitable, more general
c          error controls can be obtained by substituting a
c          user-supplied routine for the setting of ewt.
c          see part iv below.
c
c          if global errors are to be estimated by making a repeated
c          run on the same problem with smaller tolerances, then all
c          components of rtol and atol (i.e. of ewt) should be scaled
c          down uniformly.
      itol = 4
c      DO ic=1,neq
c          WRITE (*,*) ic, atol(ic),rtol(ic)
c         rtol(ic) = 1.0d-9
c         atol(ic) = 1.0d-10
c      END DO
c      PAUSE
c      iwork(6) = 500
      itask = 1
      istate = 1
      iopt = 0
      SELECT CASE (isolver)
        CASE (1)
          jt = 2
        CASE (2)
          jt = 1
        CASE DEFAULT
          write (*,*) "Illegal entry into LSODE routine..."
          write (*,*) "Report this bug to the author"
C-------EMRL JIG
          call STOPFILE_RT
C-------EMRL JIG
          stop
      END SELECT
      t = 0.0d0
      istate_counter = 1
       call solver(fex,neq,y,t,tout,itol,rtol(1),atol(1),itask,istate,
     $     iopt,rwork,lrw,iwork,liw,jac,jt)
10      CONTINUE

        IF (istate .lt. 0) THEN
c             write (*,*) istate, istate_counter, t, tout 
c              pause
            IF (istate .EQ. -1) THEN
              IF (istate_counter.LE.10) THEN
                  istate_counter = istate_counter+1
                  istate = 2
                  call solver(fex,neq,y,t,tout,itol,rtol(1),
     $                 atol(1),itask,istate,
     $                 iopt,rwork,lrw,iwork,liw,jac,jt)
                   GOTO 10
               ELSE 
                   GOTO 12
               END IF
             ENDIF
12           CONTINUE
c             write(*,90) istate, istate_counter
             write(*,*) 'Reaction solver failed....'
c	     write (*,*) 'Use Batchrxn to test and debug your rxn eqns'
             if (istate .EQ. -1) then
      write (*,*) 'Excessive amount of work done while solving rxn eqn' 
      write (*,*) 'Try reducing the time steps'
             elseif (istate .EQ. -2) then
      write (*,*) 'Too much accuracy is requested' 
      write (*,*) 'Reset appropriate tolerence levels'
             elseif (istate .EQ. -3) then
      write (*,*) 'Illegal input detected'
             elseif (istate .EQ. -4) then
      write (*,*) 'Problem may have a singularity'
      write (*,*) 'Chk input values and rxn eqns'
             elseif (istate .EQ. -5) then
      write (*,*) 'Unable to converge'
      write (*,*) 'Rxn eqns may not be differentiable'
             elseif (istate .EQ. -6) then
      write (*,*) 'Too small or zero atol value'
      write (*,*) 'Increase atol values'
             elseif (istate .EQ. -7) then
      write (*,*) 'Improper dimensioning of work arrays'
      write (*,*) 'Please Report this bug to the author'
             end if  
90           format(/// " error halt.. istate, istate_counter =",2I4)
             write(*,*) 'Run tracer simulations to check the transport'
	     write(*,*) 'Check the values of rxn kinetic constants'
C-------EMRL JIG
           call STOPFILE_RT
C-------EMRL JIG
	     STOP
	ENDIF         
        RETURN
	END
C
C
      SUBROUTINE RFCOEF(NCOL,NROW,NLAY,ICBUND,PRSITY,COLD,RETA,RFMIN,
     & RHOB,SP1,SP2,ISOTHM)
C ********************************************************************
C THIS SUBROUTINE EVALUATES RETARDATION FACTOR FOR THREE TYPES OF
C SORPTION ISOTHERMS: LINEAR, FREUNDLICH AND LANGMUIR.
C ********************************************************************
C last modified: 14-Oct-1990
C
      IMPLICIT  NONE
      INTEGER   NCOL,NROW,NLAY,ICBUND,ISOTHM,J,I,K
      REAL      PRSITY,COLD,RETA,RFMIN,RHOB,SP1,SP2
      DIMENSION PRSITY(NCOL,NROW,NLAY),ICBUND(NCOL,NROW,NLAY),
     &          COLD(NCOL,NROW,NLAY),RETA(NCOL,NROW,NLAY),
     &          RHOB(NCOL,NROW,NLAY),SP1(NCOL,NROW,NLAY),
     $          SP2(NCOL,NROW,NLAY)
C
C--INITIALIZE.
c      RFMIN= 1.E30
C
C--EVALUATE RETARDATION FACTOR
C--DEPENDING ON TYPES OF SORPTION SELECTED
      IF(ISOTHM.EQ.1) THEN
        DO 10 K=1,NLAY
          DO 20 I=1,NROW
            DO 30 J=1,NCOL
              IF(ICBUND(J,I,K).EQ.0) GOTO 30
              RETA(J,I,K)=1.+RHOB(J,I,K)/PRSITY(J,I,K)*SP1(J,I,K)
              RFMIN=MIN(RFMIN,RETA(J,I,K))
   30       CONTINUE
   20     CONTINUE
   10   CONTINUE
      ELSEIF(ISOTHM.EQ.2) THEN
        DO 40 K=1,NLAY
          DO 50 I=1,NROW
            DO 60 J=1,NCOL
              IF(ICBUND(J,I,K).EQ.0) GOTO 60
              IF(COLD(J,I,K).LE.0) THEN
                RETA(J,I,K)=1.
              ELSE
                RETA(J,I,K)=1.+RHOB(J,I,K)/PRSITY(J,I,K)*
     &          SP1(J,I,K)*SP2(J,I,K)*COLD(J,I,K)**(SP2(J,I,K)-1.)
              ENDIF
              RFMIN=MIN(RFMIN,RETA(J,I,K))
   60       CONTINUE
   50     CONTINUE
   40   CONTINUE
      ELSEIF(ISOTHM.EQ.3) THEN
        DO 70 K=1,NLAY
          DO 80 I=1,NROW
            DO 90 J=1,NCOL
              IF(ICBUND(J,I,K).EQ.0) GOTO 90
              RETA(J,I,K)=1.+RHOB(J,I,K)/PRSITY(J,I,K)*
     &        SP1(J,I,K)*SP2(J,I,K)/(1.+SP1(J,I,K)*COLD(J,I,K))**2
              RFMIN=MIN(RFMIN,RETA(J,I,K))
   90       CONTINUE
   80     CONTINUE
   70   CONTINUE
      ENDIF
C
C--RETURN
      RETURN
      END

C
      SUBROUTINE Rxnsolver2(neqn,tout,y,fex)
      IMPLICIT NONE
      external fex
      INTEGER nok,nbad
      INTEGER neqn,iwork,iflag
      double precision t,tout,y,atol,rtol,work,dum1,dum2
      COMMON /solver_consts/ atol(100),rtol(100)
      DIMENSION y(neqn),work(3+6*neqn),iwork(5)
       t = 0.0d0
       iflag = 1
       call rkf45(fex,neqn,y(1),t,tout,rtol(1),atol(1),iflag,work(1),
     &            iwork(1))
         IF (iflag .NE. 2) THEN
 	   write (*,*) "Reaction solver failed...." 
           write (*,*) "Runge-Kutta solver failure code = ",iflag
           write (*,*) "(Refer to RT3Dv2 user manual for details)"
	   STOP
	END IF
      RETURN
      END

      SUBROUTINE Rxnsolver3(neq,isolver,tout,y,fex,jac)
      IMPLICIT NONE
      external fex,jac
      INTEGER nok,nbad
      INTEGER neq,isolver
      double precision eps,t,tout,y,dum,atol,rtol
      COMMON /solver_consts/ atol(100),rtol(100)
      dimension y(neq) 
      
	dum = 0.0
c***need to automate eps**********TO BE FIXED
      eps = atol(1) !1.0d-6  
      call dodeint(y,neq,isolver,dum,tout,eps,tout,dum,nok,nbad,fex,
     &             jac)
      RETURN
      END
