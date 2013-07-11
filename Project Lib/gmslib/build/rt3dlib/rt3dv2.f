C#########################Patched by shengquan Yan 11/25/01########################
C  Replace all "SETPATH" by "SETPATH_RT", "STOPFILE" by "STOPFILE_RT", "GETPATH" by "GETPATH_RT"
C#########################Patch finished###########################################
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++                                                                   %
C                          R T 3 D  v2.0a                              +
C           A Three-Dimensional Numerical Model for Simulation of      +
C              Multi-species Reactive Transport of Contaminants        +
C                   in saturated groundwater systems                   +
C                         (Version 2.0a Beta)                               +                                                                   +								       +
C	RT3D (version 2) was developed by: T. Prabhakar Clement        +
C       The MT3DMS components were developed                           +
c       by Dr. Zheng and Wang at the University of Alabama.            +
C       Unauthorized reproduction or distribution of this              +
C	computer code or any portion of it, is not permitted.	       +
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C For Technical Information about RT3D, Contact: T. Prabhakar Clement  
C Department of Environmental Engineering, University of Western Australia     
C Nedlands, WA 6009; Australia, E-mail: clement@cwr.uwa.edu.au           
C World-Wide-Web: http://www.cwr.uwa.edu.au
C RT3D homepage: http://bioprocess.pnl.gov/rt3d.htm
C RT3D (version-2) is based on RT3D (version-1) which was orginally 
C developed by T. Prabhakar Clement at Battelle Pacific Northwest National
C Laboratory.  Funding for RT3D (version-2) development was provided, in part, 
C by the Idaho National Engineering Laboratory, through a Department of Energy 
C research project grant.  The current version of RT3D is from MT3DMS 
C (DoD_3.50.A) developed by Chunmiao Zheng and P. Patrick Wang at the 
C Univ. of Alabama.  In addition, RT3D also uses pubic domain NETLIB solvers.                                         
C For Technical Information about MT3DMS Contact: Chunmiao Zheng     
C Department of Geological Sciences, University of Alabama     
C Tuscaloosa, AL 35487; E-mail: czheng@ua.edu           
C World-Wide-Web: http://hydro.geo.ua.edu             
C MT3DMS is based on MT3D originally developed by Chunmiao Zheng
C at S.S. Papadopulos & Associates, Inc. and documented for
C the United States Environmental Protection Agency.
C MT3DMS is written by Chunmiao Zheng and P. Patrick Wang
C with the iterative solver routine by Tsun-Zee Mai.
C Funding for MT3DMS development is provided, in part, by
C U.S. Army Corps of Engineers Waterways Experiment Station.
C MT3DMS Copyright, 1998-99, The University of Alabama. All rights reserved.
C
C This program is provided without any warranty.
C No author or distributor accepts any responsibility
C to anyone for the consequences of using it
C or for whether it serves any particular purpose.
c 
      MODULE ARRAY_CONFIG
      DOUBLE PRECISION, ALLOCATABLE, SAVE :: rxnarray2(:,:,:,:)
      END MODULE ARRAY_CONFIG

      MODULE SSMDECAY_ARRAY
      REAL, ALLOCATABLE, SAVE,DIMENSION(:,:,:,:) :: ssmcdecay
      END MODULE SSMDECAY_ARRAY
C
C=======================================================================
C last modified: 06-23-98
C
C--SET MAXIMUM ARRAY DIMENSIONS AND FORTRAN UNIT NUMBERS FOR I/O FILES.
C--LENX AND LENIX: MAXIMUM DIMENSION OF STORAGE ARRAYS X AND IX;
C--MXPRS:  MAXIMUM NUMBER OF TIMES AT WHICH RESULTS ARE SAVED;
C--MXSTP:  MAXIMUM NUMBER OF TIME STEPS IN FLOW MODEL;
C--MXOBS:  MAXIMUM NUMBER OF OBSERVATION POINTS;
C--MXCOMP: MAXMUM NUMBER OF COMPONENTS.
C  =================================================================
C
C-------EMRL JIG
C      USE DFLIB   
C      USE DFPORT  
C-------EMRL JIG
C#########################Patched by shengquan Yan 11/25/01########################
	subroutine Rt3dEntry(strSuperFile)
C#########################Patch finished###########################################
      IMPLICIT  NONE
C#########################Patched by shengquan Yan 11/25/01########################
	character(*), intent(in):: strSuperFile
C#########################Patch finished###########################################
      INTEGER   LENX,LENIX,MXPRS,MXSTP,MXOBS,MXCOMP,INBTN,INADV,INDSP,
     &          INSSM,INRCT,INGCG,INUHF,IOUT,IOBS,IUCN,IMAS,ICNF,ICBM,
     &          ISUP,IDSS,ISPC,INUMSPC,I1,I2,I3
      PARAMETER (LENX=999999999,LENIX=999999999)
      PARAMETER (MXPRS=01000,MXSTP=01000,MXOBS=01000,MXCOMP=100)
      PARAMETER (INBTN=1,INADV=2,INDSP=3,INSSM=4,INRCT=8,INGCG=9,
     &          INUHF=10)
      PARAMETER (IOUT=16,ICNF=17,IUCN=200,IOBS=400,IMAS=600,ICBM=800)
      PARAMETER (ISUP=21,IDSS=22,ISPC=23) 
C
C      INTEGER   i,j,k -----------------------modified by shengquan yan.
	INTEGER	  J,K
      INTEGER   IX,ISUMX,ISUMIX,ISUM,ISUM2,NCOL,NROW,NLAY,NCOMP,MCOMP,
     &          LCLAYC,LCDELR,LCDELC,LCDZ,LCPR,LCXBC,LCYBC,LCZBC,LCQX,
     &          LCQY,LCQZ,LCDH,LCIB,LCCOLD,LCCNEW,LCCADV,LCRETA,LCBUFF,
     &          MIXELM,MXPART,LCXP,LCYP,LCZP,LCCNPT,LCCHEK,
     &          NCOUNT,NPINS,NRC,LCAL,LCTRPT,LCTRPV,LCDM,LCDXX,LCDXY,
     &          LCDXZ,LCDYX,LCDYY,LCDYZ,LCDZX,LCDZY,LCDZZ,LCSSMC,
     &          LCIRCH,LCRECH,LCCRCH,LCIEVT,LCEVTR,LCCEVT,MXSS,LCSS,
     &          ISOTHM,IREACT,isolver,ncrxndata,nvrxndata,
     &          LCRHOB,LCSP1,LCSP2,LCRC1,LCRC2,INTERP,
     &          ISEED,ITRACK,NPL,NPH,NPMIN,NPMAX,NPLANE,NLSINK,NPSINK,
     &          NPRS,NOBS,LOCOBS,NSS,KSTP,KPER,NTSS,I,N,NPS,
     &          IFMTCN,IFMTNP,IFMTRF,IFMTDP,MXSTRN,
     &          NPER,NSTP,ISTAT,LCQSTO,LCHTOP,LCCWGT,LCSR,
     &          LCINDX,LCINDY,LCINDZ,ISS,IVER,NPROBS,NPRMAS,IRCTOP,
     &          MXITER,IPRGCG,NADVFD,ITP,NODES,ICNVG,ITER1,ITO,
     &          ISOLVE,LCA,LCQ,LCWK,LCCNCG,LCLRCH,LCRHS,
     &          IMPSOL,NCRS,ISPD,IGETSC,L,INDEX,ICOMP,NPERFL,IERR
      REAL      X,TIMPRS,TSLNGH,PERCEL,HORIGN,XMAX,YMAX,ZMAX,CINACT,
     &          TMASIO,RMASIO,DCEPS,SRMULT,WD,DCHMOC,HT1,HT2,TIME1,
     &          TIME2,DT0,DELT,DTRACK,DTDISP,DTRANS,THKMIN,
     &          DTSSM,DTRCT,DTRACK2,RFMIN,TMASS,ACCL,CCLOSE,
     &          TTSMULT,TTSMAX,TMASIN,TMASOT,ERROR,ERROR2
      LOGICAL   TRNOP(10),UNIDX,UNIDY,UNIDZ,SAVUCN,SAVCBM,CHKMAS,
     &          FWEL,FDRN,FRCH,FEVT,FRIV,FGHB,PRTOUT,UPDLHS
      CHARACTER FLTYPE*4,FLNAME*80,FINDEX*30,TUNIT*4,LUNIT*4,MUNIT*4,
     &          FPRT*1,PATH*80,SPECIES(20)*80,CONCFILE*80,CONCNAME*80
	CHARACTER PREFIX*80
      DIMENSION X(:),IX(:),
     &          TIMPRS(MXPRS),TSLNGH(MXSTP),LOCOBS(3,MXOBS),
     &          NCOUNT(MXCOMP),NPINS(MXCOMP),NRC(MXCOMP),
     &          TMASIO(20,2,MXCOMP),RMASIO(20,2,MXCOMP),TMASS(4,MXCOMP),
     &          TMASIN(MXCOMP),TMASOT(MXCOMP),ERROR(MXCOMP),
     &          ERROR2(MXCOMP)
      ALLOCATABLE :: X,IX
      COMMON   /PD/HORIGN,XMAX,YMAX,ZMAX,UNIDX,UNIDY,UNIDZ
      COMMON   /FC/FWEL,FDRN,FRCH,FEVT,FRIV,FGHB
      COMMON   /OC/IFMTCN,IFMTNP,IFMTRF,IFMTDP,SAVUCN,
     &             SAVCBM,CHKMAS,NPRMAS
      COMMON   /AD/PERCEL,ITRACK,WD,ISEED,DCEPS,NPLANE,NPL,NPH,
     &             NPMIN,NPMAX,SRMULT,INTERP,NLSINK,NPSINK,DCHMOC
      COMMON   /GCGIDX/L(19)
      COMMON   /EMRL/PATH,PREFIX
      INTEGER*4 NUMARGS,RESULT,LEN
      CHARACTER*80 ARGV

C-------EMRL JIG
C      TYPE (qwinfo) winfo 
C-------EMRL JIG
 
 
C--WRITE AN IDENTIFIER TO SCREEN
C-------EMRL JIG
c      winfo.TYPE = QWIN$RESTORE
c      RESULT = SETWSIZEQQ(QWIN$FRAMEWINDOW, winfo)
C-------EMRL JIG

#ifdef _DEBUG
	!!DEC$ IF DEFINED(_DEBUG) obsolete
      WRITE(*,101)
	!!DEC$ ENDIF
#endif
  101 FORMAT(1x,'RT3D v2.0 -Three Dimensional Reactive Transport Model',
     & ' [Release 2.0a beta]',
     & /1x,'Developed by: Dr. Prabhakar Clement'/)
C
C--OPEN STANDARD OUTPUT AND BASIC INPUT FILES
C     FINDEX='Standard Output File: '
C     ISTAT=0
C     CALL OPENFL(IOUT,ISTAT,FLNAME,0,FINDEX)
C
C     FINDEX='Basic Transport Input File: '
C     ISTAT=1
C     CALL OPENFL(INBTN,ISTAT,FLNAME,0,FINDEX)
C
C****ECGL ADDTIONS

      ARGV=' '
C      NUMARGS = IARGC()  
      IF (NUMARGS.EQ.1) THEN
        CALL GETARG(2,ARGV)
        IF (ARGV(1:1).EQ.' ') CALL GETARG(1,ARGV)
      ENDIF

      FLNAME = ARGV
C#########################Patched by shengquan Yan 11/25/01########################
	FLNAME = strSuperFile
C#########################Patch finished###########################################
      IF (FLNAME(1:1).EQ.' ') THEN
600     WRITE(*,*) ' Enter the name of the RT3D super file'
        READ(*,'(A)') FLNAME
      ENDIF
      CALL GETPATH_RT(FLNAME,PATH,PREFIX)
      OPEN(UNIT=ISUP,FILE=FLNAME,STATUS='OLD',ERR=600)
      READ(ISUP, 601) FLTYPE
601   FORMAT(A)
602   FORMAT(A,A)
      IF (FLTYPE(1:4) .NE. 'RT3D') THEN
        WRITE(*,*) ' This file is not a GMS RT3D Superfile '
C-------ECGL JIG
        CALL STOPFILE_RT
C-------ECGL JIG
        STOP
      ENDIF



      INUMSPC = 0
C
603   READ(ISUP,602,END=604) FLTYPE,FLNAME
      
      IF(FLTYPE(1:3).EQ.'BTN') THEN
	  CALL SETPATH_RT(PATH,FLNAME)
        CALL OPENFL(INBTN,1,FLNAME)
      ELSEIF(FLTYPE(1:3).EQ.'ADV') THEN
	  CALL SETPATH_RT(PATH,FLNAME)
        CALL OPENFL(INADV,1,FLNAME)
      ELSEIF(FLTYPE(1:3).EQ.'DSP') THEN
	  CALL SETPATH_RT(PATH,FLNAME)
        CALL OPENFL(INDSP,1,FLNAME)
      ELSEIF(FLTYPE(1:3).EQ.'SSM') THEN
	  CALL SETPATH_RT(PATH,FLNAME)
        CALL OPENFL(INSSM,1,FLNAME)
      ELSEIF(FLTYPE(1:3).EQ.'RCT') THEN
	  CALL SETPATH_RT(PATH,FLNAME)
        CALL OPENFL(INRCT,1,FLNAME)
      ELSEIF(FLTYPE(1:3).EQ.'GCG') THEN
	  CALL SETPATH_RT(PATH,FLNAME)
        CALL OPENFL(INGCG,1,FLNAME)
      ELSEIF(FLTYPE(1:3).EQ.'FLO') THEN
	  CALL SETPATH_RT(PATH,FLNAME)
        CALL OPENFL(-INUHF,1,FLNAME)
      ELSEIF(FLTYPE(1:3).EQ.'CHK') THEN
        FPRT=FLNAME(1:1)
      ELSEIF(FLTYPE(1:3).EQ.'OUT') THEN
	  CALL SETPATH_RT(PATH,FLNAME)
        CALL OPENFL(IOUT,0,FLNAME)
      ELSEIF(FLTYPE(1:3).EQ.'CON') THEN
	  CALL SETPATH_RT(PATH,FLNAME)
        I1 = 1
605     IF ((I1.LT.81).AND.(FLNAME(I1:I1).NE.'.')) THEN
          I1 = I1 + 1
          GO TO 605
        ENDIF
        WRITE(CONCFILE,606) FLNAME(1:I1-1),INUMSPC,
     &        FLNAME(I1:I1+3)
606     FORMAT(A,I3.3,A)
      ELSEIF(FLTYPE(1:3).EQ.'OBS') THEN
	CALL SETPATH_RT(PATH,FLNAME)
        CALL OPENFL(IOBS,0,FLNAME)
C      ELSEIF(FLTYPE(1:3).EQ.'MAS') THEN
C	 CALL SETPATH_RT(PATH,FLNAME)
C        CALL OPENFL(IMAS,0,FLNAME)
      ELSEIF(FLTYPE(1:3).EQ.'DSS') THEN
	  CALL SETPATH_RT(PATH,FLNAME)
        CALL OPENFL(IDSS,0,FLNAME)
        WRITE(IDSS,607)
607     FORMAT('SUPER')
      ELSEIF(FLTYPE(1:3).EQ.'SPC') THEN
	  CALL SETPATH_RT(PATH,FLNAME)
        I2 = 80
608     IF ((I2.GT.0).AND.(FLNAME(I2:I2).NE.'\')) THEN
          I2 = I2 - 1
          GO TO 608
        ENDIF
        I3 = 80
609     IF ((I3.GT. 0).AND.(FLNAME(I3:I3).EQ.' ')) THEN
          I3 = I3 - 1
          GO TO 609
        ENDIF
        INUMSPC = INUMSPC + 1
        SPECIES(INUMSPC) = FLNAME(I2+1:I3+1)
        WRITE(CONCFILE(I1:I1+2),'(I3.3)') INUMSPC
        CALL OPENFL(-(ISPC+INUMSPC),0,CONCFILE)
        I2 = 80
610     IF ((I2.GT.0).AND.(CONCFILE(I2:I2).NE.'\')) THEN
          I2 = I2 - 1
          GO TO 610
        ENDIF
        I3 = 80
611     IF ((I3.GT.0).AND.(CONCFILE(I3:I3).EQ.' ')) THEN
          I3 = I3 - 1
          GO TO 611
        ENDIF
        len = i3-i2+1
        CONCNAME = '"'
        CONCNAME(2:len) = CONCFILE(I2+1:I3)
        CONCNAME(len+1:len+1) = '"'
        WRITE(IDSS,612) CONCNAME
612     FORMAT('DATA',3X,A)
      ELSE
      ENDIF
      GO TO 603

604   CONTINUE
C
C****END OF ECGL ADDTIONS
C--WRITE PROGRAM TITLE TO OUTPUT FILE
      WRITE(IOUT,11)
   11 FORMAT(/30X,71('+')/30X,'+',69X,'+'
     &  /30X,'+',28X,'   RT3Dv2.0',32X,'+'
     &  /30X,'+',13X,'A 3D Reactive Transport Model ',
     &           13X,'+'
     &  /30X,'+', 4X,'For Simulation of Advection, Dispersion and',
     &           ' Chemical Reactions',3X,'+'
     &  /30X,'+',16X,'of Contaminants in Groundwater Systems',15X,'+'
     &  /30X,'+',69X,'+'/30X,71('+')/)
C
C
C--DEFINE PROBLEM DIMENSION AND SIMULATION OPTIONS
      CALL BTN3DF(INBTN,IOUT,ISUM,ISUM2,NCOL,NROW,NLAY,NPER,
     & NCOMP,MCOMP,TRNOP,TUNIT,LUNIT,MUNIT,NODES,MXCOMP)
C
C--OPEN INPUT FILES FOR THE VARIOUS TRASNPORT OPTIONS
C     IF(TRNOP(1)) THEN
C       FINDEX='Advection Input File: '
C       ISTAT=1
C       CALL OPENFL(INADV,ISTAT,FLNAME,0,FINDEX)
C     ENDIF
C     IF(TRNOP(2)) THEN
C       FINDEX='Dispersion Input File: '
C       ISTAT=1
C       CALL OPENFL(INDSP,ISTAT,FLNAME,0,FINDEX)
C     ENDIF
C     IF(TRNOP(3)) THEN
C       FINDEX='Sink & Source Input File: '
C       ISTAT=1
C       CALL OPENFL(INSSM,ISTAT,FLNAME,0,FINDEX)
C     ENDIF
C     IF(TRNOP(4)) THEN
C       FINDEX='Chemical Reaction Input File: '
C       ISTAT=1
C       CALL OPENFL(INRCT,ISTAT,FLNAME,0,FINDEX)
C     ENDIF
C     IF(TRNOP(5)) THEN
C       FINDEX='GCG Solver Input File: '
C       ISTAT=1
C       CALL OPENFL(INGCG,ISTAT,FLNAME,0,FINDEX)
C     ENDIF
C
C--OPEN UNFORMATTED HEAD & FLOW FILE SAVED BY A FLOW MODEL
C     FINDEX='Flow-Transport Link File: '
C     ISTAT=1
C     CALL OPENFL(-INUHF,ISTAT,FLNAME,0,FINDEX)
C     WRITE(*,15)
C  15 FORMAT(1X,'Print Contents of Flow-Transport Link File',
C    & ' for Checking (y/n)? ')
C     READ(*,'(A1)') FPRT
C     IF(FPRT.EQ.' ') FPRT='N'
C
C--ALLOCATE STORAGE SPACE FOR DATA ARRAYS
      CALL BTN3AL(INBTN,IOUT,ISUM,ISUM2,NCOL,NROW,NLAY,NCOMP,
     & LCLAYC,LCDELR,LCDELC,LCHTOP,LCDZ,LCPR,LCXBC,LCYBC,LCZBC,
     & LCQX,LCQY,LCQZ,LCQSTO,LCDH,LCIB,LCCOLD,LCCNEW,LCCWGT,
     & LCCADV,LCRETA,LCSR,LCBUFF)
C
      CALL FMI3AL(INUHF,IOUT,TRNOP,NPERFL,ISS,IVER)
C
      IF(TRNOP(1)) CALL ADV3AL(INADV,IOUT,ISUM,ISUM2,NCOL,NROW,NLAY,
     & MCOMP,MIXELM,MXPART,PERCEL,NADVFD,LCXP,LCYP,LCZP,
     & LCINDX,LCINDY,LCINDZ,LCCNPT,LCCHEK,TRNOP)
C
      IF(TRNOP(2)) CALL DSP3AL(INDSP,IOUT,ISUM,ISUM2,
     & NCOL,NROW,NLAY,LCAL,LCTRPT,LCTRPV,LCDM,LCDXX,LCDXY,LCDXZ,
     & LCDYX,LCDYY,LCDYZ,LCDZX,LCDZY,LCDZZ)
C
      IF(TRNOP(3)) CALL SSM3AL(INSSM,IOUT,ISUM,ISUM2,NCOL,NROW,NLAY,
     & NCOMP,LCIRCH,LCRECH,LCCRCH,LCIEVT,LCEVTR,LCCEVT,MXSS,LCSS,
     & IVER,LCSSMC)
C
      IF(TRNOP(4)) CALL RCTRTAL(INRCT,IOUT,ISUM,ISUM2,NCOL,NROW,NLAY,
     & ncomp,mcomp,ISOTHM,IREACT,isolver,ncrxndata,nvrxndata,irctop,
     & LCRHOB,LCSP1,LCSP2,LCRC1,LCRC2)
C
      IF(TRNOP(5)) CALL GCG3AL(INGCG,IOUT,ISUM,ISUM2,NCOL,NROW,NLAY,
     & MXITER,ITER1,NCRS,ISOLVE,LCA,LCQ,LCWK,LCCNCG,LCLRCH,LCRHS)
C
C--CHECK WHETHER ARRAYS X AND IX ARE DIMENSIONED LARGE ENOUGH.
C--IF NOT STOP
      ISUMX=ISUM
      ISUMIX=ISUM2
      WRITE(IOUT,20) ISUMX,LENX,ISUMIX,LENIX
      IF(ISUMX.GT.LENX) WRITE(IOUT,30)
      IF(ISUMIX.GT.LENIX) WRITE(IOUT,40)
C-------EMRL JIG
      IF(ISUMX.GT.LENX) WRITE(*,30)
      IF(ISUMIX.GT.LENIX) WRITE(*,40)
C-------EMRL JIG
      IF(ISUMX.GT.LENX.OR.ISUMIX.GT.LENIX) THEN
C-------EMRL JIG
        call STOPFILE_RT
C-------EMRL JIG
	  STOP
	ENDIF
   20 FORMAT(1X,64('.')/1X,I10,' ELEMENTS OF THE  X ARRAY USED OUT OF ',
     & I10/1X,I10,' ELEMENTS OF THE IX ARRAY USED OUT OF ',I10,
     & /1X,64('.')/)
   30 FORMAT(1X,'****** X ARRAY MUST BE DIMENSIONED LARGER ******')
   40 FORMAT(1X,'****** IX ARRAY MUST BE DIMENSIONED LARGER ******')
C
      ALLOCATE (X(0:ISUMX),IX(0:ISUMIX),STAT=IERR)
      IF(IERR.NE.0) THEN
        WRITE(*,*) 'STOP.  NOT ENOUGH MEMORY'
C-------EMRL JIG
        call STOPFILE_RT
C-------EMRL JIG
        STOP
      ENDIF
C
C--INITIALIZE VARIABLES.
      IMPSOL=0
      IF(TRNOP(5)) THEN
        IMPSOL=1
        ISPD=1
        IF(MIXELM.EQ.0) ISPD=0
      ENDIF
C
C--INITILIZE ARRAYS.
      DO I=1,ISUMX
        X(I)=0.
      ENDDO
      DO I=1,ISUMIX
        IX(I)=0
      ENDDO
      DO I=1,20
        DO INDEX=1,NCOMP
          TMASIO(I,1,INDEX)=0.
          TMASIO(I,2,INDEX)=0.
        ENDDO
      ENDDO
C
C--READ AND PREPARE INPUT DATA RELEVANT TO
C--THE ENTIRE SIMULATION
      CALL BTN3RP(INBTN,IOUT,IUCN,IOBS,IMAS,ICNF,ICBM,ISPC,SPECIES,
     & NCOL,NROW,NLAY,NCOMP,IX(LCLAYC),X(LCDELR),X(LCDELC),X(LCHTOP),
     & X(LCDZ),X(LCPR),IX(LCIB),X(LCCOLD),X(LCCNEW),X(LCCADV),
     & CINACT,THKMIN,X(LCXBC),X(LCYBC),X(LCZBC),
     & X(LCRETA),RFMIN,X(LCBUFF),MXPRS,NPRS,TIMPRS,
     & MXOBS,NOBS,NPROBS,LOCOBS,TUNIT,LUNIT,MUNIT)
C
      IF(TRNOP(1)) CALL ADV3RP(INADV,IOUT,NCOL,NROW,NLAY,MCOMP,
     & MIXELM,MXPART,NADVFD,NCOUNT)
C
      IF(TRNOP(2)) CALL DSP3RP(INDSP,IOUT,NCOL,NROW,NLAY,
     & X(LCAL),X(LCTRPT),X(LCTRPV),X(LCDM))
C
      IF(TRNOP(4)) CALL RCTRTRP(INRCT,IOUT,NCOL,NROW,NLAY,ncomp,
     & mcomp,IX(LCIB),X(LCCOLD),X(LCPR),ISOTHM,IREACT,isolver,
     &ncrxndata,nvrxndata,irctop,X(LCRHOB),X(LCSP1),X(LCSP2),X(LCRC1),
     & X(LCRC2),X(LCRETA),RFMIN,IFMTRF,DTRCT)
C
      IF(TRNOP(5)) CALL GCG3RP(INGCG,IOUT,MXITER,ITER1,ISOLVE,ACCL,
     & CCLOSE,IPRGCG)
C
C--FOR EACH STRESS PERIOD***********************************************
      HT1=0.
      HT2=0.
      NPS=1
      DO KPER=1,NPER
C
C--WRITE AN INDENTIFYING MESSAGE
#ifdef _DEBUG
	!!DEC$ IF DEFINED(_DEBUG)
      WRITE(*,50) KPER
	!!DEC$ ENDIF
#endif
      WRITE(IOUT,51) KPER
      WRITE(IOUT,'(1X)')
   50 FORMAT(/1X,'STRESS PERIOD NO.',I5)
   51 FORMAT(//35X,62('+')/55X,'STRESS PERIOD NO.',I5.3/35X,62('+'))
C
C--GET STRESS TIMING INFORMATION
      CALL BTN3ST(INBTN,IOUT,NSTP,MXSTP,TSLNGH,DT0,MXSTRN,TTSMULT,
     & TTSMAX,TUNIT)
C
C--READ AND PREPARE INPUT INFORMATION WHICH IS CONSTANT
C--WITHIN EACH STRESS PERIOD
      IF(TRNOP(3)) CALL SSM3RP(INSSM,IOUT,KPER,NCOL,NROW,NLAY,NCOMP,
     & IX(LCIB),X(LCCNEW),X(LCCRCH),X(LCCEVT),MXSS,NSS,X(LCSS),
     & X(LCSSMC))
C
C--FOR EACH FLOW TIME STEP----------------------------------------------
      DO KSTP=1,NSTP
      DELT=TSLNGH(KSTP)
      HT1=HT2
      HT2=HT2+DELT
C
C--WRITE AN INDENTIFYING MESSAGE
#ifdef _DEBUG
	!!DEC$ IF DEFINED(_DEBUG)
      WRITE(*,60) KSTP,HT1,HT2
	!!DEC$ ENDIF
#endif
      WRITE(IOUT,61) KSTP,HT1,HT2
      WRITE(IOUT,'(1X)')
   60 FORMAT(/1X,'TIME STEP NO.',I5
     & /1X,'FROM TIME =',G13.5,' TO ',G13.5/)
   61 FORMAT(//42X,48('=')/57X,'TIME STEP NO.',I5.3/42X,48('=')
     & //1X,'FROM TIME =',G13.5,' TO ',G13.5)
C
C--READ AND PROCESS SATURATED THICKNESS, VELOCITY COMPONENTS
C--ACROSS CELL INTERFACES, AND SINK/SOURCE INFORMATION
C--(NOTE THAT THESE ITEMS ARE READ ONLY ONCE IF FLOW MODEL
C--IS STEADY-STATE AND HAS SINGLE STRESS PERIOD)
      IF(KPER*KSTP.GT.1.AND.ISS.NE.0.AND.NPERFL.EQ.1) GOTO 70
C
      CALL FMI3RP1(INUHF,IOUT,KPER,KSTP,NCOL,NROW,NLAY,NCOMP,FPRT,
     & IX(LCLAYC),IX(LCIB),HORIGN,X(LCDH),X(LCPR),X(LCDELR),X(LCDELC),
     & X(LCDZ),X(LCXBC),X(LCYBC),X(LCZBC),X(LCQSTO),X(LCCOLD),
     & X(LCCNEW),X(LCRETA),X(LCQX),X(LCQY),X(LCQZ),
     & DTRACK,DTRACK2,THKMIN,ISS,IVER)
C
      IF(TRNOP(3)) CALL FMI3RP2(INUHF,IOUT,KPER,KSTP,NCOL,NROW,NLAY,
     & NCOMP,FPRT,IX(LCLAYC),IX(LCIB),X(LCDH),X(LCPR),X(LCDELR),
     & X(LCDELC),IX(LCIRCH),X(LCRECH),IX(LCIEVT),X(LCEVTR),
     & MXSS,NSS,NTSS,X(LCSS),X(LCBUFF),DTSSM)
C
C--CALCULATE COEFFICIENTS THAT VARY WITH FLOW-MODEL TIME STEP
      IF(TRNOP(2)) CALL DSP3CF(IOUT,KSTP,KPER,NCOL,NROW,NLAY,
     & IX(LCIB),X(LCPR),X(LCDELR),X(LCDELC),X(LCDH),
     & X(LCQX),X(LCQY),X(LCQZ),X(LCAL),X(LCTRPT),X(LCTRPV),X(LCDM),
     & DTDISP,X(LCDXX),X(LCDXY),X(LCDXZ),
     & X(LCDYX),X(LCDYY),X(LCDYZ),X(LCDZX),X(LCDZY),X(LCDZZ),IFMTDP)
C
   70 CONTINUE
C
C--FOR EACH TRANSPORT STEP..............................................
      TIME2=HT1
      DO N=1,MXSTRN
C
C--ADVANCE ONE TRANSPORT STEP
      CALL BTN3AD(N,TRNOP,TIME1,TIME2,HT2,DELT,KSTP,NSTP,
     & MXPRS,TIMPRS,DT0,MXSTRN,MIXELM,DTRACK,DTRACK2,
     & PERCEL,DTDISP,DTSSM,DTRCT,RFMIN,NPRS,NPS,DTRANS,PRTOUT,
     & NCOL,NROW,NLAY,NCOMP,IX(LCIB),X(LCCNEW),X(LCCOLD),
     & CINACT,UPDLHS,IMPSOL,TTSMULT,TTSMAX,KPER,X(LCDELR),X(LCDELC),
     & X(LCDH),X(LCPR),X(LCSR),X(LCRHOB),ISOTHM,TMASIO,RMASIO,TMASS)
C
C--FOR EACH COMPONENT......
	do icomp = 1, mcomp
C
      IF(IMPSOL.EQ.1 .AND. MIXELM.EQ.0) GOTO 1500 !Skipping explicit block
c     the above condition is activated when everything is fully-implicit  
C--SOLVE TRANSPORT TERMS WITH EXPLICIT SCHEMES                            
C
C--FORMULATE AND SOLVE
      CALL BTN3SV(NCOL,NROW,NLAY,NCOMP,ICOMP,IX(LCIB),X(LCCNEW),
     & X(LCCWGT),CINACT,RMASIO)
C
      IF(TRNOP(1) .AND. ICOMP.LE.MCOMP)
     & CALL ADV3SV(IOUT,NCOL,NROW,NLAY,MCOMP,ICOMP,MIXELM,MXPART,
     & NCOUNT,NPINS,NRC,IX(LCCHEK),X(LCXP),X(LCYP),X(LCZP),
     & IX(LCINDX),IX(LCINDY),IX(LCINDZ),X(LCCNPT),IX(LCIB),
     & X(LCDELR),X(LCDELC),X(LCDZ),X(LCXBC),X(LCYBC),X(LCZBC),X(LCDH),
     & X(LCPR),X(LCQX),X(LCQY),X(LCQZ),X(LCRETA),X(LCCOLD),
     & X(LCCWGT),X(LCCNEW),X(LCCADV),X(LCBUFF),DTRANS,IMPSOL,NADVFD,
     & TMASIO,RMASIO,TMASS)
C
      IF(IMPSOL.EQ.1) THEN
        IF (icomp. GE. mcomp) THEN
           GOTO 1500  !move to implicit dsp&ssm solutions
	ELSE 
	  GOTO 1495 !solve advection for next component
	ENDIF
      ENDIF 
c     the above condition is activated when advection is solved using 
c     moc or tvd schemes and others implicitly
C
      IF(TRNOP(2) .AND. ICOMP.LE.MCOMP)
     & CALL DSP3SV(NCOL,NROW,NLAY,MCOMP,ICOMP,IX(LCIB),X(LCDELR),
     & X(LCDELC),X(LCDH),X(LCRETA),X(LCPR),X(LCDXX),X(LCDXY),X(LCDXZ),
     & X(LCDYX),X(LCDYY),X(LCDYZ),X(LCDZX),X(LCDZY),X(LCDZZ),
     & X(LCCNEW),X(LCCWGT),X(LCBUFF),DTRANS,TMASIO,RMASIO,TMASS)
C
      IF(TRNOP(3))
     & CALL SSM3SV(NCOL,NROW,NLAY,NCOMP,ICOMP,IX(LCIB),X(LCPR),
     & X(LCDELR),X(LCDELC),X(LCDH),X(LCRETA),IX(LCIRCH),X(LCRECH),
     & X(LCCRCH),IX(LCIEVT),X(LCEVTR),X(LCCEVT),MXSS,NTSS,NSS,
     & X(LCSS),X(LCSSMC),X(LCQSTO),X(LCCNEW),X(LCCWGT),
     & DTRANS,MIXELM,ISS,TMASIO,RMASIO,TMASS)

1495   CONTINUE
C--end of mcomp transport only loop (for explicit computations)
      enddo

      GOTO 2000 !skipping implicit transport block
 
1500  CONTINUE !Start implicit transport block

      DO icomp = 1, mcomp
C--SOLVE TRANSPORT TERMS WITH IMPLICIT SCHEMES
      IF(DTRANS.EQ.0) THEN
        ICNVG=1
        GOTO 110
      ENDIF
C
C--ALWAYS UPDATE MATRIX IF NONLINEAR SORPTION OR MULTICOMPONENT
      IF(TRNOP(4).AND.ISOTHM.GT.1) THEN
        UPDLHS=.TRUE.
      ENDIF
      IF(NCOMP.GT.1) UPDLHS=.TRUE.
C
C--FOR EACH OUTER ITERATION...
      DO ITO=1,MXITER
C
C--UPDATE COEFFICIENTS THAT VARY WITH ITERATIONS
c      IF(TRNOP(4).AND.ISOTHM.GT.1)
c     & CALL RCT3CF(NCOL,NROW,NLAY,NCOMP,ICOMP,IX(LCIB),
c     & X(LCPR),X(LCCNEW),X(LCRETA),RFMIN,X(LCRHOB),X(LCSP1),
c     & X(LCSP2),X(LCSR),ISOTHM,DTRANS)
C
C--FORMULATE MATRIX COEFFICIENTS
      CALL BTN3FM(NCOL,NROW,NLAY,NCOMP,ICOMP,IX(LCIB),X(LCCADV),
     & X(LCCOLD),X(LCRETA),X(LCPR),X(LCDELR),X(LCDELC),X(LCDH),DTRANS,
     & X(LCA),X(LCRHS),NODES,UPDLHS,NCRS,MIXELM)
C
      IF(TRNOP(1).AND.MIXELM.EQ.0 .AND. ICOMP.LE.MCOMP)
     & CALL ADV3FM(NCOL,NROW,NLAY,MCOMP,ICOMP,IX(LCIB),X(LCDELR),
     & X(LCDELC),X(LCDH),X(LCQX),X(LCQY),X(LCQZ),NADVFD,NODES,
     & X(LCA),UPDLHS)
C
      IF(TRNOP(2) .AND. ICOMP.LE.MCOMP)
     & CALL DSP3FM(NCOL,NROW,NLAY,MCOMP,ICOMP,IX(LCIB),
     & X(LCDELR),X(LCDELC),X(LCDH),X(LCDXX),X(LCDXY),X(LCDXZ),X(LCDYX),
     & X(LCDYY),X(LCDYZ),X(LCDZX),X(LCDZY),X(LCDZZ),
     & X(LCA),NODES,UPDLHS,X(LCCNEW),X(LCRHS),NCRS)
C
      IF(TRNOP(3)) CALL SSM3FM(NCOL,NROW,NLAY,NCOMP,ICOMP,IX(LCIB),
     & X(LCDELR),X(LCDELC),X(LCDH),IX(LCIRCH),X(LCRECH),X(LCCRCH),
     & IX(LCIEVT),X(LCEVTR),X(LCCEVT),MXSS,NTSS,X(LCSS),X(LCSSMC),
     & X(LCQSTO),X(LCCNEW),ISS,X(LCA),X(LCRHS),NODES,UPDLHS,MIXELM)
C
c      IF(TRNOP(4)) CALL RCT3FM(NCOL,NROW,NLAY,NCOMP,ICOMP,IX(LCIB),
c     & X(LCPR),X(LCDELR),X(LCDELC),X(LCDH),ISOTHM,IREACT,
c     & X(LCRHOB),X(LCSP1),X(LCSP2),X(LCSR),X(LCRC1),X(LCRC2),
c     & X(LCA),X(LCRHS),NODES,UPDLHS,DTRANS)
C
      CALL GCG3AP(IOUT,MXITER,ITER1,ITO,ITP,ISOLVE,ACCL,CCLOSE,
     & ICNVG,X(LCCNCG),IX(LCLRCH),NCOL,NROW,NLAY,NODES,N,KSTP,KPER,
     & TIME2,HT2,UPDLHS,IPRGCG,IX(LCIB+(ICOMP-1)*NODES),CINACT,X(LCA),
     & X(LCCNEW+(ICOMP-1)*NODES),X(LCRHS),X(LCQ),X(LCWK),NCRS,ISPD)
C
C--IF CONVERGED, GO TO NEXT OUTER ITERATION
      IF(ICNVG.EQ.1) GOTO 110
C
C--END OF OUTER ITERATION LOOP
      ENDDO
C
  110 CONTINUE
C
C--CALCULATE MASS BUDGETS FOR IMPLICIT SCHEMES
      IF(TRNOP(1).AND.MIXELM.EQ.0 .AND. ICOMP.LE.MCOMP)
     & CALL ADV3BD(IOUT,NCOL,NROW,NLAY,MCOMP,ICOMP,NADVFD,IX(LCIB),
     & X(LCDELR),X(LCDELC),X(LCDH),X(LCQX),X(LCQY),X(LCQZ),X(LCCNEW),
     & DTRANS,TMASIO,RMASIO,TMASS)
C
      IF(TRNOP(2) .AND. ICOMP.LE.MCOMP)
     & CALL DSP3BD(NCOL,NROW,NLAY,MCOMP,ICOMP,IX(LCIB),
     & X(LCDELR),X(LCDELC),X(LCDH),X(LCDXX),X(LCDXY),X(LCDXZ),
     & X(LCDYX),X(LCDYY),X(LCDYZ),X(LCDZX),X(LCDZY),X(LCDZZ),
     & X(LCCNEW),X(LCBUFF),DTRANS,TMASIO,RMASIO,TMASS)
C
      IF(TRNOP(3)) CALL SSM3BD(NCOL,NROW,NLAY,NCOMP,ICOMP,IX(LCIB),
     & X(LCDELR),X(LCDELC),X(LCDH),IX(LCIRCH),X(LCRECH),
     & X(LCCRCH),IX(LCIEVT),X(LCEVTR),X(LCCEVT),MXSS,NTSS,
     & X(LCSS),X(LCSSMC),X(LCQSTO),X(LCCNEW),DTRANS,ISS,
     & TMASIO,RMASIO,TMASS)
C
C--End of mcomp transport loop (for implicit computatations)
      end do

 2000  CONTINUE !end of implicit transport block

c******Solving reactions*********
c******Reactions are always solved implicity*********
         IF(TRNOP(4))
     & CALL RCTRTSV(NCOL,NROW,NLAY,ncomp,mcomp,IX(LCIB),X(LCPR),
     & X(LCDELR),X(LCDELC),X(LCDH),X(LCRETA),RFMIN,DTRANS,ISOTHM,IREACT,
     & isolver,nvrxndata,X(LCRHOB),X(lcSP1),X(lcSP2),X(lcRC1),X(lcRC2),
     & X(lcCNEW),x(lcCWGT),RMASIO)

     

C*******End reaction solver*********

      DO icomp = 1, ncomp
c         IF(TRNOP(4)) CALL RCT3BD(NCOL,NROW,NLAY,NCOMP,ICOMP,IX(LCIB),
c     &   X(LCPR),X(LCDELR),X(LCDELC),X(LCDH),DTRANS,ISOTHM,IREACT,
c     &   X(LCRHOB),X(LCSP1),X(LCSP2),X(LCSR),X(LCRC1),X(LCRC2),
c     &   X(LCCNEW),X(LCRETA),RFMIN,RMASIO)
C
C--CALCULATE MASS BUDGETS
       CALL BTN3BD(KPER,KSTP,N,NCOL,NROW,NLAY,NCOMP,ICOMP,IX(LCIB),
     & X(LCDELR),X(LCDELC),X(LCDH),X(LCPR),X(LCRETA),X(LCCNEW),
     & X(LCCOLD),TMASIN,TMASOT,ERROR,ERROR2,TMASIO,RMASIO,TMASS)

C--SAVE OUTPUTS
      CALL BTN3OT(NCOL,NROW,NLAY,KPER,KSTP,N,NCOMP,ICOMP,IOUT,IOBS,
     & IUCN,ISPC,IMAS,ICBM,MXOBS,NOBS,NPROBS,LOCOBS,IX(LCIB),TIME2,
     & X(LCCNEW),MIXELM,NCOUNT,NPINS,NRC,IX(LCCHEK),ISOTHM,X(LCRETA),
     & TMASIN,TMASOT,ERROR,ERROR2,TRNOP,TUNIT,MUNIT,PRTOUT,TMASIO,
     & RMASIO,TMASS)


C--End of ncomp component loop
      enddo
C
      IF(TIME2.GE.HT2) GOTO 900
      IF(IMPSOL.EQ.1.AND.ICNVG.EQ.0) THEN
        WRITE(*,*) 'STOP. GCG SOLVER FAILED TO CONVERGE.'
C-------EMRL JIG
        call STOPFILE_RT
C-------EMRL JIG
C CHANGED BY SHEGNQUAN YAN TO AVOID RT3D QUITTING 08/04/03
C        STOP
      ENDIF
C
C--END OF TRANSPORT STEP LOOP
      ENDDO
C
      IF(TIME2.LT.HT2) THEN
        WRITE(IOUT,810) MXSTRN
C-------EMRL JIG
        WRITE(*,810) MXSTRN
C-------EMRL JIG
  810   FORMAT(/1X,'NUMBER OF TRANSPORT STEPS EXCEEDS',
     &   ' SPECIFIED MAXIMUM (MXSTRN) =',I10)
C-------EMRL JIG
        call STOPFILE_RT
C-------EMRL JIG
        STOP
      ENDIF
  900 CONTINUE
C
C--END OF FLOW TIME STEP LOOP
      ENDDO
C
C--END OF STRESS PERIOD LOOP
      ENDDO

C	Write concentration to a file, By Shengquan Yan. 07/28/05
	DO ICOMP = 1, NCOMP
		CALL SAVE_CONC(NCOL,NROW,NLAY,NCOMP,ICOMP,X(LCCNEW))
	ENDDO
C
C--DEALLOCATE MEMORY
      DEALLOCATE (X,IX)
C
C--PROGRAM COMPLETED
      WRITE(IOUT,1200)
      WRITE(IOUT,1225)
      WRITE(IOUT,1200)
#ifdef _DEBUG
	!!DEC$ IF DEFINED(_DEBUG)
	WRITE(*,1200)
      WRITE(*,1225)
      WRITE(*,1200)
     	WRITE(*,*) '***PROGRAM TERMINATED SUCCESSFULLY***'
	!!DEC$ ENDIF
#endif
 1200 FORMAT(1X,' ----- ')
C-------EMRL JIG
C 1225 FORMAT(1X,'| M T |'
 1225 FORMAT(1X,'| R T |'
C-------EMRL JIG
     &      /1X,'| 3 D | END OF MODEL OUTPUT')
C
C-------EMRL JIG
      call STOPFILE_RT
C-------EMRL JIG
C#########################Patched by shengquan Yan 11/25/01########################
C	STOP
	call CloseFiles
	contains
		subroutine CloseFiles
			use array_config
			use ssmdecay_array
			implicit none

			close( INBTN )
			close( INADV )
			close( INDSP )
			close( INSSM )
			close( INRCT )
			close( INGCG )
			close( INUHF )
			close( IOUT )
			close( ICNF )
			close( IUCN )
			close( IOBS )
			close( IMAS )
			close( ICBM )
			close( ISUP )
			close( IDSS )
			close( ISPC )
			if( allocated(rxnarray2) )then
				deallocate( rxnarray2 )
			end if
			if( allocated(ssmcdecay) )then
				deallocate( ssmcdecay )
			end if
		end subroutine
	end subroutine Rt3dEntry
C      END
C#########################Patch finished###########################################

	SUBROUTINE SAVE_CONC(NCOL, NROW, NLAY, NCOMP, ICOMP, CNEW)
	IMPLICIT NONE
	INTEGER	NCOL, NROW, NLAY, NCOMP, ICOMP
	REAL	CNEW(NCOL, NROW, NLAY, NCOMP)
	INTEGER I, J, K

C	Save the concentration to the concentration file
	OPEN(UNIT= 111, FILE='newconc.out',STATUS='UNKNOWN' )
      REWIND 111
	
C	WRITE(111, *)(((CNEW(K,J,I,ICOMP),K=1,NCOL),J=1,NROW),I=1,NLAY)
 
	DO I=1,NLAY
		DO J=1, NROW
			DO K = 1, NCOL
				WRITE(111, *)CNEW(K,J,I,ICOMP)
			ENDDO
		ENDDO
	ENDDO
	CLOSE(111)
	END SUBROUTINE

C****ECGL ADDTIONS
C       THESE THREE FUNCTIONS WERE ADDED TO TAKE CARE OF
C       ANY PATH ISSUES AND TO BE ABLE TO RUN RT3D FROM
C       WITHIN GMS.
C
      SUBROUTINE GETPATH_RT(NAME,PATH,PREFIX)
      CHARACTER*80 NAME,PATH,PREFIX
      INTEGER*4 COUNT

      COUNT = 80
      PATH = NAME
	PREFIX = NAME
200   IF ((COUNT .GT. 0) .AND. (PATH(COUNT:COUNT) .NE. '\')) THEN
        PATH(COUNT:COUNT) = ' '
        COUNT = COUNT - 1
        GO TO 200
      ENDIF
	COUNT = 80
300   IF ((COUNT .GT. 0) .AND. (PREFIX(COUNT:COUNT) .NE. '.')) THEN
        PREFIX(COUNT:COUNT) = ' '
        COUNT = COUNT - 1
        GO TO 300
      ENDIF

      END

      SUBROUTINE SETPATH_RT(PATH,FNAME)
      CHARACTER*80 PATH,FNAME,NEWNAME
      INTEGER*4 COUNT,I1,I2
C
      I1 = 1
888   IF (FNAME(I1:I1) .EQ. '\') THEN
        COUNT = 0
        GO TO 996
      ELSE
        I1 = I1 + 1
      ENDIF
      IF (I1 .LT. 81) THEN
        GOTO 888
      ENDIF
      COUNT = 80
889   IF ((COUNT .GT. 0) .AND. (PATH(COUNT:COUNT) .EQ. ' ')) THEN
        COUNT = COUNT - 1
        GO TO 889
      ENDIF
      NEWNAME = PATH
996   COUNT = COUNT + 1
      I1 = 1
      I2 = 0
997   IF (FNAME(I1:I1) .EQ. '"') THEN
        GO TO 998
      ELSE
        I1 = I1 + 1
      ENDIF
      GO TO 997
998   IF (COUNT .LT. 81 .AND. I1 .LT. 81) THEN
        IF (FNAME(I1:I1) .NE. '"') THEN
          IF (I2 .LT. 2) THEN
            NEWNAME(COUNT:COUNT) = FNAME(I1:I1)
          ELSE
            NEWNAME(COUNT:COUNT) = ' '
          ENDIF
          COUNT = COUNT + 1
        ELSE
          I2 = I2 + 1
        ENDIF
        I1 = I1 + 1
        GO TO 998
      ENDIF
999   FNAME = NEWNAME
      END

	subroutine STOPFILE_RT
C	use dflib
	integer*4 result

c	result=setexitqq(qwin$exitpersist)
C#########################Patched by shengquan Yan 11/25/01########################
C	stop
C#########################Patch finished###########################################

      end

	SUBROUTINE SETNAME(EXT,FNAME,INDEX)
	CHARACTER*80 PATH,PREFIX,FNAME,EXT
	COMMON /EMRL/PATH,PREFIX
	INTEGER*4 COUNT,COUNT1,INDEX
! added by Meghna ... 14th august 2001 ****************************
        INTEGER C1
! *****************************************************************
       
      FNAME=PREFIX
	COUNT=0
200   COUNT=COUNT+1
      IF (FNAME(COUNT:COUNT).NE.'.'.AND.COUNT.LT.81) GOTO 200
	WRITE(FNAME(COUNT:COUNT+2),'(I3.3)') INDEX
	COUNT=COUNT+3
      FNAME(COUNT:COUNT)= '.'

      COUNT1=0
300   COUNT1=COUNT1+1
      IF (EXT(COUNT1:COUNT1).NE.' '.AND.COUNT.LT.81) GOTO 300
	COUNT1=COUNT1-1
	
	DO C1=1,COUNT1
	  FNAME(COUNT+C1:COUNT+C1)=EXT(C1:C1)
	END DO
      

	END
C****END OF ECGL ADDTIONS
