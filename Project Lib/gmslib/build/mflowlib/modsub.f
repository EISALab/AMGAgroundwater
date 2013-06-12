      subroutine captrans(aa)

        implicit none
        integer*4 i,ii
        character*(*) aa

        do i=1,len(aa)
          ii=ichar(aa(i:i))
          if((ii.ge.97).and.(ii.le.122)) aa(i:i)=char(ii-32)
10      end do

        return
      end



      integer function NextUnit(IFail)

C -- Function NextUnit finds the next available unit number.

        implicit none

        logical Lopened
        integer IFail,i

        Ifail=0
        do i=10,100
          inquire(unit=i,opened=lopened)
          if(.not.lopened) go to 110
        end do
        ifail=1
        return

110     continue
        NextUnit=i
        return

      end function NextUnit



      subroutine remchar(cline,achar)

C -- Subroutine remchar removes all incidences of a character from a string.

        implicit none
        integer i,n
        character achar
        character*(*) cline

        n=len_trim(cline)
        do i=1,n
          if(cline(i:i).eq.achar)cline(i:i)=' '
        end do

        return
      end


      SUBROUTINE INTREAD(IFAIL,CLINE,iTEMP)
 
C -- Subroutine INTREAD reads an integer from a string.
 
        INTEGER IFAIL
        INTEGER ITEMP
        CHARACTER*6 AFMT
        CHARACTER*(*) CLINE
 
        IFAIL=0
        AFMT='(i   )'
        WRITE(AFMT(3:5),'(I3)') len_trim(CLINE)
        READ(CLINE(1:len_trim(cline)),AFMT,ERR=100) ITEMP
        RETURN
 
100     IFAIL=1
        RETURN
      END


      SUBROUTINE REALREAD(IFAIL,CLINE,RTEMP)
 
C -- Subroutine REALREAD reads a real number from a string.
 
        INTEGER IFAIL
        REAL RTEMP
        CHARACTER*8 AFMT
        CHARACTER*(*) CLINE
 
        IFAIL=0
        AFMT='(F   .0)'
        WRITE(AFMT(3:5),'(I3)') len_trim(CLINE)
        READ(CLINE(1:len_trim(cline)),AFMT,ERR=100) RTEMP
        RETURN
 
100     IFAIL=1
        RETURN
        END


      subroutine WritInt(atemp,ival)

C --Subroutine WRITINT writes an integer to a character variable.

        integer*4 ival
        character*6 afmt
        character*(*) atemp

        afmt='(i   )'
        write(afmt(3:5),'(i3)') len(atemp)
        write(atemp,afmt)ival
        atemp=adjustl(atemp)
        return
      end



      subroutine getdate(ifail,adate,dd,mm,yy)

C -- Subroutine getdate retreives a date from a string.

        logical leap
        integer ifail,dd,mm,yy,n
        character*(*) adate
        character*(15) tdate

        ifail=0
        adate=adjustl(adate)
        tdate=adate(1:min(15,len(adate)))
        n=index(tdate,'/')
        if(n.eq.0) go to 9999
        if(n.eq.1) go to 9999
        if(n.eq.2)then
          read(tdate(1:1),'(i1)',err=9999) mm
        else if(n.eq.3)then
          read(tdate(1:2),'(i2)',err=9999) mm
        else
          go to 9999
        end if
        tdate=tdate(n+1:)
        n=index(tdate,'/')
        if(n.eq.0) go to 9999
        if(n.eq.1) go to 9999
        if(n.eq.2) then
          read(tdate(1:1),'(i1)',err=9999) dd
        else if(n.eq.3) then
          read(tdate(1:2),'(i2)',err=9999) dd
        else
          go to 9999
        end if
        tdate=tdate(n+1:)
        n=len_trim(tdate)
        if(n.ne.4) go to 9999
        read(tdate(1:4),'(i4)',err=9999) yy

        if((mm.lt.1).or.(mm.gt.12)) go to 9999
        if((mm.eq.1).or.(mm.eq.3).or.(mm.eq.5).or.
     +  (mm.eq.7).or.(mm.eq.8).or.(mm.eq.10).or.(mm.eq.12))then
          if(dd.gt.31) go to 9999
        else if((mm.eq.4).or.(mm.eq.6).or.(mm.eq.9).or.
     +  (mm.eq.11))then
          if(dd.gt.30) go to 9999
        else
          if(leap(yy)) then
            if(dd.gt.29) go to 9999
          else
            if(dd.gt.28) go to 9999
          end if
        end if

        return

9999    ifail=1
        return
      end


      logical function leap(year)

C -- Function LEAP returns .true. if a year is a leap year.


        integer year

        leap = ( mod(year,4).eq.0 .and. mod(year,100).ne.0 ) .or.
     +               ( mod(year,400).eq.0 .and. year.ne.0 )

        return
      end



      subroutine gettime(ifail,atime,hh,mm,ss)

C -- Subroutine gettime reads the time from a string.

        integer ifail,hh,mm,ss,n,i
        character*15 tatime
        character*(*) atime

        if(atime.eq.' ') go to 9999
        do i=1,len(atime)
          if(atime(i:i).ne.' ') go to 10
        end do
10        tatime=atime(i:)

        ifail=0
        tatime=adjustl(tatime)
        n=index(tatime,':')
        if(n.eq.0) then
          n=index(tatime,'.')
          if(n.eq.0) go to 9999
        end if
        if(n.eq.1) go to 9999
        if(n.eq.2)then
          read(tatime(1:1),'(i1)',err=9999) hh
        else if(n.eq.3)then
          read(tatime(1:2),'(i2)',err=9999) hh
        else
          go to 9999
        end if
        tatime=tatime(n+1:)
        n=index(tatime,':')
        if(n.eq.0)then
          n=index(tatime,'.')
          if(n.eq.0) go to 9999
        end if
        if(n.eq.0) go to 9999
        if(n.eq.1) go to 9999
        if(n.eq.2) then
          read(tatime(1:1),'(i1)',err=9999) mm
        else if(n.eq.3) then
          read(tatime(1:2),'(i2)',err=9999) mm
        else
          go to 9999
        end if
        tatime=tatime(n+1:)
        n=len_trim(tatime)
        if(n.gt.2) go to 9999
        if(n.eq.1)then
          read(tatime(1:n),'(i1)',err=9999) ss
        else
          read(tatime(1:n),'(i2)',err=9999) ss
        end if

        if((mm.lt.0).or.(mm.gt.59)) go to 9999
        if((hh.lt.0).or.(hh.gt.23)) go to 9999
        if((ss.lt.0).or.(ss.gt.59)) go to 9999

        return

9999    ifail=1
        return
      end



      subroutine TimeFac(Days1,Secs1,Days2,Secs2,Days,Secs,
     +  RFac1,RFac2)

C -- Subroutine TimeFac calculates time interpolation factors.

        implicit none

        integer Days1,Secs1,Days2,Secs2,Days,Secs
        real RFac1,RFac2

        double precision Diff,Diff1,Diff2

        Diff=(Days2-Days1)*86400.0d0+Secs2-Secs1
        Diff1=(Days-Days1)*86400.0d0+Secs-Secs1
        Diff2=(Days2-Days)*86400.0d0+Secs2-Secs
        RFac1=Diff2/Diff
        RFac2=Diff1/Diff

        return
      end



      subroutine Days2Date(NDays,dds,mms,yys,dd,mm,yy)

C -- Subroutine DAYS2DATE converts elapsed days since beginning of 
C    a simulation to a date.

        integer, intent(in) :: NDays,dds,mms,yys
        integer, intent(out) :: dd,mm,yy

        logical leap
        integer  yearref,newdays,idays,iyear,jdays,i,numdays
        integer  monthdays(12)

        data monthdays /31,28,31,30,31,30,31,31,30,31,30,31/

C -- First a reference date is chosen. This is the beginning of the first
C -- year. Alternatively the reference date is the beginning of a year prior
C -- to the likely calculated date if NDAYS is negative.

        if(ndays.ge.0) then
          yearref=yys
        else
          yearref=yys-abs(ndays)/365-1
        end if
        newdays=numdays(31,12,yearref-1,dds,mms,yys)
        newdays=ndays+newdays
        if(newdays.lt.0) then
          write(*,*)'Error in subroutine Days2Date'
          call STOPFILE_MF
          stop
        end if

c -- Next days are counted, starting at the new reference date.

        idays=0
        iyear=yearref
        do
          jdays=idays+365
          if(leap(iyear)) jdays=jdays+1
          if(jdays.ge.newdays) go to 20
          iyear=iyear+1
          idays=jdays
        end do

20      yy=iyear

        do i=1,12
          jdays=idays+monthdays(i)
          if((i.eq.2).and.(leap(yy))) jdays=jdays+1
          if(jdays.ge.newdays) go to 40
          idays=jdays
        end do
        write(*,*)'Error in subroutine Days2Date'
        call STOPFILE_MF
        stop
40      mm=i
        dd=newdays-idays
        if((dd.le.0).or.(mm.le.0).or.(yy.le.0)) then
          write(*,*)'Error in subroutine Days2Date'
          call STOPFILE_MF
          stop
        end if

        return

      end


      subroutine Secs2Time(Secs,hh,mm,ss)


C -- Subroutine SECS2TIME converts seconds since midnight to hours,
C    minutes and seconds.

        integer secs,hh,mm,ss

        hh=secs/3600
        secs=secs-hh*3600
        mm=secs/60
        ss=secs-mm*60

        return
      end




      integer function numdays(DR,MR,YR,D,M,Y)

	implicit none

c -- Function numdays calculates the number of days between dates
c    D-M-Y and DR-MR-YR. If the former preceeds the latter the answer is
c    negative.

c -- Arguments are as follows:-
c       dr,mr,yr:     days, months and years of first date
c       d,m,y:        days, months and years of second date
c       numdays returns the number of elapsed days


	integer dr,mr,yr,d,m,y

	INTEGER FLAG,I,J,DA(12),YE,ME,DE,YL,ML,DL
	logical leap

	DATA DA /31,28,31,30,31,30,31,31,30,31,30,31/

! --    THE SMALLER OF THE TWO DATES IS NOW CHOSEN TO DO THE COUNTING FROM.

	IF(Y.LT.YR)GO TO 10
	IF((Y.EQ.YR).AND.(M.LT.MR)) GO TO 10
	IF((Y.EQ.YR).AND.(M.EQ.MR).AND.(D.LT.DR)) GO TO 10
	FLAG=0
	YE=YR
	ME=MR
	DE=DR
	YL=Y
	ML=M
	DL=D
	GO TO 20
10      FLAG=1
	YE=Y
	ME=M
	DE=D
	YL=YR
	ML=MR
	DL=DR

! --    IN THE ABOVE THE POSTSCRIPT "E" STANDS FOR EARLIER DATE, WHILE
!       "L" STANDS FOR THE LATER DATE.

20      numdays=0
	IF((ME.EQ.ML).AND.(YL.EQ.YE))THEN
	numdays=DL-DE
	IF(FLAG.EQ.1) numdays=-numdays
	RETURN
	END IF

	DO 30 J=ME,12
	IF((ML.EQ.J).AND.(YE.EQ.YL))GOTO 40
	numdays=numdays+DA(J)
	IF((J.EQ.2).AND.(leap(ye)))numdays=numdays+1
30      CONTINUE
	GO TO 50
40      numdays=numdays+DL-DE
	IF(FLAG.EQ.1)numdays=-numdays
	RETURN

50      DO 60 I=YE+1,YL
	DO 70 J=1,12
	IF((YL.EQ.I).AND.(ML.EQ.J))GO TO 80
	numdays=numdays+DA(J)
	IF((J.EQ.2).AND.(leap(i))) numdays=numdays+1
70      CONTINUE
60      CONTINUE
	write(*,65)
65	format(/,' Error in subroutine NUMDAYS')
        call STOPFILE_MF
	stop

80      numdays=numdays+DL-DE
	IF(FLAG.EQ.1) numdays=-numdays

	RETURN
	end


      subroutine SumCell(IType,Nodes,NCell,CellNo,CellFac,
     +  Heads1,Heads2,Value)

C -- Subroutine SumCell sums the contributions made by various cells
C    according to GMS-supplied factors.

        implicit none

        integer, intent(in) :: Nodes,IType,NCell
        integer, intent(in) :: CellNo(NCell)
        real, intent(in)    :: CellFac(NCell)
        real, intent(in)  :: Heads1(Nodes)
        double precision, intent(in) :: Heads2(Nodes)
        real, intent(out) :: Value

        integer i

C -- Note that no account is taken of dry or inactive cells here

        Value=0.0
        if(IType.eq.1)then
          do i=1,NCell
            Value=Value+Heads1(CellNo(i))*CellFac(i)
          end do
        else
          do i=1,NCell
            Value=Value+Heads2(CellNo(i))*CellFac(i)
          end do
        end if
        return
        end


      subroutine HeadObs_Int(NCol,NRow,NLay,Hnew)

C -- Subroutine HeadObs_Int provides an interface between MODFLOW
C    and subroutine HeadObs of module Observation.

        use Observation
        implicit none
        integer, intent(in) :: NCol,NRow,NLay
        double precision, intent(in) :: hnew(NCol,NRow,NLay)

        call HeadObs(Ncol,Nrow,Nlay,HNew)

        return

      end



      subroutine SetOldHeads_Int(NCol,NRow,NLay,HNew)

C -- Subroutine SetOldHeads_Int provides an interface between MODFLOW
C    and subroutine SetOldHeads of module Observation.

        use Observation
        implicit none
        integer, intent(in) :: NCol,NRow,NLay
        double precision, intent(in) :: Hnew(NCol,NRow,NLay)

        call SetOldHeads(Ncol,Nrow,Nlay,HNew)

        return

      end


      subroutine SetStartHeads_Int(NCol,NRow,NLay,HOld)

C -- Subroutine SetStartHeads_Int provides an interface between MODFLOW
C    and subroutine SetStartHeads of module TrueLayer.

        use TrueLayer
        implicit none
        integer, intent(in) :: NCol,NRow,NLay
        real, intent(in)    :: HOld(NCol,NRow,NLay)

        call SetStartHeads(HOld)

        return

      end




      SUBROUTINE U2DREL1(A,ANAME,II,JJ,K,IN,IOUT)

C -- This is slightly modified from the version supplied with MODFLOW.
C    Note that this subroutine is inappropriate if it is required that
C    another file be opened to read the array. However it is assumed
C    that this will not occur. Nevertheless, despite the fact that certain
C    code segments in the code below will, presumably, not thus be used,
C    they have not been removed for now.

C
C
C-----VERSION 1539 22JUNE1993 U2DREL
C     ******************************************************************
C     ROUTINE TO INPUT 2-D REAL DATA MATRICES
C       A IS ARRAY TO INPUT
C       ANAME IS 24 CHARACTER DESCRIPTION OF A
C       II IS NO. OF ROWS
C       JJ IS NO. OF COLS
C       K IS LAYER NO. (USED WITH NAME TO TITLE PRINTOUT --)
C              IF K=0, NO LAYER IS PRINTED
C              IF K<0, CROSS SECTION IS PRINTED)
C       IN IS INPUT UNIT
C       IOUT IS OUTPUT UNIT
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      CHARACTER*24 ANAME
      DIMENSION A(JJ,II)
      CHARACTER*20 FMTIN
      CHARACTER*80 CNTRL
      CHARACTER*16 TEXT
      CHARACTER*80 FNAME
      DATA NUNOPN/99/
C     ------------------------------------------------------------------
C
C1------READ ARRAY CONTROL RECORD AS CHARACTER DATA.
      READ(IN,'(A)') CNTRL
C
C2------LOOK FOR ALPHABETIC WORD THAT INDICATES THAT THE RECORD IS FREE
C2------FORMAT.  SET A FLAG SPECIFYING IF FREE FORMAT OR FIXED FORMAT.
      ICLOSE=0
      IFREE=1
      ICOL=1
      CALL URWORD(CNTRL,ICOL,ISTART,ISTOP,1,N,R,IOUT,IN)
      IF (CNTRL(ISTART:ISTOP).EQ.'CONSTANT') THEN
         LOCAT=0
      ELSE IF(CNTRL(ISTART:ISTOP).EQ.'INTERNAL') THEN
         LOCAT=IN
      ELSE IF(CNTRL(ISTART:ISTOP).EQ.'EXTERNAL') THEN
         CALL URWORD(CNTRL,ICOL,ISTART,ISTOP,2,LOCAT,R,IOUT,IN)
      ELSE IF(CNTRL(ISTART:ISTOP).EQ.'OPEN/CLOSE') THEN
         CALL URWORD(CNTRL,ICOL,ISTART,ISTOP,0,N,R,IOUT,IN)
         FNAME=CNTRL(ISTART:ISTOP)
         LOCAT=NUNOPN
         WRITE(IOUT,15) LOCAT,FNAME
   15    FORMAT(1X,/1X,'OPENING FILE ON UNIT',I4,':',/1X,A)
         ICLOSE=1
      ELSE
C
C2A-----DID NOT FIND A RECOGNIZED WORD, SO NOT USING FREE FORMAT.
C2A-----READ THE CONTROL RECORD THE ORIGINAL WAY.
         IFREE=0
         READ(CNTRL,1,ERR=500) LOCAT,CNSTNT,FMTIN,IPRN
    1    FORMAT(I10,F10.0,A20,I10)
         if(locat.ne.0)locat=in                                    !jd
      END IF
C
C3------FOR FREE FORMAT CONTROL RECORD, READ REMAINING FIELDS.
      IF(IFREE.NE.0) THEN
         CALL URWORD(CNTRL,ICOL,ISTART,ISTOP,3,N,CNSTNT,IOUT,IN)
         IF(LOCAT.NE.0) THEN
            CALL URWORD(CNTRL,ICOL,ISTART,ISTOP,1,N,R,IOUT,IN)
            FMTIN=CNTRL(ISTART:ISTOP)
            IF(ICLOSE.NE.0) THEN
               IF(FMTIN.EQ.'(BINARY)') THEN
                  OPEN(UNIT=LOCAT,FILE=FNAME,FORM='UNFORMATTED')
               ELSE
                  OPEN(UNIT=LOCAT,FILE=FNAME)
               END IF
            END IF
            IF(LOCAT.GT.0 .AND. FMTIN.EQ.'(BINARY)') LOCAT=-LOCAT
            CALL URWORD(CNTRL,ICOL,ISTART,ISTOP,2,IPRN,R,IOUT,IN)
         END IF
      END IF
C
C4------TEST LOCAT TO SEE HOW TO DEFINE ARRAY VALUES.
      IF(LOCAT) 200,50,90
C
C4A-----LOCAT=0; SET ALL ARRAY VALUES EQUAL TO CNSTNT. RETURN.
   50 DO 80 I=1,II
      DO 80 J=1,JJ
   80 A(J,I)=CNSTNT
      IF(K.GT.0) WRITE(IOUT,2) ANAME,CNSTNT,K
    2 FORMAT(1X,/1X,A,' =',G15.7,' FOR LAYER',I4)
      IF(K.LE.0) WRITE(IOUT,3) ANAME,CNSTNT
    3 FORMAT(1X,/1X,A,' =',G15.7)
      RETURN
C
C4B-----LOCAT>0; READ FORMATTED RECORDS USING FORMAT FMTIN.
   90 IF(K.GT.0) THEN
         WRITE(IOUT,94) ANAME,K,LOCAT,FMTIN
   94    FORMAT(1X,///11X,A,' FOR LAYER',I4,/
     1       1X,'READING ON UNIT',I4,' WITH FORMAT: ',A)
      ELSE IF(K.EQ.0) THEN
         WRITE(IOUT,95) ANAME,LOCAT,FMTIN
   95    FORMAT(1X,///11X,A,/
     1       1X,'READING ON UNIT',I4,' WITH FORMAT: ',A)
      ELSE
         WRITE(IOUT,96) ANAME,LOCAT,FMTIN
   96    FORMAT(1X,///11X,A,' FOR CROSS SECTION',/
     1       1X,'READING ON UNIT',I4,' WITH FORMAT: ',A)
      END IF
      DO 100 I=1,II
      IF(FMTIN.EQ.'(FREE)') THEN
         READ(LOCAT,*) (A(J,I),J=1,JJ)
      ELSE
         READ(LOCAT,FMTIN) (A(J,I),J=1,JJ)
      END IF
  100 CONTINUE
      GO TO 300
C
C4C-----LOCAT<0; READ UNFORMATTED ARRAY VALUES.
  200 LOCAT=-LOCAT
      IF(K.GT.0) THEN
         WRITE(IOUT,201) ANAME,K,LOCAT
  201    FORMAT(1X,///11X,A,' FOR LAYER',I4,/
     1    1X,'READING BINARY ON UNIT',I4)
      ELSE IF(K.EQ.0) THEN
         WRITE(IOUT,202) ANAME,LOCAT
  202    FORMAT(1X,///1X,A,/
     1    1X,'READING BINARY ON UNIT',I4)
      ELSE
         WRITE(IOUT,203) ANAME,LOCAT
  203    FORMAT(1X,///1X,A,' FOR CROSS SECTION',/
     1    1X,'READING BINARY ON UNIT',I4)
      END IF
      READ(LOCAT) KSTP,KPER,PERTIM,TOTIM,TEXT,NCOL,NROW,ILAY
      READ(LOCAT) A
C
C5------IF CNSTNT NOT ZERO THEN MULTIPLY ARRAY VALUES BY CNSTNT.
  300 IF(ICLOSE.NE.0) CLOSE(UNIT=LOCAT)
      ZERO=0.
      IF(CNSTNT.EQ.ZERO) GO TO 320
      DO 310 I=1,II
      DO 310 J=1,JJ
      A(J,I)=A(J,I)*CNSTNT
  310 CONTINUE
C
C6------IF PRINT CODE (IPRN) >0 OR =0 THEN PRINT ARRAY VALUES.
  320 IF(IPRN.GE.0) CALL ULAPRW(A,ANAME,0,0,JJ,II,0,IPRN,IOUT)
C
C7------RETURN
      RETURN
C
C8------CONTROL RECORD ERROR.
  500 IF(K.GT.0) THEN
         WRITE(IOUT,501) ANAME,K
  501    FORMAT(1X,/1X,'ERROR READING ARRAY CONTROL RECORD FOR ',A,
     1     ' FOR LAYER',I4,':')
      ELSE
         WRITE(IOUT,502) ANAME
  502    FORMAT(1X,/1X,'ERROR READING ARRAY CONTROL RECORD FOR ',A,':')
      END IF
      WRITE(IOUT,'(1X,A)') CNTRL
	      call STOPFILE_MF
      STOP
      END subroutine u2drel1



