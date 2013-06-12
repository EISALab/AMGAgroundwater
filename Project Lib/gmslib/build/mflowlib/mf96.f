
      module DryHandle

        implicit none

        private
        public::GetNoDry,ReadDryVar,GetNoConvStp,GetHDryBot

        integer NoConvStp,HDryBot
        integer rs(12),ls(12)
        real NoDry
        character*320 CLine,ErrMsg

      contains

        real function GetNoDry()
          implicit none
          GetNoDry=NoDry
        end function GetNoDry

        integer function GetNoConvStp()
          implicit none
          GetNoConvStp=NoConvStp
        end function GetNoConvStp

        integer function GetHDryBot()
          implicit none
          GetHDryBot=HDryBot
        end function GetHDryBot


        subroutine ReadDryVar(IUnit)

C -- Subroutine ReadDryVar reads variables pertaining to altered
C    handling of dry conditions from the MODFLOW super file.

          implicit none
          integer, intent(in) :: IUnit
          integer ILine,IFail
          character*10 Aline

C -- First default values are supplied.

          NoDry=-1.2e37
          NoConvStp=-9999
          HDryBot=-9999

C -- Next the values for these variables are read from the MODFLOW
C    super file.

          Iline=1
          do
            Iline=Iline+1
            read(iunit,'(a)',end=100) Cline
            call LineSplit(Ifail,1)
            if(Ifail.lt.0) cycle
            call CapTrans(Cline(ls(1):rs(1)))
            if(CLine(ls(1):rs(1)).eq.'NODRY')then
              call LineSplit(IFail,2)
              if(IFail.ne.0) go to 9000
              call Realread(Ifail,Cline(ls(2):rs(2)),NoDry)
              if(IFail.ne.0) go to 9000
            else if(CLine(ls(1):rs(1)).eq.'NOCONVSTP')then
              call LineSplit(IFail,2)
              if(IFail.ne.0) go to 9000
              call IntRead(IFail,CLine(ls(2):rs(2)),NoConvStp)
              if(IFail.ne.0) go to 9000
            else if(CLine(ls(1):rs(1)).eq.'HDRYBOT') then
              call LineSplit(IFail,2)
              if(IFail.ne.0) go to 9000
              call IntRead(IFail,CLine(ls(2):rs(2)),HDryBot)
              if(IFail.ne.0) go to 9000
            end if
          end do

100       continue
          if(nodry.lt.-1.0e37)then
c            ErrMsg='No value supplied for NODRY in MODFLOW super file.'
c            go to 9900
          else if(NoConvStp.eq.-9999) then
c            ErrMsg='No value supplied for NOCONVSTP in MODFLOW '
c     +      //'super file.'
c            go to 9900
             NoConvStp=0
          else if(HDryBot.eq.-9999)then
c            ErrMsg='No value supplied for HDRYBOT in MODFLOW '
c     +      //'super file.'
c            go to 9900
             HDryBot=0
          end if

          rewind(unit=IUnit)
          read(Iunit,'(a)') Cline
          return


9000      call writint(Aline,Iline)
          ErrMsg='Error reading line '//trim(ALine)//' of MODFLOW '//
     +    'super file.'
          go to 9900


9900      call StpErr()

        end subroutine ReadDryVar




       SUBROUTINE LINESPLIT(IFAIL,NUM)
 
C -- Subroutine LINESPLIT splits a string into blank-delimited fragments.
 

        integer, intent(out) :: IFail
        integer, intent(in)  :: num
        INTEGER NW,NBLC,J,I

        IFAIL=0
        NW=0
        NBLC=len_trim(CLINE)
        IF((NBLC.NE.0).AND.(INDEX(CLINE,CHAR(9)).NE.0)) THEN
          DO I=1,LEN(CLINE)
            IF(ICHAR(CLINE(I:I)).EQ.9) CLINE(I:I)=' '
          END DO
          NBLC=len_trim(CLINE)
        ENDIF
        IF(NBLC.EQ.0) THEN
          IFAIL=-1
          RETURN
        END IF
        J=0
5       IF(NW.EQ.NUM) RETURN
        DO 10 I=J+1,NBLC
          IF(CLINE(I:I).NE.' ')GO TO 20
10      CONTINUE
        IFAIL=1
        RETURN
20      NW=NW+1
        LS(NW)=I
        DO 30 I=LS(NW)+1,NBLC
          IF(CLINE(I:I).EQ.' ')GO TO 40
30      CONTINUE
        RS(NW)=NBLC
        IF(NW.LT.NUM) IFAIL=1
        RETURN
40      RS(NW)=I-1
        J=RS(NW)
        GO TO 5
 
      END subroutine LineSplit


      subroutine StpErr

C -- Subroutine StpErr writes an error message before ceasing execution.

        implicit none

        write(*,*)
        write(*,'(1x,a)') Trim(ErrMsg)
        write(*,*)
        call STOPFILE_MF
        stop

      end subroutine StpErr



      end module DryHandle

      module Observation

        implicit none

        private
        public:: RunType,InitObserv,SetBaseFileName,ReadPmaFile,
     +           GetObsDim,ReadPointFile,ReadFluxFile,ReadMeasFile,
     +           Mod2DaySec,SetOldHeads,HeadObs,FluxObs,ModObsWrit,
     +           ReadObsVar,GetInvMod,ObsFin,debug1,debug2

        integer Ncc,Nrr,Nll
        integer db,mb,yb,hb,minb,sb,df,mf,yf,hf,minf,sf
        integer Mdays,MSecs,MDaysOld,MSecsOld
        integer FinDays,FinSecs
        integer Nhead,NFlux,Ncell,Nobs,Iss
        integer IppoUnit,IpflUnit,IpmeUnit,IpmaUnit,ImooUnit
        integer InvMod
        integer ICell
        integer IDunit          !for debugging purposes
        real TSLength
        integer ls(12),rs(12)
        integer, allocatable :: HCell1(:),HCell2(:),HeadId(:)
        integer, allocatable :: ICellNo(:)
        integer, allocatable :: Hobs1(:),Hobs2(:)
        integer, allocatable :: MeasTyp(:),FObs1(:),FObs2(:),FCell1(:),
     +                          FCell2(:),FluxId(:)
        integer, allocatable :: Days(:),Secs(:)
        real, allocatable :: CellFac(:)
        real, allocatable :: Value(:)
        real, allocatable :: HOld(:,:,:)
        character*2 TimeUnit
        character*120 BaseFile,AFile
        character*320 ErrMsg,Cline
        character*16, allocatable :: ObjTyp(:)

        contains


       subroutine RunType(Is,NCol,NRow,NLay)

C -- Subroutine RunType passes some data from MODFLOW.

         implicit none

         integer, intent(in) :: Is,NCol,NRow,NLay

         if(InvMod.eq.0) return
         Iss=Is
         Ncc=NCol
         Nrr=NRow
         Nll=NLay

         return

       end subroutine RunType



       subroutine InitObserv

C -- Subroutine InitObserv initialises certain variables in the observation
C    module

         implicit none

         if(InvMod.eq.0) return
         MDays=0
         MSecs=hb*3600+minb*60+sb
         ICell=0

         return

       end subroutine InitObserv

      subroutine SetBaseFileName(FileName)

C -- Subroutine SetBaseFileName receives the name of the MODFLOW
C    super file and extracts the basename (ie. the filename, including
C    path, minus extension).

         implicit none
         character*(*), intent(in) :: FileName
         integer n,i,NFin

         n=Len_Trim(FileName)
         NFin=max(1,n-3)
         do i=n,NFin,-1
           if(FileName(i:i).eq.'\') go to 50
           if(FileName(i:i).eq.'.') go to 100
         end do
50       i=n+1

100      BaseFile=FileName(1:i-1)

         return

      end subroutine SetBaseFileName


      subroutine ReadObsVar(IUnit)

C -- Subroutine ReadObsVar reads observation-related variables from the
C    MODFLOW super file.

          implicit none
          integer, intent(in) :: IUnit
          integer ILine,IFail
          character*10 Aline

C -- First default values are supplied.

          InvMod=-9999

C -- Next the values for these variables are read from the MODFLOW
C    super file.

          Iline=1
          do
            Iline=Iline+1
            read(iunit,'(a)',end=100) Cline
            call LineSplit(Ifail,1)
            if(Ifail.lt.0) cycle
            call CapTrans(Cline(ls(1):rs(1)))
            if(CLine(ls(1):rs(1)).eq.'INVMOD')then
              call LineSplit(IFail,2)
              if(IFail.ne.0) go to 9000
              call IntRead(Ifail,Cline(ls(2):rs(2)),InvMod)
              if(IFail.ne.0) go to 9000
            end if
          end do

100       continue
          if(InvMod.eq.-9999) then
            ErrMsg='No value supplied for INVMOD in MODFLOW '
     +      //'super file.'
            go to 9900
          end if

          rewind(unit=IUnit)
          read(Iunit,'(a)') Cline
          return

9000      call writint(Aline,Iline)
          ErrMsg='Error reading line '//trim(ALine)//' of MODFLOW '//
     +    'super file.'
          go to 9900

9900      call StpErr()

      end subroutine ReadObsVar


      Integer Function GetInvMod()

        implicit none

        GetInvMod=InvMod
        return
      end function GetInvMod


      subroutine ReadPmaFile

C -- Subroutine ReadPmaFile reads the GMS-produced PEST main file.

        implicit none
        integer NextUnit,IFail,Ierr,ILine
        character*(10) ALine

        if(InvMod.eq.0) return
        AFile=trim(BaseFile)//'.pma'
        IpmaUnit=NextUnit(Ifail)
        if(IFail.ne.0) go to 9100
        open(unit=IpmaUnit,file=AFile,status='old',Iostat=Ierr)
        if(Ierr.ne.0)then
          ErrMsg='Cannot open file '//trim(Afile)//'.'
          go to 9900
        end if

        ILine=0
        do
          Iline=Iline+1
          read(IpmaUnit,'(a)',end=100) Cline
          call LineSplit(Ifail,1)
          if(Ifail.lt.0) cycle
          call CapTrans(Cline(ls(1):rs(1)))
          if(CLine(ls(1):rs(1)).eq.'STARTTIME')then
            call remchar(cline,'/')
            call remchar(cline,':')
            call LineSplit(Ifail,7)
            if(Ifail.ne.0) go to 9000
            call IntRead(IFail,Cline(ls(2):rs(2)),mb)
            if(IFail.ne.0) go to 9000
            call IntRead(IFail,Cline(ls(3):rs(3)),db)
            if(IFail.ne.0) go to 9000
            call IntRead(IFail,Cline(ls(4):rs(4)),yb)
            if(IFail.ne.0) go to 9000
            call IntRead(IFail,CLine(ls(5):rs(5)),hb)
            if(IFail.ne.0) go to 9000
            call IntRead(IFail,CLine(ls(6):rs(6)),minb)
            if(IFail.ne.0) go to 9000
            call IntRead(IFail,Cline(ls(7):rs(7)),sb)
            if(IFail.ne.0) go to 9000
          else if(CLine(ls(1):rs(1)).eq.'ENDTIME')then
            call remchar(cline,'/')
            call remchar(cline,':')
            call LineSplit(Ifail,7)
            if(Ifail.ne.0) go to 9000
            call IntRead(IFail,Cline(ls(2):rs(2)),mf)
            if(IFail.ne.0) go to 9000
            call IntRead(IFail,Cline(ls(3):rs(3)),df)
            if(IFail.ne.0) go to 9000
            call IntRead(IFail,Cline(ls(4):rs(4)),yf)
            if(IFail.ne.0) go to 9000
            call IntRead(IFail,CLine(ls(5):rs(5)),hf)
            if(IFail.ne.0) go to 9000
            call IntRead(IFail,CLine(ls(6):rs(6)),minf)
            if(IFail.ne.0) go to 9000
            call IntRead(IFail,Cline(ls(7):rs(7)),sf)
            if(IFail.ne.0) go to 9000
          else if(CLine(ls(1):rs(1)).eq.'TIMEUNIT')then
            call LineSplit(IFail,2)
            if(IFail.ne.0) go to 9000
            call CapTrans(cline(ls(2):rs(2)))
            TimeUnit=CLine(ls(2):rs(2))
            TimeUnit=adjustl(TimeUnit)
          end if
        end do

100     continue
        close(unit=IpmaUnit)
        write(*,20) trim(AFile)
20      format('  - file ',a,' read ok.')
        return

9000    call WritInt(Aline,Iline)
        ErrMsg='Error reading line '//trim(Aline)//
     +  ' of file '//Trim(AFile)//'.'
        go to 9900
9100    ErrMsg='Cannot allocate unit number to open another file.'
        go to 9900

9900    call StpErr()

      end subroutine ReadPmaFile




      subroutine GetObsDim

C -- Subroutine GetObsDim obtains the dimensions of arrays required for
C    holding and processing observation data by reading the various
C    GMS-written files which hold this data. It then allocates memory
C    for the arrays.

        implicit none

        logical Lexist
        integer IFail,ILine,n,Itemp,IErr,NextUnit
        character*10 ALine

C -- The Point File is perused to determine the number of head
C    points and the the number of cells from which interpolation must
C    take place.

        if(InvMod.eq.0) return
        AFile=trim(BaseFile)//'.ppo'
        inquire(file=AFile,exist=Lexist)
        if(.not.Lexist)then
          nhead=0
          go to 150
        end if
        IppoUnit=NextUnit(Ifail)
        if(IFail.ne.0) go to 9600
        open(unit=IppoUnit,file=Afile,status='old',err=9200)
        NHead=0
        NCell=0
        Iline=0
        do
          Iline=Iline+1
          read(IppoUnit,'(a)',end=100) Cline
          call LineSplit(Ifail,1)
          if(Ifail.lt.0) cycle
          call CapTrans(Cline(ls(1):rs(1)))
          if(Cline(ls(1):rs(1)).eq.'BEGPT') then
            Nhead=Nhead+1
          else if(Cline(ls(1):rs(1)).eq.'INTERP')then
            call LineSplit(Ifail,2)
            if(Ifail.ne.0) go to 9400
            call IntRead(Ifail,cline(ls(2):rs(2)),n)
            if(Ifail.ne.0) go to 9400
            Ncell=Ncell+n
          end if
        end do
100     if(Nhead.eq.0) then
          close(unit=IppoUnit)
        else
          rewind(unit=IppoUnit,err=9300)
        end if

C -- The flux file is perused in order to determine the number of
C    flux objects and the number of cells involved in flux
C    summation.

150     continue
        Afile=trim(BaseFile)//'.pfl'
        inquire(file=Afile,exist=Lexist)
        if(.not.lexist)then
          NFlux=0
          go to 250
        end if
        IpflUnit=NextUnit(Ifail)
        if(Ifail.ne.0) go to 9600
        open(unit=IpflUnit,file=AFile,status='old',err=9200)
        NFlux=0
        Iline=0
        do
          Iline=Iline+1
          read(IpflUnit,'(a)',end=195) Cline
          call LineSplit(Ifail,1)
          if(Ifail.lt.0) cycle
          call CapTrans(Cline(ls(1):rs(1)))
          if(Cline(ls(1):rs(1)).eq.'BEGOBJ') then
            NFlux=NFlux+1
          else if(Cline(ls(1):rs(1)).eq.'CELLS')then
            call linesplit(Ifail,2)
            if(ifail.ne.0) go to 9400
            call Intread(Ifail,Cline(ls(2):rs(2)),n)
            if(Ifail.ne.0) go to 9400
            NCell=NCell+n
          end if
        end do
195     continue
        if(Nflux.eq.0) then
          close(unit=IpflUnit)
        else
          rewind(unit=IpflUnit,err=9300)
        end if

        if(NHead+NFlux.eq.0)then
          ErrMsg='There are no point or flux objects for current model.'
          go to 9990
        end if

C -- The measurement file is now perused in order to determine the
C    number of measurements.

250     continue
        AFile=trim(BaseFile)//'.pme'
        IpmeUnit=NextUnit(IFail)
        if(IFail.ne.0) go to 9600
        open(unit=IpmeUnit,file=afile,status='old',err=9200)
        Nobs=0
        do
          read(IpmeUnit,'(a)',end=400) Cline
          if(Cline.eq.' ') cycle
          Nobs=Nobs+1
        end do
400     continue
        if(Nobs.eq.0)then
          write(ErrMsg,410) trim(AFile)
410       format('There are no observations cited in file ',a,'.')
          go to 9990
        end if
        rewind(unit=IpmeUnit,err=9300)

500     continue

C -- Arrays are now dimensioned

        ITemp=max(NHead,1)
        allocate(HCell1(ITemp),HCell2(ITemp),HObs1(Itemp),
     +  HObs2(ITemp),HeadId(ITemp),stat=Ierr)
        if(Ierr.ne.0) go to 9500
        ITemp=max(NCell,1)
        allocate(ICellNo(ITemp),CellFac(ITemp),stat=Ierr)
        if(Ierr.ne.0) go to 9500
        ITemp=max(NObs,1)
        allocate(Days(ITemp),secs(ITemp),Value(ITemp),stat=Ierr)
        if(Ierr.ne.0) go to 9500
        ITemp=max(NFlux,1)
        allocate(MeasTyp(ITemp),FObs1(ITemp),FObs2(ITemp),
     +  FCell1(ITemp),FCell2(ITemp),ObjTyp(ITemp),FluxId(ITemp),
     +  stat=IErr)
        if(IErr.ne.0) go to 9500
        allocate(HOld(Ncc,Nrr,Nll),stat=IErr)
        if(IErr.ne.0) go to 9500
        Value=-1.23E37                  ! Value is an array

        return

9200    ErrMsg='Cannot open file '//trim(AFile)//'.'
        go to 9990
9300    ErrMsg='Cannot rewind file '//trim(AFile)//'.'
        go to 9990
9400    call WritInt(ALine,ILine)
        ErrMsg='Error reading line '//trim(ALine)//
     +  ' of file '//trim(AFile)//'.'
        go to 9990
9500    ErrMsg='Cannot allocate sufficient memory to continue '//
     +  'execution.'
        go to 9990
9600    ErrMsg='Cannot allocate a unit number to open another file.'
        go to 9990

9990    call StpErr()
      end subroutine GetObsDim


      subroutine ReadPointFile

C -- Subroutine ReadPtFile reads the contents of a GMS-produced Points File.

        implicit none

        integer IHead,ILine,NumCell,IFail
        character*10 ALine

        if(InvMod.eq.0) return
        if(NHead.eq.0) return
        AFile=trim(BaseFile)//'.ppo'

        IHead=0
        ILine=0
        NumCell=0

        do
          ILine=ILine+1
          read(IppoUnit,'(a)',end=200) CLine
          call LineSplit(IFail,1)
          if(IFail.lt.0) cycle
          if(NumCell.gt.0)then
            ICell=ICell+1
            call LineSplit(IFail,2)
            if(IFail.ne.0) go to 9400
            call IntRead(IFail,CLine(ls(1):rs(1)),ICellNo(ICell))
            if(ifail.ne.0) go to 9400
            call RealRead(IFail,CLine(ls(2):rs(2)),CellFac(ICell))
            if(ifail.ne.0) go to 9400
            NumCell=NumCell-1
          else
            call CapTrans(CLine(ls(1):rs(1)))
            if(cline(ls(1):rs(1)).eq.'BEGPT')then
              IHead=IHead+1
            else if(CLine(ls(1):rs(1)).eq.'ID')then
              call LineSplit(IFail,2)
              if(IFail.ne.0) go to 9400
              call IntRead(IFail,CLine(ls(2):rs(2)),HeadId(IHead))
              if(IFail.ne.0) go to 9400
            else if(cline(ls(1):rs(1)).eq.'INTERP')then
              call LineSplit(IFail,2)
              if(IFail.ne.0) go to 9400
              call IntRead(IFail,CLine(ls(2):rs(2)),NumCell)
              if(IFail.ne.0) go to 9400
              HCell1(IHead)=ICell+1
              HCell2(IHead)=ICell+NumCell
            end if
          end if
        end do

200     continue
        close(unit=IppoUnit)
        write(*,210) Trim(Afile)
210     format('  - file ',a,' read ok.')
        return

9400    call WritInt(ALine,ILine)
        errmsg='Error reading line '//trim(ALine)//
     +  ' of file '//trim(AFile)//'.'
        call StpErr()
      end subroutine ReadPointFile




      subroutine ReadFluxFile

C -- Subroutine ReadFluxFle reads a GMS-produced flux file.

        implicit none

        integer IFlux,ILine,NumCell,IFail
        character*10 aline

        if(InvMod.eq.0) return
        if(NFlux.eq.0) return
        AFile=trim(BaseFile)//'.pfl'

        IFlux=0
        ILine=0
        NumCell=0
        do
          ILine=ILine+1
          read(IpflUnit,'(a)',end=200) CLine
          call LineSplit(ifail,1)
          if(ifail.lt.0) cycle
          if(NumCell.gt.0)then
            ICell=ICell+1
            call LineSplit(IFail,2)
            if(IFail.ne.0) go to 9400
            call IntRead(IFail,CLine(ls(1):rs(1)),ICellNo(ICell))
            if(IFail.ne.0) go to 9400
            call RealRead(IFail,CLine(ls(2):rs(2)),CellFac(ICell))
            if(ifail.ne.0) go to 9400
            NumCell=NumCell-1
          else
            call CapTrans(CLine(ls(1):rs(1)))
            if(CLine(ls(1):rs(1)).eq.'BEGOBJ')then
              IFlux=IFlux+1
            else if(CLine(ls(1):rs(1)).eq.'ID')then
              call LineSplit(IFail,2)
              if(IFail.ne.0) go to 9400
              call IntRead(IFail,CLine(ls(2):rs(2)),FluxId(IFlux))
              if(IFail.ne.0) go to 9400
            else if(CLine(ls(1):rs(1)).eq.'MEASTYPE')then
              call LineSplit(ifail,2)
              if(ifail.ne.0) go to 9400
              call captrans(CLine(ls(2):rs(2)))
              if(Cline(ls(2):rs(2)).eq.'FLUX')then
                MeasTyp(IFlux)=0
              else if(Cline(ls(2):rs(2)).eq.'VOLUME')then
                MeasTyp(IFlux)=1
              else
                go to 9400
              end if
            else if(Cline(ls(1):rs(1)).eq.'OBJTYPE')then
              call LineSplit(ifail,2)
              if(IFail.ne.0) go to 9400
              ObjTyp(IFlux)=cline(ls(2):rs(2))
              call CapTrans(ObjTyp(IFlux))
            else if(CLine(ls(1):rs(1)).eq.'CELLS')then
              call LineSplit(IFail,2)
              if(IFail.ne.0) go to 9400
              call IntRead(IFail,CLine(ls(2):rs(2)),NumCell)
              if(ifail.ne.0) go to 9400
              FCell1(IFlux)=ICell+1
              FCell2(IFlux)=ICell+NumCell
            end if
          end if
        end do

200     close(unit=IpflUnit)
        write(*,210) trim(afile)
210     format('  - file ',a,' read ok.')
        return

9400    call WritInt(ALine,ILine)
        errmsg='Error reading line '//trim(ALine)//
     +  ' of file '//trim(AFile)//'.'
        call StpErr()

      end subroutine ReadFluxFile




      subroutine ReadMeasFile

C -- Subroutine ReadMeasFile reads a GMS-produced measurement file.

        implicit none
        integer idold,iline,iheadold,ifluxold,ihead,iflux,
     +  id,dd,mm,yy,hh,min,ss,iobs,NumDays,IFail,LastType
        character*10 ALine

        if(InvMod.eq.0) return
        IFail=0
        Iobs=0

        FinDays=NumDays(db,mb,yb,df,mf,yf)
        FinSecs=hf*3600+minf*60+sf

        AFile=trim(BaseFile)//'.pme'
        IdOld=0
        ILine=0
        IHeadOld=0
        IFluxOld=0
        IHead=0
        IFlux=0
        do
          ILine=ILine+1
          read(IpmeUnit,'(a)',end=500) CLine
          call LineSplit(Ifail,4)
          if(IFail.lt.0) then
            cycle
          else if(IFail.gt.0) then
            go to 9000
          else
            call IntRead(IFail,CLine(ls(1):rs(1)),Id)
            if(IFail.ne.0) go to 9000
            call GetDate(IFail,CLine(ls(2):rs(2)),dd,mm,yy)
            if(IFail.ne.0) go to 9000
            call GetTime(IFail,CLine(ls(3):rs(3)),hh,min,ss)
            if(ifail.ne.0) go to 9000
            Iobs=iobs+1
            Days(Iobs)=NumDays(db,mb,yb,dd,mm,yy)
            Secs(Iobs)=hh*3600+min*60+ss
            if(Id.ne.IdOld)then
              if(NHead.ne.0)then
                do IHead=1,NHead
                  if(Id.eq.HeadId(IHead))then
                    Hobs1(IHead)=Iobs
                    if(IdOld.ne.0)then
                      if(LastType.eq.1)then
                        Hobs2(IHeadOld)=Iobs-1
                      else
                        Fobs2(IFluxOld)=Iobs-1
                      end if
                    end if
                    IheadOld=IHead
                    LastType=1
                    go to 200
                  end if
                end do
              end if
              if(NFlux.eq.0) then
                go to 9100
              else
                do IFlux=1,NFlux
                  if(Id.eq.FluxId(IFlux))then
                    Fobs1(IFlux)=Iobs
                    if(IdOld.ne.0)then
                      if(LastType.eq.1)then
                        Hobs2(IHeadOld)=Iobs-1
                      else
                        Fobs2(IFluxOld)=Iobs-1
                      end if
                    end if
                    IFluxOld=IFlux
                    LastType=2
                    go to 200
                  end if
                end do
                go to 9100
              end if
            end if 
200         continue
            IdOld=Id
          end if
        end do

500     continue
        if(LastType.eq.1)then
          Hobs2(IHeadOld)=Iobs
        else
          Fobs2(IFluxOld)=Iobs
        end if

        close(unit=IpmeUnit)
        write(*,510) trim(afile)
510     format('  - file ',a,' read ok.')
        return

9000    call WritInt (ALine,ILine)
        errmsg='Error reading line '//trim(ALine)//
     +  ' of file '//trim(AFile)//'.'
        go to 9990
9100    call WritInt(ALine,ILine)
        errmsg='Unrecognised point or flux object identifier at '//
     +  'line '//trim(ALine)//' of file '//trim(AFile)//'.'
        go to 9990

9990    call StpErr()

      end subroutine ReadMeasFile





      subroutine Mod2DaySec(ToTim,Delt)

C -- Subroutine Mod2DaySec converts total ModFlow simulation time
C    to days since start of simulation day and seconds since midnight.

        Implicit none
        real, intent(in) :: ToTim,Delt
        double precision DToTim

        if(InvMod.eq.0) return
        if(TimeUnit.eq.'YR')then
          DToTim=ToTim*365.25d0
        else if(TimeUnit.eq.'D ')then
          DToTim=ToTim
        else if(TimeUnit.eq.'H ')then
          DToTim=ToTim/24.0d0
        else if(TimeUnit.eq.'M ')then
          DToTim=ToTim/1440.0d0
        else if(TimeUnit.eq.'S ')then
          DToTim=ToTim/86400.0d0
        end if

        MDaysOld=MDays
        MSecsOld=MSecs

        MDays=DToTim
        MSecs=nint((DToTim-dble(MDays))*86400.0d0)
        MSecs=MSecs+(hb*3600+minb*60+sb)
        if(MSecs.gt.86400)then
          MSecs=Msecs-86400
          MDays=MDays+1
        end if
        TSLength=Delt

        return
      end subroutine Mod2DaySec


      subroutine SetOldHeads(NCol,NRow,NLay,HNew)

C -- Subroutine SetOldHeads transfers new heads to old heads.

         implicit none
         integer, intent(in) :: NCol,NRow,NLay
         double precision, intent(in) :: HNew(NCol,NRow,NLay)

         integer ICol,IRow,ILay

         if(InvMod.eq.0) return
         do ILay=1,NLay
           do IRow=1,NRow
             do ICol=1,NCol
               HOld(ICol,IRow,ILay)=HNew(ICol,IRow,ILay)
             end do
           end do
         end do

         return

       end subroutine SetOldHeads



      subroutine HeadObs(NCol,NRow,NLay,HNew)

C -- Subroutine HEADOBS carries out time interpolation of heads from
C    model time steps to observation measurement times.

        implicit none
        integer, intent(in) :: NCol,NRow,NLay
        double precision, intent(in) :: HNew(NCol,NRow,NLay)

        integer IHead,Obs1,Obs2,IObs,Nodes
        real Rfac1,Rfac2,Rtemp1,Rtemp2

        if(InvMod.eq.0) return
        if(NHead.eq.0) return
        Nodes=Ncol*Nrow*Nlay

        if(Iss.ne.1)then
          do IHead=1,NHead
            Obs1=HObs1(IHead)
            Obs2=Hobs2(IHead)
            do IObs=Obs1,Obs2
              if((Days(IObs).lt.MDaysOld).or.
     +         ((Days(iobs).eq.MdaysOld).and.
     +          (Secs(iobs).lt.MSecsOld))) cycle
              if((Days(IObs).lt.MDays).or.
     +          ((Days(IObs).eq.MDays).and.
     +           (Secs(IObs).le.MSecs)))then
                 call SumCell(1,Nodes,HCell2(IHead)-HCell1(IHead)+1,
     +           ICellNo(HCell1(IHead)),CellFac(HCell1(IHead)),
     +           HOld,HNew,Rtemp1)
                 call SumCell(2,Nodes,HCell2(IHead)-HCell1(IHead)+1,
     +           IcellNo(HCell1(IHead)),CellFac(HCell1(IHead)),
     +           HOld,HNew,RTemp2)
                 call TimeFac(MDaysOld,MSecsOld,Mdays,Msecs,
     +           Days(iobs),Secs(iobs),RFac1,RFac2)
                 Value(iobs)=RTemp1*RFac1+RTemp2*RFac2
              end if
            end do
          end do
        else
          do IHead=1,NHead
            Obs1=HObs1(IHead)
            Obs2=HObs2(IHead)
            do IObs=Obs1,Obs2
              call SumCell(2,Nodes,HCell2(IHead)-HCell1(IHead)+1,
     +        ICellNo(HCell1(IHead)),CellFac(HCell1(IHead)),
     +        HOld,HNew,Value(Iobs))
            end do
          end do
        end if

C -- make sure the above works with initial heads and steady state.

        return
      end subroutine HeadObs




      subroutine FluxObs(NCol,NRow,NLay,Text,Buf)

C -- Subroutine FluxObs carries out time interpolation of fluxes
C    from model time steps to observation measurement times.

        implicit none

        integer, intent(in) :: NCol,NRow,NLay
        character*(16), intent(in) :: Text
        real, intent(in) :: Buf(NCol,NRow,NLay)

        integer Nodes,IFlux,Obs1,Obs2,Iobs,NStep,NFin
        character*(16) AText
        real RTemp,RFac1,RFac2
        double precision TDays,DTemp(1,1,1)

        if(InvMod.eq.0) return
        if(NFlux.eq.0) return
        Nodes=NCol*NRow*NLay
        AText=Text
        call CapTrans(AText)
        
        if(Iss.ne.1)then
          do IFlux=1,NFlux
c            if(ObjTyp(IFlux).ne.AText) cycle
            if(index(Atext,trim(ObjTyp(IFlux))).eq.0) cycle
            Obs1=FObs1(IFlux)
            Obs2=FObs2(IFlux)
            if(MeasTyp(IFlux).eq.0)then
              do IObs=Obs1,Obs2
                if((Days(IObs).lt.MDaysOld).or.
     +            ((Days(IObs).eq.MDaysOld).and.
     +            (Secs(IObs).lt.MSecsOld))) go to 400
                if((Days(IObs).lt.MDays).or.
     +            ((Days(IObs).eq.MDays).and.
     +             (Secs(Iobs).le.MSecs))) then
                   call SumCell(1,Nodes,FCell2(IFlux)-FCell1(IFlux)+1,
     +             ICellNo(FCell1(IFlux)),CellFac(FCell1(IFlux)),
     +             Buf,DTemp,Value(Iobs))
                   go to 400
                 end if
400              continue
              end do
            else
              do IObs=FObs1(IFlux),FObs2(IFlux)-1,2
                if((Days(IObs).lt.0).or.
     +            ((Days(IObs).eq.0).and.
     +             (Secs(IObs).lt.0))) then
                    Value(IObs)=-2.23e37
                    go to 450
                end if
                if((Days(IObs+1).gt.FinDays).or.
     +            ((Days(IObs+1).eq.FinDays).and.
     +             (Secs(IObs+1).gt.FinSecs))) then
                   Value(IObs)=-2.23e37
                   go to 450
                 end if
                if((Days(IObs+1).lt.MDaysOld).or.
     +            ((Days(IObs+1).eq.MDaysOld).and.
     +             (Secs(IObs+1).lt.MSecsOld))) go to 450
                if((Days(IObs).lt.MDays).or.
     +            ((Days(IObs).eq.MDays).and.
     +             (Secs(IObs).le.MSecs))) then
                     call SumCell(1,Nodes,FCell2(IFlux)-FCell1(IFlux)+1,
     +               ICellNo(FCell1(IFlux)),CellFac(FCell1(IFlux)),
     +               Buf,DTemp,RTemp)
                     if(Value(Iobs+1).gt.-1.0e35)then
                       Value(IObs+1)=Value(Iobs+1)+Rtemp*TSLength
                     else
                       call TimeFac(MDaysOld,MSecsOld,MDays,MSecs,
     +                 Days(IObs),Secs(IObs),RFac1,RFac2)
                       Value(IObs+1)=Rtemp*RFac1*TSLength
                     end if
                end if
                if((Days(IObs+1).lt.MDays).or.
     +            ((Days(IObs+1).eq.MDays).and.
     +            (Secs(IObs+1).le.MSecs))) then
                   call SumCell(1,Nodes,FCell2(IFlux)-FCell1(IFlux)+1,
     +             ICellNo(FCell1(IFlux)),CellFac(FCell1(IFlux)),
     +             Buf,DTemp,RTemp)
                  call TimeFac(MDaysOld,MSecsOld,MDays,MSecs,
     +            Days(IObs+1),Secs(IObs+1),RFac1,RFac2)
                  Value(IObs+1)=Value(IObs+1)-RTemp*TSLength*RFac1
                  Value(IObs)=-2.23e37
                end if
450             continue
              end do
            end if
          end do
        else
          do IFlux=1,NFlux
c            if(ObjTyp(IFlux).ne.AText) cycle
            if(index(Atext,trim(ObjTyp(IFlux))).eq.0) cycle
            Obs1=FObs1(IFlux)
            Obs2=FObs2(IFlux)
            if(MeasTyp(IFlux).eq.0)then
              NStep=1
              NFin=FObs2(IFlux)
            else
              NStep=2
              NFin=FObs2(IFlux)-1
            end if
            do IObs=FObs1(IFlux),NFin,NStep
              call SumCell(1,Nodes,FCell2(IFlux)-FCell1(IFlux)+1,
     +        ICellNo(FCell1(IFlux)),CellFac(FCell1(IFlux)),
     +        Buf,DTemp,RTemp)
              if(MeasTyp(IFlux).eq.0) then
                Value(IObs)=RTemp
              else
                TDays=Days(Iobs+1)-Days(IObs)+
     +                (Secs(IObs+1)-Secs(IObs))/86400.0d0
                if(TimeUnit.eq.'YR')then
                  TDays=TDays/365.25
                else if(TimeUnit.eq.'D ')then
                  TDays=TDays*1.0d0
                else if(TimeUnit.eq.'H ')then
                  TDays=TDays*24.0d0
                else if(TimeUnit.eq.'M ')then
                  TDays=TDays*1440d0
                else if(TimeUnit.eq.'S ')then
                  TDays=TDays*86400.0d0
                end if
                Value(IObs+1)=RTemp*TDays
                Value(IObs)=-2.23e37
              end if
            end do
          end do
        end if

        return
      end subroutine FluxObs



      subroutine ModObsWrit

C -- Subroutine MODOBSWRIT writes to an output file the model-generated
C    counterparts of observations.

        implicit none

        integer IObs,IFlux,IHead,id,dd,mm,IFail,
     +  yy,hh,min,ss
        character*10 ATemp

        integer Nextunit

        if(InvMod.eq.0) return
        AFile=trim(BaseFile)//'.moo'
        ImooUnit=NextUnit(IFail)
        if(IFail.ne.0) go to 9000
        open(unit=ImooUnit,file=AFile,err=9100)

        do IObs=1,NObs
          if(NHead.ne.0)then
            do IHead=1,NHead
              if((IObs.ge.HObs1(IHead)).and.
     +           (IObs.le.HObs2(IHead)))then
                Id=HeadId(IHead)
                go to 20
              end if
            end do
            if(NFlux.eq.0)go to 9200
          end if
          do IFlux=1,NFlux
            if((IObs.ge.FObs1(IFlux)).and.
     +         (IObs.le.FObs2(IFlux)))then !check fobs2(nflux) if meastype=2
              Id=FluxId(IFlux)
              go to 20
            end if
          end do
          go to 9200

20        continue
          call Days2Date(days(iobs),db,mb,yb,dd,mm,yy)
          call Secs2Time(secs(iobs),hh,min,ss)
c          if(Value(Iobs).le.-2.0e37) cycle
          if(Value(Iobs).le.-2.0e37) Value(Iobs)=0.0d0
          write(ImooUnit,30,err=9100) id,mm,dd,yy,hh,min,ss,value(iobs)
30        format(i6,1x,i2.2,'/',i2.2,'/',i4.4,'  ',i2.2,':',i2.2,
     +    ':',i2.2,'   ',1pg14.7)
        end do

        close(unit=ImooUnit)
        write(*,100) trim(Afile)
100     format(' - file ',a,' written ok')
        return


9000    ErrMsg='Cannot allocate unit number to open another file.'
        go to 9900
9100    ErrMsg='Cannot open file '//trim(AFile)//' for output.'
        go to 9900
9200    call WritInt(Atemp,IObs)
        ErrMsg='Cannot find head/flux object for observation number '
     +  //trim(ATemp)//'.'
        go to 9900


9900    continue
        Call StpErr

      end subroutine ModObsWrit
        


      subroutine StpErr

C -- Subroutine StpErr writes an error message before ceasing execution.

        implicit none
        integer IErr
        logical LOpened

        write(*,*)
        write(*,'(1x,a)') Trim(ErrMsg)
        write(*,*)

        inquire(unit=Ipmaunit,opened=lopened)
        if(lopened) close(unit=Ipmaunit)
        inquire(unit=IppoUnit,opened=lopened)
        if(lopened) close(unit=IppoUnit)
        inquire(unit=IpflUnit,opened=lopened)
        if(lopened) close(unit=IpflUnit)
        inquire(unit=Ipmeunit,opened=lopened)
        if(lopened) close(unit=Ipmeunit)
        inquire(unit=Imoounit,opened=lopened)
        if(lopened) close(unit=Imoounit)

        deallocate(HCell1,HCell2,HeadId,ICellNo,HObs1,HObs2,
     +  MeasTyp,FObs1,FObs2,FCell1,FCell2,FluxId,Days,Secs,CellFac,
     +  Value,stat=IErr)

        call STOPFILE_MF
        stop

      end subroutine StpErr


      Subroutine ObsFin()

C -- Subroutine ObsFin deallocates arrays used by the observation
C    module.

        implicit none
        integer IErr

        if(InvMod.eq.0) return
        deallocate(HCell1,HCell2,HeadId,ICellNo,HObs1,HObs2,
     +  MeasTyp,FObs1,FObs2,FCell1,FCell2,FluxId,Days,Secs,CellFac,
     +  Value,stat=IErr)
        return

      end subroutine ObsFin


       SUBROUTINE LINESPLIT(IFAIL,NUM)
 
C -- Subroutine LINESPLIT splits a string into blank-delimited fragments.
 

        integer, intent(out) :: IFail
        integer, intent(in)  :: num
        INTEGER NW,NBLC,J,I

        IFAIL=0
        NW=0
        NBLC=len_trim(CLINE)
        IF((NBLC.NE.0).AND.(INDEX(CLINE,CHAR(9)).NE.0)) THEN
          DO I=1,LEN(CLINE)
            IF(ICHAR(CLINE(I:I)).EQ.9) CLINE(I:I)=' '
          END DO
          NBLC=len_trim(CLINE)
        ENDIF
        IF(NBLC.EQ.0) THEN
          IFAIL=-1
          RETURN
        END IF
        J=0
5       IF(NW.EQ.NUM) RETURN
        DO 10 I=J+1,NBLC
          IF(CLINE(I:I).NE.' ')GO TO 20
10      CONTINUE
        IFAIL=1
        RETURN
20      NW=NW+1
        LS(NW)=I
        DO 30 I=LS(NW)+1,NBLC
          IF(CLINE(I:I).EQ.' ')GO TO 40
30      CONTINUE
        RS(NW)=NBLC
        IF(NW.LT.NUM) IFAIL=1
        RETURN
40      RS(NW)=I-1
        J=RS(NW)
        GO TO 5
 
      END subroutine LineSplit
 

      subroutine debug1

        implicit none
        integer nextunit,ifail,i

        idunit=nextunit(ifail)
        open(unit=idunit,file='debug.dat')
        write(idunit,10) Mdays,MSecs
10      format(' Mdays = ',i4,'  MSecs = ',i4)
        write(idunit,20) MDaysOld,MSecsOld
20      format(' MDaysOld = ',i4,' MSecsOld = ',i4)
        write(idunit,30) ICell
30      format(' ICell = ',i4)
        write(idunit,40) 'Ncol Nrow Nlay Iss = ',Ncc,Nrr,Nll,Iss
40      format(1x,a,4i5)
        write(idunit,60) db,mb,yb,hb,minb,sb
60      format(' Starting time is ',6i5)
        write(idunit,70) df,mf,yf,hf,minf,sf
70      format(' Finishing time is ',6i5)
        write(idunit,80) Timeunit
80      format(' Time unit = ',a)

        write(idunit,90) Nhead
90      format(' Nhead = ',i5)
        write(idunit,100) NCell
100     format(' NCell = ',i5)
        write(idunit,110) Nobs
110     format(' Nobs = ',i5)
        write(idunit,120) NFlux
120     format(' NFlux = ',i5)

        write(idunit,*)
        write(idunit,130)
130     format(' HCell1  HCell1  HObs1  HObs2  HeadId')
        do i=1,NHead
          write(idunit,140) Hcell1(i),Hcell2(i),HObs1(i),Hobs2(i),
     +    Headid(i)
140       format(1x,5i5)
        end do

        write(idunit,*)
        write(idunit,150)
150     format(' ICellNo  CellFac')
        do i=1,Ncell
          write(idunit,160) Icellno(i), cellfac(i)
160       format(1x,i6,1x,1pg13.6)
        end do

        write(idunit,*)
        write(idunit,170)
170     format(' Meastyp Fobs1 Fobs2 FCell1  FCell2 FluxId  OBjTyp')
        do i=1,NFlux
          write(idunit,180) Meastyp(i),Fobs1(i),fobs2(i),
     +    Fcell1(i),fcell2(i),fluxid(i),trim(objtyp(i))
180       format(1x,6i5,1x,a)
        end do

        write(idunit,*)
        write(idunit,190)
190     format(' Days  Secs   Value')
        do i=1,nobs
          write(idunit,200) days(i),secs(i),value(i)
200       format(1x,2i7,2x,1pg14.6)
        end do

        return

      end subroutine debug1


      subroutine debug2

        implicit none

        write(idunit,*)
        write(idunit,10) MDays,MSecs,MDaysOld,MSecsOld
10      format(' MDays MSecs MDaysOld MSecsOld = ',4i7)
        write(idunit,20) TSLength
20      format(' TsLength = ',1pg14.6)
c        write(idunit,30) db,mb,yb,hb,minb,sb
c30      format(' Startime = ',6i6)
        write(idunit,40) hold(12,5,1)
40      format(' hold(12,5,1) = ',1pg14.6)

      end subroutine debug2


      end module Observation


      module How_to_Stop

        implicit none

        private
        public::KillApp,GetStayOpen

        character*10 StayOpen

      contains

        subroutine GetStayOpen(ATemp)
          character*(*) ATemp
          StayOpen=Adjustl(ATemp)
          if(StayOpen(1:1).eq.'"')then
            StayOpen(1:1)=' '
            StayOpen=Adjustl(StayOpen)
          end if
          return
        end subroutine GetStayOpen

        subroutine KillApp()
          character*1 dummyvar
          if(StayOpen.eq.' ')return
          if(StayOpen(1:1).eq.'0')return
          write(*,10)
10        format(/,'  Press ENTER to quit.')
          read(*,'(a)',err=20,end=20) dummyvar
20        return
        end subroutine KillApp

      end module How_to_Stop

      module TrueLayer

        implicit none
        private
        public::ReadLayVar,SetLayDim,ReadLayDat,
     +  LayArrFin,SetStartHeads,ReReadLayer,SetLayRead

        integer Ncc,Nrr,Nll
        integer TrueLay,VCont1,ReadLayerDat,ICurrentLayer,ICurrentLaycon
        integer rs(12),ls(12)
        real, allocatable :: Bot(:,:,:),Top(:,:),Kh(:,:,:),Kv(:,:,:),
     +  S1(:,:,:),S2(:,:,:),SHead1(:,:)
        character*120 LayFile
        character*320 CLine,ErrMsg

      contains


       subroutine ReadLayVar(IUnit,Path)

C -- Subroutine ReadLayVar reads variables pertaining to MODFLOW
C    "truelay" functionality from the MODFLOW super file.

          implicit none
          integer, intent(in) :: IUnit
          character*(*), intent(in) :: Path
          integer ILine,IFail
          character*10 Aline

C -- First initial values are supplied.

          TrueLay=-9999
          VCont1=-9999
          Layfile=' '
          ReadLayerDat=0

C -- Next the values for these variables are read from the MODFLOW
C    super file.

          Iline=1
          do
            Iline=Iline+1
            read(iunit,'(a)',end=100) Cline
            call LineSplit(Ifail,1)
            if(Ifail.lt.0) cycle
            call CapTrans(Cline(ls(1):rs(1)))
            if(CLine(ls(1):rs(1)).eq.'LAY')then
              call remchar(cline,'"')
              call remchar(cline,'''')
              call LineSplit(IFail,2)
              if(IFail.ne.0) cycle
              LayFile=cline(ls(2):)
              if(index(LayFile,'\').eq.0)then
                LayFile=trim(Path)//trim(LayFile)
              end if
            else if(CLine(ls(1):rs(1)).eq.'USESHEAD')then
              call LineSplit(IFail,2)
              if(IFail.ne.0) go to 9000
              call IntRead(IFail,CLine(ls(2):rs(2)),VCont1)
              if(IFail.ne.0) go to 9000
            end if
          end do

100       continue
          if(LayFile.eq.' ')then
            TrueLay=0
          else
            TrueLay=1
            if(VCont1.eq.-9999)then
              Errmsg='No value supplied for USESHEAD in MODFLOW '//
     +        'Super File.'
              go to 9900
            else if((VCont1.ne.0).and.(VCont1.ne.1))then
              ErrMsg='Illegal value supplied for USESHEAD in '//
     +        'MODFLOW Super File.'
              go to 9900
            end if
          end if

          rewind(unit=IUnit)
          read(Iunit,'(a)') Cline
          return

9000      call writint(Aline,Iline)
          ErrMsg='Error reading line '//trim(ALine)//' of MODFLOW '//
     +    'super file.'
          go to 9900


9900      call StpErr()

       end subroutine ReadLayVar


       subroutine SetLayDim(ITemp1,ITemp2,ITemp3)

         implicit none
         integer, intent(in):: ITemp1,ITemp2,ITemp3
         integer IErr,k,Ilay

         Ncc=ITemp1
         Nrr=ITemp2
         Nll=ITemp3

         if(TrueLay.eq.1)then
           allocate(Bot(Ncc,Nrr,Nll),Top(Ncc,Nrr),
     +     Kh(Ncc,Nrr,Nll),Kv(Ncc,Nrr,Nll),S1(Ncc,Nrr,Nll),
     +     S2(Ncc,Nrr,Nll),SHead1(Ncc,Nrr),stat=Ierr)
           if(Ierr.ne.0) then
             ErrMsg='Cannot allocate memory for storage of arrays '//
     +       'to implement TRUELAY option.'
             call StpErr()
           end if
           do k=1,Nll
             Bot(1,1,k)=-1.123e37
             Kh(1,1,k)=-1.123e37
             Kv(1,1,k)=-1.123e37
             S1(1,1,k)=-1.123e37
	       S2(1,1,k)=-1.123e37
           end do
           Top(1,1)=-1.123e37
           SHead1(1,1)=-1.123e37
         else
           allocate(Bot(1,1,1),Top(1,1),Kh(1,1,1),Kv(1,1,1),S1(1,1,1),
     +     S2(1,1,1),SHead1(1,1),stat=Ierr)
           if(Ierr.ne.0)then
             ErrMsg='Memory allocation error.'
             call StpErr()
           end if
         end if

         return
       end subroutine SetLayDim


       subroutine ReadLayDat(Iout)

         implicit none
         integer, intent(in)::IOut
         integer NextUnit,IFail,LayUnit,Ierr,ILay,ICol,IRow
         character*10 LayType
         character*24 AName

         if(Truelay.eq.0) return
         LayUnit=NextUnit(IFail)
         if(IFail.ne.0)then
           ErrMsg='Cannot allocate a unit number to read true layer '//
     +     'data.'
           go to 9900
         end if
         open(unit=LayUnit,file=LayFile,status='old',iostat=Ierr)
         if(Ierr.ne.0)then
           ErrMsg='Cannot open true layer file '//trim(LayFile)//'.'
           go to 9900
         end if

50       read(LayUnit,'(a)',err=9000,end=9100) Cline
         if(CLine.eq.' ') go to 50
         call captrans(CLine)
         if(CLine(1:6).ne.'LAYER ')then
           ErrMsg='File '//trim(LayFIle)//' is not a GMS layer '//
     +     'file.'
           go to 9900
         end if

60       read(LayUnit,'(a)',err=9000,end=200) CLine
         if(Cline.eq.' ') go to 60
         call captrans(Cline)
         if(CLine(1:4).eq.'LAY ')then
           call LineSplit(IFail,2)
           if(IFail.ne.0) go to 9200
           call IntRead(IFail,CLine(ls(2):rs(2)),ILay)
           if(IFail.ne.0) go to 9200
           if(LayType(1:7).eq.'TOPTYPE')then
             AName='True Layer Top'
             call u2drel1(Top,AName,Nrr,Ncc,1,LayUnit,Iout)
           else if(LayType(1:7).eq.'BOTTYPE')then
             AName='True Layer  Bottom'
             call u2drel1(Bot(1,1,ILay),AName,Nrr,Ncc,ILay,LayUnit,IOut)
           else if(LayType(1:7).eq.'KH_TYPE')then
             AName='True Layer Kh'
             call u2drel1(Kh(1,1,Ilay),AName,Nrr,Ncc,Ilay,LayUnit,IOut)
           else if(LayType(1:7).eq.'KV_TYPE')then
             AName='True Layer  Kv'
             call u2drel1(Kv(1,1,Ilay),AName,Nrr,Ncc,ILay,LayUnit,IOut)
           else if(LayType(1:9).eq.'SPEC_STOR')then
             AName='True Layer S1'
             call u2drel1(S1(1,1,Ilay),Aname,Nrr,Ncc,ILay,LayUnit,IOut)
           else if(LayType(1:10).eq.'SPEC_YIELD')then
             AName='True Layer Sy'
             call u2drel1(S2(1,1,Ilay),AName,Nrr,Ncc,ILay,LayUnit,IOut)
           else
             ErrMsg='Unknown data type "'//Trim(LayType)//
     +       '" in GMS layer file '//trim(LayFile)//'.'
             go to 9900
           end if
         else
           LayType=trim(CLine)
         end if
         go to 60

200      continue
         close(unit=LayUnit)
         return

9000     Errmsg='Error reading true layer file '//trim(LayFile)//'.'
         go to 9900
9100     Errmsg='Unexpected end encountered to true layer file '//
     +   trim(LayFile)//'.'
         go to 9900
9200     ErrMsg='Error reading layer number from file '//
     +   trim(LayFile)//'.'
         go to 9900


9900     Call StpErr()

       end subroutine ReadLayDat


       subroutine SetStartHeads(HOld)

         implicit none
         Real, intent(in) ::HOld(Ncc,Nrr,Nll)
         integer IRow,ICol

         if(TrueLay.eq.0) return

         do IRow=1,Nrr
           do Icol=1,Ncc
             SHead1(ICol,IRow)= HOld(ICol,IRow,1)
           end do
         end do

         return
       end subroutine SetStartHeads




       subroutine GetSC1(k,LayCon,Stor)

         implicit none
         integer, intent(in)::k,LayCon
         integer ICol,IRow
         real Thick
         real, intent(out) :: Stor(Ncc,Nrr)

         if(TrueLay.eq.0) return
         if(LayCon.eq.1)return

         call IsRead(S1(1,1,k),k,'S1')
         if(k.eq.1)then
           call IsRead(Top(1,1),0,'Top')
         else
           call IsRead(Bot(1,1,k-1),0,'Bottom')
         end if
         call IsRead(Bot(1,1,k),k,'Bottom')

         do irow=1,Nrr
           do icol=1,Ncc
             if(k.eq.1)then
               Thick=Top(icol,irow)-Bot(ICol,IRow,k)
             else
               Thick=Bot(ICol,IRow,k-1)-Bot(ICol,IRow,k)
             end if
             Stor(icol,irow)=S1(icol,irow,k)*Thick
           end do
         end do

         return
       end subroutine GetSC1


       subroutine GetTrans(k,Trans)

         implicit none
         integer, intent(in)::k
         integer ICol,IRow
         real Thick
         real, intent(out) :: Trans(Ncc,Nrr)

         if(TrueLay.eq.0) return
         call IsRead(Kh(1,1,k),k,'Kh')
         if(k.eq.1)then
           call IsRead(Top(1,1),0,'Top')
         else
           call IsRead(Bot(1,1,k-1),0,'Bottom')
         end if
         call IsRead(Bot(1,1,k),k,'Bottom')

         do irow=1,Nrr
           do icol=1,Ncc
             if(k.eq.1)then
               Thick=Top(icol,irow)-Bot(ICol,IRow,k)
             else
               Thick=Bot(ICol,IRow,k-1)-Bot(ICol,IRow,k)
             end if
             Trans(icol,irow)=Kh(icol,irow,k)*Thick
           end do
         end do

         return
       end subroutine GetTrans

       subroutine GetKh(k,TheKh)   !If truelayer, always read from layer

         implicit none                          !jig KHFIX
         integer, intent(in)::k                 !jig KHFIX
         integer ICol,IRow                      !jig KHFIX
         real, intent(out) :: TheKh(Ncc,Nrr)    !jig KHFIX

         if(TrueLay.eq.0) return                !jig KHFIX
         call IsRead(Kh(1,1,k),k,'Kh')          !jig KHFIX

         do irow=1,Nrr                          !jig KHFIX
           do icol=1,Ncc                        !jig KHFIX
             TheKh(icol,irow)=Kh(icol,irow,k)   !jig KHFIX
           end do                               !jig KHFIX
         end do                                 !jig KHFIX

         return                                 !jig KHFIX
       end subroutine GetKh                     !jig KHFIX

       subroutine GetBottom(k,TheBottom)  !If truelayer, always read from layer

         implicit none                              !jig KHFIX
         integer, intent(in)::k                     !jig KHFIX
         integer ICol,IRow                          !jig KHFIX
         real, intent(out) :: TheBottom(Ncc,Nrr)    !jig KHFIX

         if(TrueLay.eq.0) return                    !jig KHFIX
         call IsRead(Bot(1,1,k),k,'Bottom')         !jig KHFIX

         do irow=1,Nrr                              !jig KHFIX
           do icol=1,Ncc                            !jig KHFIX
             TheBottom(icol,irow)=Bot(icol,irow,k)  !jig KHFIX
           end do                                   !jig KHFIX
         end do                                     !jig KHFIX

         return                                     !jig KHFIX
       end subroutine GetBottom                     !jig KHFIX

       subroutine GetTop(k,TheTop)  !If truelayer, always read from layer

         implicit none                                !jig KHFIX
         integer, intent(in)::k                       !jig KHFIX
         integer ICol,IRow                            !jig KHFIX
         real, intent(out) :: TheTop(Ncc,Nrr)         !jig KHFIX

         if(TrueLay.eq.0) return                      !jig KHFIX
         if (k.eq.1) then
           call IsRead(Top(1,1),0,'Top')
         else
           call IsRead(Bot(1,1,k-1),0,'Bottom')       !jig KHFIX
         endif

         do irow=1,Nrr                                !jig KHFIX
           do icol=1,Ncc                              !jig KHFIX
             if (k.eq.1) then
               TheTop(icol,irow)=Top(icol,irow)       !jig KHFIX
             else
               TheTop(icol,irow)=Bot(icol,irow,k-1)
             endif
           end do                                     !jig KHFIX
         end do                                       !jig KHFIX

         return                                       !jig KHFIX
       end subroutine GetTop                          !jig KHFIX


      subroutine GetSC2(k,LayCon,Stor)                     !jig KHFIX

         implicit none                                     !jig KHFIX
         integer, intent(in)::k,LayCon                     !jig KHFIX
         integer ICol,IRow                                 !jig KHFIX
         real Thick                                        !jig KHFIX
         real, intent(out) :: Stor(Ncc,Nrr)                !jig KHFIX

         if(TrueLay.eq.0) return                           !jig KHFIX

         call IsRead(S2(1,1,k),k,'S2')                      !jig KHFIX

         do irow=1,Nrr                                     !jig KHFIX
           do icol=1,Ncc                                   !jig KHFIX
             Stor(icol,irow)=S2(icol,irow,k)                !jig KHFIX
           end do                                          !jig KHFIX
         end do                                            !jig KHFIX

         return                                            !jig KHFIX
       end subroutine GetSC2                               !jig KHFIX



       subroutine GetVCont(k,VCont)

         implicit none
         integer, intent(in)::k
         integer ICol,IRow
         real Thick1,Thick2,Den
         real, intent(out) :: VCont(Ncc,Nrr)

         if(TrueLay.eq.0) return
         if(k.eq.1)then
           if(Vcont1.eq.0)then
             call IsRead(Top(1,1),0,'Top')
           end if
         else
           call IsRead(Bot(1,1,k-1),k-1,'Bottom')
         end if
         call ISread(Bot(1,1,k),k,'Bottom')
         call ISRead(Bot(1,1,k+1),k+1,'Bottom')
         call IsRead(Kv(1,1,k),k,'Kv')
         call IsRead(Kv(1,1,k+1),k+1,'Kv')
         
         do irow=1,Nrr
           do icol=1,Ncc
             if(k.eq.1)then
               if(VCont1.eq.0)then
                 Thick1=(Top(icol,irow)-Bot(ICol,IRow,k))*0.5
               else
                 Thick1=(SHead1(ICol,Irow)-Bot(ICol,IRow,k))*0.5
               end if
             else
               Thick1=(Bot(ICol,IRow,k-1)-Bot(ICol,IRow,k))*0.5
             end if
             Thick2=(Bot(ICol,IRow,k)-Bot(ICol,IRow,K+1))*0.5
             Den=Kv(ICol,IRow,k+1)*Thick1+Kv(ICol,IRow,k)*Thick2
             if(Den.ne.0.0)
     +       VCont(ICol,IRow)=Kv(Icol,IRow,k)*Kv(ICol,IRow,k+1)/Den
           end do
         end do

         return
       end subroutine GetVCont


       subroutine IsRead(Value,Layer,Astring)

         implicit none
         real, intent(in) :: Value
         integer, intent(in) :: Layer
         character*(*), intent(in) :: Astring
         character*5 Anum

         if((Value.lt.-1.122e37).and.(Value.gt.-1.124e37)) then
           call writint(anum,Layer)
           if(Layer.eq.0)then
             Errmsg=trim(Astring)//' array not read from GMS '//
     +       'layer file.'
             call StpErr
           else
             Errmsg=trim(Astring)//' array for layer '//trim(anum)//
     +       ' not read from GMS layer file.'
             call StpErr
           end if
         end if
         return
       end subroutine IsRead


       subroutine SetLayRead(i,k,lk)

         implicit none
         integer, intent(in) :: i,k,lk

         ReadLayerDat=i
         ICurrentLayer=k
         ICurrentLaycon=lk

         return
       end subroutine SetLayRead




       subroutine ReReadlayer(rarray)

         implicit none
         integer k,lk
         real rarray(Ncc,Nrr)

         if(ReadlayerDat.eq.1)then
           k=ICurrentLayer
           lk=ICurrentLaycon
           if (lk.ne.1) then
             call GetSc1(k,lk,Rarray)
           else
             call GetSc2(k,lk,Rarray)
           endif
         else if(ReadlayerDat.eq.2)then
           k=ICurrentLayer
           call GetTrans(k,Rarray)
         else if(ReadLayerDat.eq.3)then
           k=ICurrentLayer
           call GetVCont(k,Rarray)
         else if(ReadLayerDat.eq.4)then             !jig KHFIX
           k=ICurrentLayer                          !jig KHFIX
           call GetKh(k,Rarray)                     !jig KHFIX
         else if(ReadLayerDat.eq.5)then              !jig KHFIX
           k=ICurrentLayer                           !jig KHFIX
           call GetBottom(k,Rarray)                  !jig KHFIX
         else if(ReadLayerDat.eq.6)then             !jig KHFIX
           k=ICurrentLayer                          !jig KHFIX
           lk=ICurrentLaycon                        !jig KHFIX
           call GetSc2(k,lk,Rarray)                 !jig KHFIX
         else if(ReadLayerDat.eq.7)then              !jig KHFIX
           k=ICurrentLayer                           !jig KHFIX
           call GetTop(k,Rarray)                     !jig KHFIX
         end if
         ReadLayerDat=0

         return
       end subroutine ReReadLayer



       subroutine LayArrFin()

         implicit none
         integer IErr

         deallocate(Bot,Top,Kh,Kv,S1,S2,SHead1,stat=Ierr)

         return
       end subroutine LayArrFin



       SUBROUTINE LINESPLIT(IFAIL,NUM)
 
C -- Subroutine LINESPLIT splits a string into blank-delimited fragments.
 

         integer, intent(out) :: IFail
         integer, intent(in)  :: num
         INTEGER NW,NBLC,J,I

         IFAIL=0
         NW=0
         NBLC=len_trim(CLINE)
         IF((NBLC.NE.0).AND.(INDEX(CLINE,CHAR(9)).NE.0)) THEN
           DO I=1,LEN(CLINE)
             IF(ICHAR(CLINE(I:I)).EQ.9) CLINE(I:I)=' '
           END DO
           NBLC=len_trim(CLINE)
         ENDIF
         IF(NBLC.EQ.0) THEN
           IFAIL=-1
           RETURN
         END IF
         J=0
5        IF(NW.EQ.NUM) RETURN
         DO 10 I=J+1,NBLC
           IF(CLINE(I:I).NE.' ')GO TO 20
10       CONTINUE
         IFAIL=1
         RETURN
20       NW=NW+1
         LS(NW)=I
         DO 30 I=LS(NW)+1,NBLC
           IF(CLINE(I:I).EQ.' ')GO TO 40
30       CONTINUE
         RS(NW)=NBLC
         IF(NW.LT.NUM) IFAIL=1
         RETURN
40       RS(NW)=I-1
         J=RS(NW)
         GO TO 5
 
       END subroutine LineSplit


       subroutine StpErr

C -- Subroutine StpErr writes an error message before ceasing execution.

         implicit none

         write(*,*)
         write(*,'(1x,a)') Trim(ErrMsg)
         write(*,*)
         call STOPFILE_MF
         stop

       end subroutine StpErr



      end module TrueLayer

C     ******************************************************************
C     MAIN CODE FOR U.S. GEOLOGICAL SURVEY MODULAR MODEL -- MODFLOW-96
C           BY MICHAEL G. MCDONALD AND ARLEN W. HARBAUGH
C     MODFLOW-88 documented in:
C           McDonald, M.G. and Harbaugh, A.W., 1988, A modular
C           three-dimensional finite-difference ground-water flow
C           model: U.S. Geological Survey Techniques of Water
C           Resources Investigations, Book 6, Chapter A1, 586 p.
C     MODFLOW-96 documented in:
C           Harbaugh, A.W. and McDonald, M.G., 1996, User's
C           documentation for the U.S. Geological Survey modular
C           finite-difference ground-water flow model: U.S. Geological
C           Survey Open-File Report 96-485
C-----VERSION 0950 23MAY1996 MAIN
C-----VERSION 1401 03DEC1996 -- added PCG2, STR1, IBS1, CHD1, GFD1,
C                               HFB1, TLK1, DE45, and RES1 as documented
C                               in USGS reports
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
C1------SPECIFY THE SIZE OF THE X ARRAY.  TO CHANGE THE SIZE OF THE
C1------X ARRAY, CHANGE VALUE OF LENX IN THE NEXT STATEMENT.
C-----EMRL COMMENT - ADD DFLIB LIBRARY
c emrl unix      USE DFPORT                                               !jd
C#########################Patched by shengquan Yan 11/25/01########################
	subroutine ModFlowEntry(strSuperFile)
C#########################Patch finished###########################################
      use Observation                                          !jd
      use DryHandle                                            !jd
      use TrueLayer                                            !jd
      use How_to_Stop                                          !jd
C#########################Patched by shengquan Yan 11/25/01########################
	character(*), intent(in):: strSuperFile
C#########################Patch finished###########################################
      character*10 Command_Line                                !jd
      PARAMETER (LENX=10000000)
      COMMON X(LENX)
      COMMON /FLWCOM/LAYCON(200)
      CHARACTER*16 VBNM(40)
      CHARACTER*80 HEADNG(2)
      DIMENSION VBVL(4,40),IUNIT(40)
      DOUBLE PRECISION DUMMY
      EQUIVALENCE (DUMMY,X(1))
      CHARACTER*20 CHEDFM,CDDNFM
      CHARACTER*256 FNAME, ARGV, PATH, STAYOPEN
      CHARACTER*6 MFTYPE
C-----EMRL COMMENT - ADD GMS VARIABLES AND MT3D LINKAGE
      INTEGER*4   NUMARGS
      common /emrl1/stayopen
      LOGICAL EXISTS
      CHARACTER*4 CUNIT(40), FLTYPE
      DATA CUNIT/'BCF ','WEL ','DRN ','RIV ','EVT ','TLK ','GHB ',
     1           'RCH ','SIP ','DE4 ','SOR ','OC  ','PCG ','GFD ',
     2           '    ','HFB ','RES ','STR ','IBS ','CHD ','FHB ',
     3           'MT3D','    ','    ','    ','    ','    ','    ',
     4           '    ','    ','    ','    ','    ','    ','    ',
     5           '    ','    ','    ','    ','    '/
C     ------------------------------------------------------------------
C     set string for use if RCS ident command
      FNAME =
     &'$Id: modflw96.f,v 3.2 1998/01/09 19:19:39 rsregan Exp rsregan $'
      FNAME = 
     &    '@(#)MODFLOW-96 - Modular 3-D Finite-Difference GW Flow Model'
      FNAME = '@(#)MODFLOW-96 - USGS TWRI, Book 6, Chap. A1, McDonald an
     &d Harbaugh'
      FNAME = '@(#)MODFLOW-96 - USGS OFR 96-485, Harbaugh and McDonald'
      FNAME = 
     &     '@(#)MODFLOW-96 - Contact: h2osoft@usgs.gov'
      FNAME = '@(#)MODFLOW-96 - Version: 3.0 1996/12/03 includes MOC'
      FNAME =
     &     '@(#)MODFLOW-96 - Version: 3.1 1997/03/11 fixed HFB1FM call'
      FNAME = '@(#)MODFLOW-96 - Version: 3.2x 1998/01/09 includes FHB'
C     ------------------------------------------------------------------

c      call GetCl(Command_Line)                              !jd
c      call GetStayOpen(Command_Line)                        !jd

      INUNIT=99
      IBUNIT=98
      IBOUTS=97
      IBATCH=0
	ISUP = 96
C      INQUIRE(FILE='modflow.bf',EXIST=EXISTS)
C      IF(EXISTS) THEN
C         IBATCH=1
C         OPEN(UNIT=IBUNIT,FILE='modflow.bf',STATUS='OLD')
C         OPEN(UNIT=IBOUTS,FILE='modbatch.rpt')
C         WRITE(IBOUTS,*) ' USGS MODFLOW MODEL BATCH-MODE REPORT'
C      END IF
C
C-----EMRL COMMENT - ADD COMMAND LINE ARGUEMENT ADDITIONS

      ARGV=' '
      STAYOPEN='0'
      NUMARGS = IARGC()
      IF (NUMARGS.EQ.2) THEN
        CALL GETARG(3,ARGV)
        IF (ARGV(1:1).EQ.' ') THEN
          CALL GETARG(1,ARGV)
          CALL GETARG(2,STAYOPEN)
        ELSE
          CALL GETARG(2,ARGV)
          CALL GETARG(3,STAYOPEN)
        ENDIF
      ELSE IF (NUMARGS.EQ.1) THEN
        CALL GETARG(2,ARGV)
        IF (ARGV(1:1).EQ.' ') CALL GETARG(1,ARGV)
      ENDIF

      CALL GetStayOpen(STAYOPEN)

      FNAME=ARGV
C#########################Patched by shengquan Yan 11/25/01########################
	FNAME = strSuperFile
C#########################Patch finished###########################################
      IF (FNAME(1:1) .EQ. ' ') THEN
600     WRITE(*,*) ' Enter the name of the GMS MODFLOW Superfile:'
        READ(*,'(A)') FNAME
      ENDIF

569   call SetBaseFileName(FName)                               !jd

      CALL GETPATH_MF(FNAME,PATH)
#ifdef _DEBUG
	!!DEC$ IF DEFINED(_DEBUG)
      WRITE(*,601) FNAME
	!!DEC$ ENDIF
#endif
      OPEN(UNIT=ISUP,FILE=FNAME,STATUS='OLD',ERR=600)
      READ(ISUP,601) MFTYPE
601   FORMAT(A)
603   FORMAT(A,1X,I4,1X,A)
      IF(MFTYPE(1:6).NE.'MODSUP') THEN
         WRITE(*,*) ' This file is not a GMS MODFLOW Superfile '
	      call STOPFILE_MF
         STOP
      ENDIF

      call ReadDryVar(ISup)                                     !jd
      NoConvStp=GetNoConvStp()                                  !jd
      call ReadLayVar(ISup,Path)                                !jd
      call ReadObsVar(ISup)                                     !jd

604   READ(ISUP,601,END=605) FLTYPE
      IF(FLTYPE(1:4).EQ.'NAME') THEN
         BACKSPACE(ISUP)
         READ(ISUP,603) FLTYPE, INUNIT, FNAME
         CALL SETPATH_MF(PATH,FNAME)
         OPEN(UNIT=INUNIT,FILE=FNAME,STATUS='OLD')
	ELSE
C-----EMRL COMMENT - ADD ADDITIONAL DATA TO BE READ HERE
      ENDIF
      GO TO 604
C
C-----EMRL COMMENT - END EMRL ADDITIONS
C
C2------OPEN FILE OF FILE NAMES.
C50    IF(IBATCH.GT.0) THEN
C         READ(IBUNIT,'(A)',END=500) FNAME
C         IF(FNAME.EQ.' ') GO TO 50
C         WRITE(IBOUTS,'(1X,/1X,A)') FNAME
C      ELSE
C         WRITE(*,*) ' Enter the name of the NAME FILE:'
C         READ(*,'(A)') FNAME
C      END IF
C      INQUIRE(FILE=FNAME,EXIST=EXISTS)
C      IF(.NOT.EXISTS) THEN
C         IF(IBATCH.GT.0) THEN
C            WRITE(IBOUTS,*) ' Specified name file does not exist.'
C            WRITE(IBOUTS,*) ' Processing will continue with the next ',
C     1                      'name file in modflow.bf.'
C         ELSE
C            WRITE(*,*) ' File does not exist'
C         END IF
C        GO TO 50
C      END IF
C      OPEN(UNIT=INUNIT,FILE=FNAME,STATUS='OLD')
C
C3------DEFINE PROBLEM--ROWS,COLUMNS,LAYERS,STRESS PERIODS,PACKAGES.
C-----EMRL COMMENT - ADD PATH TO BAS5DF SUBROUTINE
605   CALL BAS5DF(ISUM,HEADNG,NPER,ITMUNI,TOTIM,NCOL,NROW,NLAY,
     1        NODES,INBAS,IOUT,IUNIT,CUNIT,INUNIT,IXSEC,ICHFLG,
     1        IFREFM,PATH)
C
C4------ALLOCATE SPACE IN "X" ARRAY.
      CALL BAS5AL(ISUM,LENX,LCHNEW,LCHOLD,LCIBOU,LCCR,LCCC,LCCV,
     1              LCHCOF,LCRHS,LCDELR,LCDELC,LCSTRT,LCBUFF,LCIOFL,
     2              INBAS,ISTRT,NCOL,NROW,NLAY,IOUT,IAPART,IFREFM)
      IF(IUNIT(1).GT.0) CALL BCF5AL(ISUM,LENX,LCSC1,LCHY,
     1     LCBOT,LCTOP,LCSC2,LCTRPY,IUNIT(1),ISS,
     2     NCOL,NROW,NLAY,IOUT,IBCFCB,LCWETD,IWDFLG,LCCVWD,
     3     WETFCT,IWETIT,IHDWET,HDRY,IAPART,IFREFM)
      IF(IUNIT(2).GT.0) CALL WEL5AL(ISUM,LENX,LCWELL,MXWELL,NWELLS,
     1                 IUNIT(2),IOUT,IWELCB,NWELVL,IWELAL,IFREFM)
      IF(IUNIT(3).GT.0) CALL DRN5AL(ISUM,LENX,LCDRAI,NDRAIN,MXDRN,
     1                 IUNIT(3),IOUT,IDRNCB,NDRNVL,IDRNAL,IFREFM)
      IF(IUNIT(4).GT.0) CALL RIV5AL(ISUM,LENX,LCRIVR,MXRIVR,NRIVER,
     1            IUNIT(4),IOUT,IRIVCB,NRIVVL,IRIVAL,IFREFM)
      IF(IUNIT(5).GT.0) CALL EVT5AL(ISUM,LENX,LCIEVT,LCEVTR,LCEXDP,
     1            LCSURF,NCOL,NROW,NEVTOP,IUNIT(5),IOUT,IEVTCB,IFREFM)
      IF(IUNIT(6).GT.0) CALL TLK1AL(ISUM,LENX,NCOL,NROW,NLAY,
     1          LCRAT,LCZCB,LCA1,LCB1,LCALPH,LCBET,LCRM1,LCRM2,LCRM3,
     2          LCRM4,LCTL,LCTLK,LCSLU,LCSLD,NODES1,NM1,NM2,NUMC,
     3          NTM1,ITLKSV,ITLKRS,ITLKCB,ISS,IUNIT(6),IOUT,ifrefm) ! emrl ff
      IF(IUNIT(7).GT.0) CALL GHB5AL(ISUM,LENX,LCBNDS,NBOUND,MXBND,
     1            IUNIT(7),IOUT,IGHBCB,NGHBVL,IGHBAL,IFREFM)
      IF(IUNIT(8).GT.0) CALL RCH5AL(ISUM,LENX,LCIRCH,LCRECH,NRCHOP,
     1            NCOL,NROW,IUNIT(8),IOUT,IRCHCB,IFREFM)
      IF(IUNIT(9).GT.0) CALL SIP5AL(ISUM,LENX,LCEL,LCFL,LCGL,LCV,
     1          LCHDCG,LCLRCH,LCW,MXITER,NPARM,NCOL,NROW,NLAY,
     2          IUNIT(9),IOUT,IFREFM)
      IF(IUNIT(10).GT.0) CALL DE45AL(ISUM,LENX,LCAU,LCAL,LCIUPP,
     1           LCIEQP,LCD4B,LCLRCH,LCHDCG,
     2           MXUP,MXLOW,MXEQ,MXBW,IUNIT(10),ITMX,ID4DIR,
     3           NCOL,NROW,NLAY,IOUT,ID4DIM)
      IF(IUNIT(11).GT.0) CALL SOR5AL(ISUM,LENX,LCA,LCRES,LCHDCG,LCLRCH,
     1       LCIEQP,MXITER,NCOL,NLAY,NSLICE,MBW,IUNIT(11),IOUT,IFREFM)
      IF(IUNIT(13).GT.0) CALL PCG2AL(ISUM,LENX,LCV,LCSS,LCP,LCCD,
     1       LCHCHG,LCLHCH,LCRCHG,LCLRCH,MXITER,ITER1,NCOL,NROW,NLAY,
     2       IUNIT(13),IOUT,NPCOND,LCIT1)
      IF(IUNIT(14).GT.0) CALL GFD1AL(ISUM,LENX,LCSC1,LCCDTR,LCCDTC,
     1     LCBOT,LCTOP,LCSC2,IUNIT(14),ISS,NCOL,NROW,NLAY,IOUT,IGFDCB,
     2     ifrefm)  !emrl ff
      IF(IUNIT(16).GT.0) CALL HFB1AL(ISUM,LENX,LCHFBR,NHFB,IUNIT(16),      *HFB*
     1           IOUT)                                                     *HFB*
      IF(IUNIT(17).GT.0) CALL RES1AL(ISUM,LENX,LCIRES,LCIRSL,LCBRES,
     1  LCCRES,LCBBRE,LCHRES,LCHRSE,IUNIT(17),IOUT,NRES,IRESCB,
     2  NRESOP,IRESPT,NPTS,NCOL,NROW,ifrefm)  !emrl ff
      IF(IUNIT(18).GT.0) CALL STR1AL(ISUM,LENX,LCSTRM,ICSTRM,MXSTRM,    STR1
     1                 NSTREM,IUNIT(18),IOUT,ISTCB1,ISTCB2,NSS,NTRIB,   STR1
     2                  NDIV,ICALC,CONST,LCTBAR,LCTRIB,LCIVAR,LCFGAR,   STR1
     3                  ifrefm)  !emrl ff                               STR1
      IF (IUNIT(19).GT.0) CALL IBS1AL(ISUM,LENX,LCHC,LCSCE,LCSCV,       IBS
     1           LCSUB,NCOL,NROW,NLAY,IIBSCB,IIBSOC,ISS,IUNIT(19),IOUT, IBS
     2           ifrefm) !emrl ff                                       IBS
      IF(IUNIT(20).GT.0) CALL CHD1AL(ISUM,LENX,LCCHDS,NCHDS,MXCHD,      CHD
     1           IUNIT(20),IOUT)                                        CHD
      IF(IUNIT(21).GT.0) CALL FHB1AL(ISUM,LENX,LCFLLC,LCBDTM,LCFLRT,
     1          LCBDFV,LCBDHV,LCHDLC,LCSBHD,NBDTIM,NFLW,NHED,IUNIT(21),
     2          IOUT,IFHBCB,NFHBX1,NFHBX2,IFHBD3,IFHBD4,IFHBD5,
     3          IFHBSS,ISS)
C
C5------IF THE "X" ARRAY IS NOT BIG ENOUGH THEN STOP.
      IF(ISUM-1.GT.LENX)       call STOPFILE_MF
C

      call RunType(Iss,NCol,Nrow,NLay)                                  !jd
      call ReadPmaFile()                                                !jd
      call InitObserv()                                                 !jd
      call GetObsDim()                                                  !jd
      call ReadPointFile()                                              !jd
      call ReadFluxFile()                                               !jd
      call ReadMeasFile()                                               !jd
      call SetLayDim(NCol,NRow,NLay)                                    !jd
      call ReadLayDat(Iout)                                             !jd
c      call debug1()                                                     !jd

C6------READ AND PREPARE INFORMATION FOR ENTIRE SIMULATION.
      CALL BAS5RP(X(LCIBOU),X(LCHNEW),X(LCSTRT),X(LCHOLD),
     1       ISTRT,INBAS,HEADNG,NCOL,NROW,NLAY,VBVL,X(LCIOFL),
     2       IUNIT(12),IHEDFM,IDDNFM,IHEDUN,IDDNUN,IOUT,IPEROC,ITSOC,
     3       CHEDFM,CDDNFM,IBDOPT,IXSEC,LBHDSV,LBDDSV,IFREFM)

      call SetStartHeads_Int(NCol,NRow,NLay,x(lchold))                  !jd

      IF(IUNIT(1).GT.0) CALL BCF5RP(X(LCIBOU),X(LCHNEW),X(LCSC1),
     1          X(LCHY),X(LCCR),X(LCCC),X(LCCV),X(LCDELR),
     2     X(LCDELC),X(LCBOT),X(LCTOP),X(LCSC2),X(LCTRPY),IUNIT(1),
     3     ISS,NCOL,NROW,NLAY,IOUT,X(LCWETD),IWDFLG,X(LCCVWD))
      IF(IUNIT(6).GT.0) CALL TLK1RP(X(LCRAT),X(LCZCB),X(LCA1),X(LCB1),
     1          X(LCALPH),X(LCBET),X(LCRM1),X(LCRM2),X(LCRM3),X(LCRM4),
     2          NODES1,NM1,NM2,NUMC,NTM1,ITLKRS,DELTM1,X(LCBUFF),
     3          X(LCDELC),X(LCDELR),TLKTIM,NROW,NCOL,IUNIT(6),IOUT)
      IF(IUNIT(9).GT.0) CALL SIP5RP(NPARM,MXITER,ACCL,HCLOSE,X(LCW),
     1          IUNIT(9),IPCALC,IPRSIP,IOUT,IFREFM)
      IF(IUNIT(10).GT.0) CALL DE45RP(IUNIT(10),MXITER,NITER,ITMX,
     1            ACCL,HCLOSE,IFREQ,IPRD4,IOUT,MUTD4)
      IF(IUNIT(11).GT.0) CALL SOR5RP(MXITER,ACCL,HCLOSE,IUNIT(11),
     1         IPRSOR,IOUT,IFREFM)
      IF(IUNIT(13).GT.0) CALL PCG2RP(MXITER,ITER1,HCLOSE,RCLOSE,
     1         NPCOND,NBPOL,RELAX,IPRPCG,IUNIT(13),IOUT,MUTPCG,
     2         NITER,X(LCIT1),DAMP)
      IF(IUNIT(14).GT.0) CALL GFD1RP(X(LCIBOU),X(LCHNEW),X(LCSC1),
     1          X(LCCDTR),X(LCCDTC),X(LCCR),X(LCCC),X(LCCV),X(LCDELR),
     2          X(LCDELC),X(LCBOT),X(LCTOP),X(LCSC2),
     3          IUNIT(14),ISS,NCOL,NROW,NLAY,NODES,IOUT)
      IF(IUNIT(16).GT.0) CALL HFB1RP(X(LCCR),X(LCCC),X(LCDELR),            *HFB*
     1         X(LCDELC),X(LCHFBR),IUNIT(16),NCOL,NROW,NLAY,NODES,         *HFB*
     1         NHFB,IOUT,ifrefm)  !emrl ff                                 *HFB*
      IF(IUNIT(19).GT.0) CALL IBS1RP(X(LCDELR),X(LCDELC),X(LCHNEW),     IBS
     1      X(LCHC),X(LCSCE),X(LCSCV),X(LCSUB),NCOL,NROW,NLAY,          IBS
     2      NODES,IIBSOC,ISUBFM,ICOMFM,IHCFM,ISUBUN,ICOMUN,IHCUN,       IBS
     3      IUNIT(19),IOUT,ifrefm)  !emrl ff                            IBS
      IF(IUNIT(21).GT.0) CALL FHB1RP(X(LCIBOU),NROW,NCOL,NLAY,
     &          X(LCFLLC),X(LCBDTM),NBDTIM,X(LCFLRT),NFLW,NHED,
     &          X(LCHDLC),X(LCSBHD),IUNIT(21),IOUT,
     &          NFHBX1,NFHBX2,IFHBD3,IFHBD5)
C
C7------SIMULATE EACH STRESS PERIOD.
      DO 300 KPER=1,NPER
      KKPER=KPER
C
C7A-----READ STRESS PERIOD TIMING INFORMATION.
      CALL BAS5ST(NSTP,DELT,TSMULT,PERTIM,KKPER,INBAS,IOUT,IFREFM)
C
C7B-----READ AND PREPARE INFORMATION FOR STRESS PERIOD.
      IF(IUNIT(2).GT.0) CALL WEL5RP(X(LCWELL),NWELLS,MXWELL,IUNIT(2),
     1             IOUT,NWELVL,IWELAL,IFREFM)
      IF(IUNIT(3).GT.0) CALL DRN5RP(X(LCDRAI),NDRAIN,MXDRN,IUNIT(3),
     1                 IOUT,NDRNVL,IDRNAL,IFREFM)
      IF(IUNIT(4).GT.0) CALL RIV5RP(X(LCRIVR),NRIVER,MXRIVR,IUNIT(4),
     1            IOUT,NRIVVL,IRIVAL,IFREFM)
      IF(IUNIT(5).GT.0) CALL EVT5RP(NEVTOP,X(LCIEVT),X(LCEVTR),
     1            X(LCEXDP),X(LCSURF),X(LCDELR),X(LCDELC),NCOL,NROW,
     1            IUNIT(5),IOUT,IFREFM)
      IF(IUNIT(7).GT.0) CALL GHB5RP(X(LCBNDS),NBOUND,MXBND,IUNIT(7),
     1              IOUT,NGHBVL,IGHBAL,IFREFM)
      IF(IUNIT(8).GT.0) CALL RCH5RP(NRCHOP,X(LCIRCH),X(LCRECH),
     1            X(LCDELR),X(LCDELC),NROW,NCOL,IUNIT(8),IOUT,IFREFM)
      IF(IUNIT(17).GT.0) CALL RES1RP(X(LCIRES),X(LCIRSL),X(LCBRES),
     1   X(LCCRES),X(LCBBRE),X(LCHRSE),X(LCIBOU),X(LCDELR),X(LCDELC),
     2   NRES,NRESOP,NPTS,NCOL,NROW,NLAY,PERLEN,DELT,NSTP,TSMULT,
     3   IUNIT(17),IOUT)
      IF(IUNIT(18).GT.0) CALL STR1RP(X(LCSTRM),X(ICSTRM),NSTREM,        STR1
     1                     MXSTRM,IUNIT(18),IOUT,X(LCTBAR),NDIV,NSS,    STR1
     2                     NTRIB,X(LCIVAR),ICALC,IPTFLG,ifrefm)                STR1
      IF(IUNIT(20).GT.0) CALL CHD1RP(X(LCCHDS),NCHDS,MXCHD,X(LCIBOU),   CHD
     1            NCOL,NROW,NLAY,PERLEN,DELT,NSTP,TSMULT,IUNIT(20),IOUT, 
     2            ifrefm)  !emrl                                        CHD
C
C7C-----SIMULATE EACH TIME STEP.
      DO 200 KSTP=1,NSTP
      KKSTP=KSTP
C
C7C1----CALCULATE TIME STEP LENGTH. SET HOLD=HNEW..
      CALL BAS5AD(DELT,TSMULT,TOTIM,PERTIM,X(LCHNEW),X(LCHOLD),KKSTP,
     1             NCOL,NROW,NLAY)

      call Mod2DaySec(Totim,Delt)                                      !jd
      call SetOldHeads_Int(NCol,NRow,NLay,x(lchnew))                   !jd
c      call debug2                                                      !jd

      IF(IUNIT(6).GT.0) CALL TLK1AD(X(LCRAT),X(LCZCB),X(LCA1),X(LCB1),
     1          X(LCALPH),X(LCBET),X(LCRM1),X(LCRM2),X(LCRM3),X(LCRM4),
     2          X(LCTL),X(LCTLK),X(LCSLU),X(LCSLD),NM1,NM2,NUMC,NTM1,
     3          DELTM1,X(LCHNEW),X(LCIBOU),X(LCTOP),
     4          NROW,NCOL,NLAY,DELT,TLKTIM,IUNIT(6),IOUT)
      IF(IUNIT(20).GT.0) CALL CHD1FM(NCHDS,MXCHD,X(LCCHDS),X(LCIBOU),   CHD
     1          X(LCHNEW),X(LCHOLD),PERLEN,PERTIM,DELT,NCOL,NROW,NLAY)  CHD
      IF(IUNIT(1).GT.0) CALL BCF5AD(X(LCIBOU),X(LCHOLD),X(LCBOT),
     1             X(LCWETD),IWDFLG,ISS,NCOL,NROW,NLAY)
      IF(IUNIT(17).GT.0) CALL RES1AD(X(LCHRES),X(LCHRSE),X(LCIRES),
     1 X(LCBRES),X(LCDELR),X(LCDELC),NRES,IRESPT,NCOL,NROW,
     1      PERLEN,PERTIM,TOTIM,KKSTP,KKPER,IOUT)
      IF(IUNIT(21).GT.0) CALL FHB1AD(X(LCHNEW),X(LCHOLD),NCOL,NROW,NLAY,
     &          ISS,TOTIM,DELT,X(LCBDTM),NBDTIM,X(LCFLRT),
     &          X(LCBDFV),X(LCBDHV),NFLW,X(LCSBHD),X(LCHDLC),NHED,
     &          NFHBX1,NFHBX2,IFHBD3,IFHBD4,IFHBD5,IFHBSS)
C
C7C2----ITERATIVELY FORMULATE AND SOLVE THE EQUATIONS.
      DO 100 KITER=1,MXITER
      KKITER=KITER
C------EMRL RJD Echo out the iteration and time step 7/26/94
#ifdef _DEBUG
	!!DEC$ IF DEFINED(_DEBUG)
      WRITE(*,1800) KITER,KSTP,KPER
	!!DEC$ ENDIF
#endif
 1800 FORMAT(1X,'Starting iteration',I4,' of time step',I3,' in stress p
     1eriod',I3)
C
C7C2A---FORMULATE THE FINITE DIFFERENCE EQUATIONS.
      CALL BAS5FM(X(LCHCOF),X(LCRHS),NODES)
      IF(IUNIT(1).GT.0) CALL BCF5FM(X(LCHCOF),X(LCRHS),X(LCHOLD),
     1          X(LCSC1),X(LCHNEW),X(LCIBOU),X(LCCR),X(LCCC),X(LCCV),
     2          X(LCHY),X(LCTRPY),X(LCBOT),X(LCTOP),X(LCSC2),
     3          X(LCDELR),X(LCDELC),DELT,ISS,KKITER,KKSTP,KKPER,NCOL,
     4          NROW,NLAY,IOUT,X(LCWETD),IWDFLG,X(LCCVWD),WETFCT,
     5          IWETIT,IHDWET,HDRY,X(LCBUFF))
      IF(IUNIT(14).GT.0) CALL GFD1FM(X(LCHCOF),X(LCRHS),X(LCHOLD),
     1          X(LCSC1),X(LCHNEW),X(LCIBOU),X(LCCR),X(LCCC),X(LCCV),
     2          X(LCCDTR),X(LCCDTC),X(LCBOT),X(LCTOP),X(LCSC2),
     3          DELT,ISS,KKITER,KKSTP,KKPER,NCOL,NROW,NLAY,IOUT)
      IF(IUNIT(16).GT.0) CALL HFB1FM(X(LCHNEW),X(LCCR),X(LCCC),            *HFB*
     1          X(LCBOT),X(LCTOP),X(LCDELR),X(LCDELC),X(LCHFBR),           *HFB*
     2          NCOL,NROW,NLAY,NHFB)                                       *HFB*
      IF(IUNIT(6).GT.0) CALL TLK1FM(X(LCRAT),X(LCTL),X(LCTLK),X(LCSLU),
     1          X(LCSLD),NUMC,X(LCHNEW),X(LCIBOU),X(LCTOP),X(LCCV),
     2          X(LCHCOF),X(LCRHS),NROW,NCOL,NLAY)
      IF(IUNIT(2).GT.0) CALL WEL5FM(NWELLS,MXWELL,X(LCRHS),X(LCWELL),
     1           X(LCIBOU),NCOL,NROW,NLAY,NWELVL)
      IF(IUNIT(3).GT.0) CALL DRN5FM(NDRAIN,MXDRN,X(LCDRAI),X(LCHNEW),
     1         X(LCHCOF),X(LCRHS),X(LCIBOU),NCOL,NROW,NLAY,NDRNVL)
      IF(IUNIT(4).GT.0) CALL RIV5FM(NRIVER,MXRIVR,X(LCRIVR),X(LCHNEW),
     1            X(LCHCOF),X(LCRHS),X(LCIBOU),NCOL,NROW,NLAY,NRIVVL)
      IF(IUNIT(5).GT.0) CALL EVT5FM(NEVTOP,X(LCIEVT),X(LCEVTR),
     1            X(LCEXDP),X(LCSURF),X(LCRHS),X(LCHCOF),X(LCIBOU),
     1            X(LCHNEW),NCOL,NROW,NLAY)
      IF(IUNIT(7).GT.0) CALL GHB5FM(NBOUND,MXBND,X(LCBNDS),X(LCHCOF),
     1            X(LCRHS),X(LCIBOU),NCOL,NROW,NLAY,NGHBVL)
      IF(IUNIT(8).GT.0) CALL RCH5FM(NRCHOP,X(LCIRCH),X(LCRECH),
     1            X(LCRHS),X(LCIBOU),NCOL,NROW,NLAY)
      IF(IUNIT(17).GT.0) CALL RES1FM(X(LCIRES),X(LCIRSL),X(LCBRES),
     1   X(LCCRES),X(LCBBRE),X(LCHRES),X(LCIBOU),X(LCHNEW),X(LCHCOF),
     2   X(LCRHS),NRES,NRESOP,NCOL,NROW,NLAY)
      IF(IUNIT(18).GT.0) CALL STR1FM(NSTREM,X(LCSTRM),X(ICSTRM),        STR1
     1                     X(LCHNEW),X(LCHCOF),X(LCRHS),X(LCIBOU),      STR1
     2              MXSTRM,NCOL,NROW,NLAY,IOUT,NSS,X(LCTBAR),           STR1
     3              NTRIB,X(LCTRIB),X(LCIVAR),X(LCFGAR),ICALC,CONST)    STR1
      IF(IUNIT(19).GT.0) CALL IBS1FM(X(LCRHS),X(LCHCOF),X(LCHNEW),      IBS
     1       X(LCHOLD),X(LCHC),X(LCSCE),X(LCSCV),X(LCIBOU),             IBS
     2       NCOL,NROW,NLAY,DELT)                                       IBS
      IF(IUNIT(21).GT.0) CALL FHB1FM(X(LCRHS),X(LCIBOU),X(LCFLLC),
     1 X(LCBDFV),NFLW,NCOL,NROW,NLAY,IFHBD4)
C
C7C2B---MAKE ONE CUT AT AN APPROXIMATE SOLUTION.
      IF(IUNIT(9).GT.0) CALL SIP5AP(X(LCHNEW),X(LCIBOU),X(LCCR),X(LCCC),
     1     X(LCCV),X(LCHCOF),X(LCRHS),X(LCEL),X(LCFL),X(LCGL),X(LCV),
     2     X(LCW),X(LCHDCG),X(LCLRCH),NPARM,KKITER,HCLOSE,ACCL,ICNVG,
     3     KKSTP,KKPER,IPCALC,IPRSIP,MXITER,NSTP,NCOL,NROW,NLAY,NODES,
     4     IOUT)
      IF(IUNIT(10).GT.0) CALL DE45AP(X(LCHNEW),X(LCIBOU),X(LCAU),
     1  X(LCAL),X(LCIUPP),X(LCIEQP),X(LCD4B),MXUP,MXLOW,MXEQ,MXBW,
     2  X(LCCR),X(LCCC),X(LCCV),X(LCHCOF),X(LCRHS),ACCL,KKITER,ITMX,
     3  MXITER,NITER,HCLOSE,IPRD4,ICNVG,NCOL,NROW,NLAY,IOUT,X(LCLRCH),
     4  X(LCHDCG),IFREQ,KKSTP,KKPER,DELT,NSTP,ID4DIR,ID4DIM,MUTD4)
      IF(IUNIT(11).GT.0) CALL SOR5AP(X(LCHNEW),X(LCIBOU),X(LCCR),
     1     X(LCCC),X(LCCV),X(LCHCOF),X(LCRHS),X(LCA),X(LCRES),X(LCIEQP),
     2     X(LCHDCG),X(LCLRCH),KKITER,HCLOSE,ACCL,ICNVG,KKSTP,KKPER,
     3     IPRSOR,MXITER,NSTP,NCOL,NROW,NLAY,NSLICE,MBW,IOUT)
      IF(IUNIT(13).GT.0) CALL PCG2AP(X(LCHNEW),X(LCIBOU),X(LCCR),
     1      X(LCCC),X(LCCV),X(LCHCOF),X(LCRHS),X(LCV),X(LCSS),X(LCP),
     2      X(LCCD),X(LCHCHG),X(LCLHCH),X(LCRCHG),X(LCLRCH),KKITER,
     3      NITER,HCLOSE,RCLOSE,ICNVG,KKSTP,KKPER,IPRPCG,MXITER,ITER1,
     4      NPCOND,NBPOL,NSTP,NCOL,NROW,NLAY,NODES,RELAX,IOUT,MUTPCG,
     5      0,0,SN,SP,SR,X(LCIT1),DAMP)
C
C7C2C---IF CONVERGENCE CRITERION HAS BEEN MET STOP ITERATING.
      IF(ICNVG.EQ.1) GO TO 110
  100 CONTINUE
      KITER=MXITER
  110 CONTINUE

      call HeadObs_Int(NCol,NRow,NLay,x(lchnew))                   !jd

C
C7C3----DETERMINE WHICH OUTPUT IS NEEDED.
      CALL BAS5OC(NSTP,KKSTP,ICNVG,X(LCIOFL),NLAY,IBUDFL,ICBCFL,
     1   IHDDFL,IUNIT(12),IOUT,KKPER,IPEROC,ITSOC,IBDOPT,IXSEC,IFREFM)
C
C7C4----CALCULATE BUDGET TERMS. SAVE CELL-BY-CELL FLOW TERMS.
      MSUM=1
      IF(IUNIT(6).GT.0) CALL TLK1BD(X(LCRAT),X(LCTL),X(LCTLK),
     1          X(LCSLU),X(LCSLD),NUMC,ITLKCB,X(LCHNEW),X(LCBUFF),
     2          X(LCIBOU),X(LCTOP),X(LCCV),VBNM,VBVL,MSUM,NCOL,NROW,
     3          NLAY,DELT,KSTP,KPER,ICBCFL,IOUT)
C7C4A---THE ORIGINAL BCF BUDGET MODULE HAS BEEN REPLACED BY THREE
C7C4A---SUBMODULES: SBCF5S, SBCF5F, AND SBCF5B .
      IF(IUNIT(1).GT.0) THEN
         CALL SBCF5S(VBNM,VBVL,MSUM,X(LCHNEW),X(LCIBOU),X(LCHOLD),
     1     X(LCSC1),X(LCTOP),X(LCSC2),DELT,ISS,NCOL,NROW,NLAY,KKSTP,
     2     KKPER,IBCFCB,ICBCFL,X(LCBUFF),IOUT,PERTIM,TOTIM)
         CALL SBCF5F(VBNM,VBVL,MSUM,X(LCHNEW),X(LCIBOU),X(LCCR),
     1     X(LCCC),X(LCCV),X(LCTOP),DELT,NCOL,NROW,NLAY,KKSTP,KKPER,
     2     IBCFCB,X(LCBUFF),IOUT,ICBCFL,PERTIM,TOTIM,ICHFLG)
         IBDRET=0
         IC1=1
         IC2=NCOL
         IR1=1
         IR2=NROW
         IL1=1
         IL2=NLAY
         DO 155 IDIR=1,3
         CALL SBCF5B(X(LCHNEW),X(LCIBOU),X(LCCR),X(LCCC),X(LCCV),
     1      X(LCTOP),NCOL,NROW,NLAY,KKSTP,KKPER,IBCFCB,X(LCBUFF),
     2      IOUT,ICBCFL,DELT,PERTIM,TOTIM,IDIR,IBDRET,ICHFLG,
     3      IC1,IC2,IR1,IR2,IL1,IL2)
155      CONTINUE
      END IF
      IF(IUNIT(14).GT.0) CALL GFD1BD(VBNM,VBVL,MSUM,X(LCHNEW),
     1     X(LCIBOU),X(LCHOLD),X(LCSC1),X(LCCR),X(LCCC),X(LCCV),
     2     X(LCTOP),X(LCSC2),DELT,ISS,NCOL,NROW,NLAY,KKSTP,KKPER,
     3     IGFDCB,ICBCFL,X(LCBUFF),IOUT)
      IF(IUNIT(2).GT.0) CALL WEL5BD(NWELLS,MXWELL,VBNM,VBVL,MSUM,
     1     X(LCWELL),X(LCIBOU),DELT,NCOL,NROW,NLAY,KKSTP,KKPER,IWELCB,
     1     ICBCFL,X(LCBUFF),IOUT,PERTIM,TOTIM,NWELVL,IWELAL)
      IF(IUNIT(3).GT.0) CALL DRN5BD(NDRAIN,MXDRN,VBNM,VBVL,MSUM,
     1     X(LCDRAI),DELT,X(LCHNEW),NCOL,NROW,NLAY,X(LCIBOU),KKSTP,
     2     KKPER,IDRNCB,ICBCFL,X(LCBUFF),IOUT,PERTIM,TOTIM,NDRNVL,
     3     IDRNAL)
      IF(IUNIT(4).GT.0) CALL RIV5BD(NRIVER,MXRIVR,X(LCRIVR),X(LCIBOU),
     1     X(LCHNEW),NCOL,NROW,NLAY,DELT,VBVL,VBNM,MSUM,KKSTP,KKPER,
     2     IRIVCB,ICBCFL,X(LCBUFF),IOUT,PERTIM,TOTIM,NRIVVL,IRIVAL)
      IF(IUNIT(5).GT.0) CALL EVT5BD(NEVTOP,X(LCIEVT),X(LCEVTR),
     1     X(LCEXDP),X(LCSURF),X(LCIBOU),X(LCHNEW),NCOL,NROW,NLAY,
     2     DELT,VBVL,VBNM,MSUM,KKSTP,KKPER,IEVTCB,ICBCFL,X(LCBUFF),IOUT,
     3     PERTIM,TOTIM)
      IF(IUNIT(7).GT.0) CALL GHB5BD(NBOUND,MXBND,VBNM,VBVL,MSUM,
     1     X(LCBNDS),DELT,X(LCHNEW),NCOL,NROW,NLAY,X(LCIBOU),KKSTP,
     2     KKPER,IGHBCB,ICBCFL,X(LCBUFF),IOUT,PERTIM,TOTIM,NGHBVL,
     3     IGHBAL)
      IF(IUNIT(8).GT.0) CALL RCH5BD(NRCHOP,X(LCIRCH),X(LCRECH),
     1     X(LCIBOU),NROW,NCOL,NLAY,DELT,VBVL,VBNM,MSUM,KKSTP,KKPER,
     2     IRCHCB,ICBCFL,X(LCBUFF),IOUT,PERTIM,TOTIM)
      IF(IUNIT(17).GT.0) CALL RES1BD(X(LCIRES),X(LCIRSL),X(LCBRES),
     1      X(LCCRES),X(LCBBRE),X(LCHRES),X(LCIBOU),X(LCHNEW),
     2      X(LCBUFF),VBVL,VBNM,MSUM,KSTP,KPER,NRES,NRESOP,
     3      NCOL,NROW,NLAY,DELT,IRESCB,ICBCFL,IOUT)
      IF(IUNIT(18).GT.0) CALL STR1BD(NSTREM,X(LCSTRM),X(ICSTRM),        STR1
     1   X(LCIBOU),MXSTRM,X(LCHNEW),NCOL,NROW,NLAY,DELT,VBVL,VBNM,MSUM, STR1
     2   KKSTP,KKPER,ISTCB1,ISTCB2,ICBCFL,X(LCBUFF),IOUT,NTRIB,NSS,     STR1
     3   X(LCTRIB),X(LCTBAR),X(LCIVAR),X(LCFGAR),ICALC,CONST,IPTFLG)    STR1
      IF(IUNIT(19).GT.0) CALL IBS1BD(X(LCIBOU),X(LCHNEW),X(LCHOLD),     IBS
     1      X(LCHC),X(LCSCE),X(LCSCV),X(LCSUB),X(LCDELR),X(LCDELC),     IBS
     2      NCOL,NROW,NLAY,DELT,VBVL,VBNM,MSUM,KSTP,KPER,IIBSCB,        IBS
     3      ICBCFL,X(LCBUFF),IOUT)                                      IBS
      IF(IUNIT(21).GT.0) CALL FHB1BD(X(LCFLLC),X(LCBDFV),NFLW,
     1     VBNM,VBVL,MSUM,X(LCIBOU),DELT,NCOL,NROW,NLAY,KKSTP,KKPER,
     2     IFHBCB,ICBCFL,X(LCBUFF),IOUT,IFHBD4)
C
C-----EMRL COMMENT - SAVE CELL-BY-CELL FLOW TERMS FOR USE IN MT3DMS
      INCLUDE 'lkmt3.inc'
C
C7C5---PRINT AND OR SAVE HEADS AND DRAWDOWNS. PRINT OVERALL BUDGET.
      CALL BAS5OT(X(LCHNEW),X(LCSTRT),ISTRT,X(LCBUFF),X(LCIOFL),
     1     MSUM,X(LCIBOU),VBNM,VBVL,KKSTP,KKPER,DELT,PERTIM,TOTIM,
     2     ITMUNI,NCOL,NROW,NLAY,ICNVG,IHDDFL,IBUDFL,IHEDFM,IHEDUN,
     3     IDDNFM,IDDNUN,IOUT,CHEDFM,CDDNFM,IXSEC,LBHDSV,LBDDSV)
C
C7C5A--PRINT AND OR SAVE SUBSIDENCE, COMPACTION, AND CRITICAL HEAD.
      IF(IUNIT(19).GT.0) CALL IBS1OT(NCOL,NROW,NLAY,PERTIM,TOTIM,KSTP,  IBS
     1      KPER,NSTP,X(LCBUFF),X(LCSUB),X(LCHC),IIBSOC,ISUBFM,ICOMFM,  IBS
     2      IHCFM,ISUBUN,ICOMUN,IHCUN,IUNIT(19),IOUT)                   IBS
C
C7C6----IF ITERATION FAILED TO CONVERGE THEN STOP.
      if(noconvstp.eq.0)then                             !jd
      IF(ICNVG.EQ.0) THEN
        WRITE(*,*) 'ITERATION FAILED TO CONVERGE. STOPPING'
        call STOPFILE_MF
      ENDIF
      end if                                             !jd
  200 CONTINUE
  300 CONTINUE

      call ModObsWrit()                                  !jd

C
C7C7----WRITE RESTART RECORDS
C7C7A---WRITE RESTART RECORDS FOR TRANSIENT-LEAKAGE PACKAGE
      IF(IUNIT(6).GT.0) CALL TLK1OT(X(LCRM1),X(LCRM2),
     1     X(LCRM3),X(LCRM4),NM1,NM2,ITLKSV,DELTM1,TLKTIM,IOUT)
C
C8------END OF SIMULATION
C         WRITE(IBOUTS,*) ' Normal termination of simulation.'
C         DO 400 I=1,IBOUTS-1
C            INQUIRE(UNIT=I,OPENED=EXISTS)
C            IF(EXISTS) CLOSE(I)
C  400    CONTINUE
C         GO TO 50
C      END IF
C------EMRL JIG
#ifdef _DEBUG
	!!DEC$ IF DEFINED(_DEBUG)
      WRITE(*,*) ' MODFLOW TERMINATED SUCCESSFULLY'
	!!DEC$ ENDIF
#endif
  500       call STOPFILE_MF
C
C################################Pathed by Shengquan Yan############################
C      END
	call CloseFiles
	contains
		subroutine CloseFiles
			integer :: i
			do i=LBOUND(IUNIT, 1), UBOUND(IUNIT, 1)
				if(IUNIT(i).GT.0) close(IUNIT(i))
			end do

			close( INUNIT )
			close( IBUNIT )
			close( IBOUTS )
			close( IBATCH )
			close( ISUP )

			close( INBAS )
			close( IOUT )
		end subroutine

	end subroutine ModFlowEntry
C################################Path finished#######################################

C-----EMRL COMMENT - FUNCTIONS FOR OPENING FILES
      SUBROUTINE GETPATH_MF(NAME,PATH)
      CHARACTER*256 NAME,PATH
      INTEGER*4 COUNT

      COUNT = 256
      PATH = NAME
200   IF ((COUNT .GT. 0) .AND. (PATH(COUNT:COUNT) .NE. '\')) THEN
        PATH(COUNT:COUNT) = ' '
        COUNT = COUNT - 1
        GO TO 200
      ENDIF
      END

      SUBROUTINE SETPATH_MF(PATH,FNAME)
      CHARACTER*256 PATH,FNAME,NEWNAME
      INTEGER*4 COUNT,I1,I2
C
      I1 = 1
888   IF (FNAME(I1:I1) .EQ. '\') THEN
        COUNT = 0
        GO TO 996
      ELSE
        I1 = I1 + 1
      ENDIF
      IF (I1 .LT. 256) THEN
        GOTO 888
      ENDIF
      COUNT = 256
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
998   IF (COUNT .LT. 256 .AND. I1 .LT. 256) THEN
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

      subroutine STOPFILE_MF
      use Observation                                 !jd
      use How_to_Stop                                 !jd

      call ObsFin()                                   !jd
      call KillApp()                                  !jd
C#################Pathed by Shengquan Yan, 11/25/01##########################
C      stop
C#################Path finished##############################################

      end
C-----EMRL COMMENT - END OF EMRL ADDITIONS
