# Microsoft Developer Studio Project File - Name="umatilla" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Console Application" 0x0103

CFG=umatilla - Win32 Debug
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "umatilla.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "umatilla.mak" CFG="umatilla - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "umatilla - Win32 Release" (based on "Win32 (x86) Console Application")
!MESSAGE "umatilla - Win32 Debug" (based on "Win32 (x86) Console Application")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
F90=df.exe
RSC=rc.exe

!IF  "$(CFG)" == "umatilla - Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "Release"
# PROP BASE Intermediate_Dir "Release"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "Release"
# PROP Intermediate_Dir "Release"
# PROP Target_Dir ""
# ADD BASE F90 /compile_only /nologo /warn:nofileopt
# ADD F90 /compile_only /nologo /warn:nofileopt
# ADD BASE CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_CONSOLE" /D "_MBCS" /YX /FD /c
# ADD CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_CONSOLE" /D "_MBCS" /YX /FD /c
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /machine:I386
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /machine:I386

!ELSEIF  "$(CFG)" == "umatilla - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "Debug"
# PROP BASE Intermediate_Dir "Debug"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "Debug"
# PROP Intermediate_Dir "Debug"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE F90 /check:bounds /compile_only /dbglibs /debug:full /nologo /traceback /warn:argument_checking /warn:nofileopt
# ADD F90 /browser /check:bounds /compile_only /dbglibs /debug:full /nologo /traceback /warn:argument_checking /warn:nofileopt
# ADD BASE CPP /nologo /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_CONSOLE" /D "_MBCS" /YX /FD /GZ /c
# ADD CPP /nologo /MTd /W3 /Gm /GR /GX /ZI /Od /I "d:/Project Lib/condor/helper" /I "d:/Project Lib/condor/ncp" /D "WIN32" /D "_DEBUG" /D "_CONSOLE" /D "_MBCS" /YX /FD /I /Project /I /Project /GZ /c
# SUBTRACT CPP /Fr
# ADD BASE RSC /l 0x409 /d "_DEBUG"
# ADD RSC /l 0x409 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /debug /machine:I386 /pdbtype:sept
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib Ws2_32.lib /nologo /subsystem:console /incremental:no /debug /machine:I386 /pdbtype:sept /DEFAULTLIB:LIBCMTD.LIB /force:MULTIPLE
# SUBTRACT LINK32 /pdb:none

!ENDIF 

# Begin Target

# Name "umatilla - Win32 Release"
# Name "umatilla - Win32 Debug"
# Begin Group "Source Files"

# PROP Default_Filter "cpp;c;cxx;rc;def;r;odl;idl;hpj;bat;f90;for;f;fpp"
# Begin Source File

SOURCE=.\caseio.f90
NODEP_F90_CASEI=\
	".\Debug\STD_HELPER.mod"\
	".\Debug\umacase.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\casemeta.f90
DEP_F90_CASEM=\
	".\Debug\scaling.mod"\
	".\Debug\umacase.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\chmcache.f90
# End Source File
# Begin Source File

SOURCE=.\chmcache_c.cpp
# End Source File
# Begin Source File

SOURCE="..\..\..\..\..\Project Lib\condor\ncp\clntkel.cpp"

!IF  "$(CFG)" == "umatilla - Win32 Release"

!ELSEIF  "$(CFG)" == "umatilla - Win32 Debug"

# SUBTRACT CPP /YX

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\clusterfit.f90
NODEP_F90_CLUST=\
	".\Debug\chmcache.mod"\
	".\Debug\condor_helper.mod"\
	".\Debug\condorio.mod"\
	".\Debug\costfun.mod"\
	".\Debug\fmt_helper.mod"\
	".\Debug\neuro_cluster.mod"\
	".\Debug\scaling.mod"\
	".\Debug\sga.mod"\
	".\Debug\STD_HELPER.mod"\
	".\Debug\umacase.mod"\
	
# End Source File
# Begin Source File

SOURCE="..\..\..\..\..\Project Lib\condor\helper\CondorHelpC.cpp"
# End Source File
# Begin Source File

SOURCE="..\..\..\..\..\Project Lib\condor\helper\CondorHelper.f90"
# End Source File
# Begin Source File

SOURCE=.\condorio.f90
NODEP_F90_CONDO=\
	".\Debug\condor_helper.mod"\
	".\Debug\STD_HELPER.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\costfun.f90
NODEP_F90_COSTF=\
	".\Debug\PARAMS.MOD"\
	
# End Source File
# Begin Source File

SOURCE=.\fmt_helper.f90
NODEP_F90_FMT_H=\
	".\Debug\umacase.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\main.f90
DEP_F90_MAIN_=\
	".\Debug\CASEIO.MOD"\
	".\Debug\chmcache.mod"\
	".\Debug\condor_helper.mod"\
	".\Debug\meta_fit.mod"\
	".\Debug\sga.mod"\
	".\Debug\STD_HELPER.mod"\
	".\Debug\umacase.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\matlab.cpp
# End Source File
# Begin Source File

SOURCE=.\metafit.f90
DEP_F90_METAF=\
	".\Debug\casemeta.mod"\
	".\Debug\chmcache.mod"\
	".\Debug\condor_helper.mod"\
	".\Debug\condorio.mod"\
	".\Debug\costfun.mod"\
	".\Debug\fmt_helper.mod"\
	".\Debug\metamodel.mod"\
	".\Debug\neural_model.mod"\
	".\Debug\scaling.mod"\
	".\Debug\sga.mod"\
	".\Debug\STD_HELPER.mod"\
	".\Debug\umacase.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\metamodel.cpp
# End Source File
# Begin Source File

SOURCE=.\metamodel_c.cpp
# End Source File
# Begin Source File

SOURCE=.\metamodel_f.f90
# End Source File
# Begin Source File

SOURCE=.\mininnet.cpp
# End Source File
# Begin Source File

SOURCE="..\..\..\..\..\Project Lib\condor\ncp\ncpjob.cpp"
# End Source File
# Begin Source File

SOURCE="..\..\..\..\..\Project Lib\condor\ncp\netsock.cpp"

!IF  "$(CFG)" == "umatilla - Win32 Release"

!ELSEIF  "$(CFG)" == "umatilla - Win32 Debug"

# SUBTRACT CPP /YX

!ENDIF 

# End Source File
# Begin Source File

SOURCE="..\..\..\..\..\Project Lib\condor\ncp\netstd.cpp"
# End Source File
# Begin Source File

SOURCE=.\neurocluster.f90
# End Source File
# Begin Source File

SOURCE=.\neuronet.f90
# End Source File
# Begin Source File

SOURCE=.\neuronet_c.cpp
# End Source File
# Begin Source File

SOURCE="..\..\..\..\..\Project Lib\condor\ncp\ofdump.cpp"
# End Source File
# Begin Source File

SOURCE=.\params.f90
# End Source File
# Begin Source File

SOURCE=.\scaling.f90
# End Source File
# Begin Source File

SOURCE=.\scaling_c.cpp
# End Source File
# Begin Source File

SOURCE=.\sga.f90
NODEP_F90_SGA_F=\
	".\Debug\chmcache.mod"\
	".\Debug\condorio.mod"\
	".\Debug\STD_HELPER.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\sgafit.f90
NODEP_F90_SGAFI=\
	".\Debug\chmcache.mod"\
	".\Debug\condor_helper.mod"\
	".\Debug\condorio.mod"\
	".\Debug\fmt_helper.mod"\
	".\Debug\sga.mod"\
	".\Debug\umacase.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\sgannfit.f90
NODEP_F90_SGANN=\
	".\Debug\chmcache.mod"\
	".\Debug\condor_helper.mod"\
	".\Debug\condorio.mod"\
	".\Debug\costfun.mod"\
	".\Debug\fmt_helper.mod"\
	".\Debug\neuronet.mod"\
	".\Debug\scaling.mod"\
	".\Debug\sga.mod"\
	".\Debug\STD_HELPER.mod"\
	".\Debug\umacase.mod"\
	
# End Source File
# Begin Source File

SOURCE="..\..\..\..\..\Project Lib\condor\helper\StdHelper.f90"
# End Source File
# Begin Source File

SOURCE=.\stdnorm.cpp
# End Source File
# Begin Source File

SOURCE=.\svm.cpp
# End Source File
# Begin Source File

SOURCE=.\tlhelper.cpp
# End Source File
# Begin Source File

SOURCE=.\umacase.f90
NODEP_F90_UMACA=\
	".\Debug\sga.mod"\
	".\Debug\STD_HELPER.mod"\
	
# End Source File
# Begin Source File

SOURCE="..\..\..\..\..\Project Lib\condor\ncp\uwkelobj.cpp"

!IF  "$(CFG)" == "umatilla - Win32 Release"

!ELSEIF  "$(CFG)" == "umatilla - Win32 Debug"

# SUBTRACT CPP /YX

!ENDIF 

# End Source File
# Begin Source File

SOURCE="..\..\..\..\..\Project Lib\condor\ncp\uwstd.cpp"

!IF  "$(CFG)" == "umatilla - Win32 Release"

!ELSEIF  "$(CFG)" == "umatilla - Win32 Debug"

# SUBTRACT CPP /YX

!ENDIF 

# End Source File
# End Group
# Begin Group "Header Files"

# PROP Default_Filter "h;hpp;hxx;hm;inl;fi;fd"
# Begin Source File

SOURCE="..\..\..\..\..\Project Lib\condor\ncp\clntkel.h"
# End Source File
# Begin Source File

SOURCE="..\..\..\..\..\Project Lib\condor\ncp\cpuinfo.h"
# End Source File
# Begin Source File

SOURCE=.\matlab.h
# End Source File
# Begin Source File

SOURCE=.\metamodel.h
# End Source File
# Begin Source File

SOURCE=.\mininnet.h
# End Source File
# Begin Source File

SOURCE="..\..\..\..\..\Project Lib\condor\ncp\msgqueue.h"
# End Source File
# Begin Source File

SOURCE="..\..\..\..\..\Project Lib\condor\ncp\ncphead.h"
# End Source File
# Begin Source File

SOURCE="..\..\..\..\..\Project Lib\condor\ncp\ncpjob.h"
# End Source File
# Begin Source File

SOURCE="..\..\..\..\..\Project Lib\condor\ncp\ncpthpool.h"
# End Source File
# Begin Source File

SOURCE="..\..\..\..\..\Project Lib\condor\ncp\netbase.h"
# End Source File
# Begin Source File

SOURCE="..\..\..\..\..\Project Lib\condor\ncp\netsock.h"
# End Source File
# Begin Source File

SOURCE="..\..\..\..\..\Project Lib\condor\ncp\netstd.h"
# End Source File
# Begin Source File

SOURCE="..\..\..\..\..\Project Lib\condor\ncp\ofdump.h"
# End Source File
# Begin Source File

SOURCE=.\stdnorm.h
# End Source File
# Begin Source File

SOURCE=.\svm.h
# End Source File
# Begin Source File

SOURCE=.\tlhelper.h
# End Source File
# Begin Source File

SOURCE="..\..\..\..\..\Project Lib\condor\ncp\uwkelobj.h"
# End Source File
# Begin Source File

SOURCE="..\..\..\..\..\Project Lib\condor\ncp\uwstd.h"
# End Source File
# End Group
# Begin Group "Resource Files"

# PROP Default_Filter "ico;cur;bmp;dlg;rc2;rct;bin;rgs;gif;jpg;jpeg;jpe"
# End Group
# End Target
# End Project
