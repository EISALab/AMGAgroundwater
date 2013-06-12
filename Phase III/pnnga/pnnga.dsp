# Microsoft Developer Studio Project File - Name="pnnga" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Console Application" 0x0103

CFG=pnnga - Win32 Debug
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "pnnga.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "pnnga.mak" CFG="pnnga - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "pnnga - Win32 Release" (based on "Win32 (x86) Console Application")
!MESSAGE "pnnga - Win32 Debug" (based on "Win32 (x86) Console Application")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
F90=df.exe
RSC=rc.exe

!IF  "$(CFG)" == "pnnga - Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "pnnga___Win32_Release"
# PROP BASE Intermediate_Dir "pnnga___Win32_Release"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "Release"
# PROP Intermediate_Dir "Release"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE F90 /compile_only /nologo /warn:nofileopt
# ADD F90 /compile_only /nologo /warn:nofileopt
# ADD BASE CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_CONSOLE" /D "_MBCS" /YX /FD /c
# ADD CPP /nologo /MT /W3 /GX /O2 /I "../ncp" /D "WIN32" /D "NDEBUG" /D "_CONSOLE" /D "_MBCS" /YX /FD /c
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /machine:I386
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib rt3d.lib mflow.lib Ws2_32.lib /nologo /subsystem:console /machine:I386 /DEFAULTLIB:LIBCMT.LIB /force:multiple
# SUBTRACT LINK32 /pdb:none

!ELSEIF  "$(CFG)" == "pnnga - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "pnnga___Win32_Debug"
# PROP BASE Intermediate_Dir "pnnga___Win32_Debug"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "Debug"
# PROP Intermediate_Dir "Debug"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE F90 /check:bounds /compile_only /dbglibs /debug:full /nologo /traceback /warn:argument_checking /warn:nofileopt
# ADD F90 /check:bounds /compile_only /dbglibs /debug:full /nologo /traceback /warn:argument_checking /warn:nofileopt
# SUBTRACT F90 /browser
# ADD BASE CPP /nologo /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_CONSOLE" /D "_MBCS" /YX /FD /GZ /c
# ADD CPP /nologo /MTd /W3 /Gm /GR /GX /ZI /Od /I "d:/project lib/condor/ncp" /I "d:/project lib/condor/helper" /D "WIN32" /D "_DEBUG" /D "_CONSOLE" /D "_MBCS" /YX /FD /GZ /c
# SUBTRACT CPP /Fr
# ADD BASE RSC /l 0x409 /d "_DEBUG"
# ADD RSC /l 0x409 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /debug /machine:I386 /pdbtype:sept
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib rt3d.lib mflow.lib Ws2_32.lib /nologo /subsystem:console /incremental:no /debug /machine:I386 /pdbtype:sept /force:MULTIPLE /DEFAULTLIB:LIBCMTD.LIB
# SUBTRACT LINK32 /pdb:none

!ENDIF 

# Begin Target

# Name "pnnga - Win32 Release"
# Name "pnnga - Win32 Debug"
# Begin Group "Source Files"

# PROP Default_Filter "cpp;c;cxx;rc;def;r;odl;idl;hpj;bat;f90;for;f;fpp"
# Begin Source File

SOURCE=.\case.f90
DEP_F90_CASE_=\
	".\Debug\SGA.MOD"\
	".\Debug\STD_HELPER.MOD"\
	
# End Source File
# Begin Source File

SOURCE=.\caseio.f90
DEP_F90_CASEI=\
	".\Debug\casewell.mod"\
	".\Debug\STD_HELPER.MOD"\
	
# End Source File
# Begin Source File

SOURCE=.\casemeta.f90
# End Source File
# Begin Source File

SOURCE="..\..\..\..\..\..\Project Lib\condor\helper\chmcache.f90"
# End Source File
# Begin Source File

SOURCE="..\..\..\..\..\..\Project Lib\condor\helper\chmcache_c.cpp"
# End Source File
# Begin Source File

SOURCE="..\..\..\..\..\..\Project Lib\condor\ncp\clntkel.cpp"
# End Source File
# Begin Source File

SOURCE=.\condorio.f90
DEP_F90_CONDO=\
	".\Debug\casewell.mod"\
	".\Debug\ncpfor.mod"\
	".\Debug\STD_HELPER.MOD"\
	".\Debug\stdfor.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\costfunc.f90
# End Source File
# Begin Source File

SOURCE=.\fmt_helper.f90
DEP_F90_FMT_H=\
	".\Debug\casewell.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\main.f90
DEP_F90_MAIN_=\
	".\Debug\CASEIO.mod"\
	".\Debug\casewell.mod"\
	".\Debug\chmcache.mod"\
	".\Debug\sampling.mod"\
	".\Debug\SGA.MOD"\
	".\Debug\sgafit.mod"\
	".\Debug\STD_HELPER.MOD"\
	".\Debug\stdfor.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\mininnet.cpp
# End Source File
# Begin Source File

SOURCE=.\ncpfit.cpp
# End Source File
# Begin Source File

SOURCE="..\..\..\..\..\..\Project Lib\condor\helper\ncpfor.f90"
# End Source File
# Begin Source File

SOURCE="..\..\..\..\..\..\Project Lib\condor\helper\ncpfor_c.cpp"
# End Source File
# Begin Source File

SOURCE="..\..\..\..\..\..\Project Lib\condor\ncp\ncpjob.cpp"
# End Source File
# Begin Source File

SOURCE="..\..\..\..\..\..\Project Lib\condor\ncp\netsock.cpp"
# End Source File
# Begin Source File

SOURCE="..\..\..\..\..\..\Project Lib\condor\ncp\netstd.cpp"
# End Source File
# Begin Source File

SOURCE=.\Neuroio.f90
DEP_F90_NEURO=\
	".\Debug\casewell.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\neuronet.f90
# End Source File
# Begin Source File

SOURCE=.\neuronet_c.cpp
# End Source File
# Begin Source File

SOURCE="..\..\..\..\..\..\Project Lib\condor\ncp\ofdump.cpp"
# End Source File
# Begin Source File

SOURCE=.\Params.f90
# End Source File
# Begin Source File

SOURCE=.\pdeio.f90
DEP_F90_PDEIO=\
	".\Debug\casewell.mod"\
	".\Debug\STD_HELPER.MOD"\
	
# End Source File
# Begin Source File

SOURCE=.\sampling.f90
DEP_F90_SAMPL=\
	".\Debug\casewell.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\scaling.f90
# End Source File
# Begin Source File

SOURCE=.\scaling_c.cpp
# End Source File
# Begin Source File

SOURCE=.\sga.f90
DEP_F90_SGA_F=\
	".\Debug\sampling.mod"\
	".\Debug\STD_HELPER.MOD"\
	
# End Source File
# Begin Source File

SOURCE=.\sgafit.f90
DEP_F90_SGAFI=\
	".\Debug\casemeta.mod"\
	".\Debug\casewell.mod"\
	".\Debug\chmcache.mod"\
	".\Debug\condorio.mod"\
	".\Debug\costfunc.mod"\
	".\Debug\fmt_helper.mod"\
	".\Debug\ncpfor.mod"\
	".\Debug\neuroio.mod"\
	".\Debug\neuronet.mod"\
	".\Debug\SGA.MOD"\
	".\Debug\STD_HELPER.MOD"\
	".\Debug\stdfor.mod"\
	
# End Source File
# Begin Source File

SOURCE="..\..\..\..\..\..\Project Lib\condor\helper\stdfor.f90"
# End Source File
# Begin Source File

SOURCE="..\..\..\..\..\..\Project Lib\condor\helper\stdfor_c.cpp"
# End Source File
# Begin Source File

SOURCE="..\..\..\..\..\..\Project Lib\condor\helper\StdHelper.f90"
# End Source File
# Begin Source File

SOURCE="..\..\..\..\..\..\Project Lib\condor\helper\stdnorm.cpp"
# End Source File
# Begin Source File

SOURCE="..\..\..\..\..\..\Project Lib\condor\ncp\uwkelobj.cpp"
# End Source File
# Begin Source File

SOURCE="..\..\..\..\..\..\Project Lib\condor\ncp\uwstd.cpp"
# End Source File
# End Group
# Begin Group "Header Files"

# PROP Default_Filter "h;hpp;hxx;hm;inl;fi;fd"
# Begin Source File

SOURCE="..\..\..\..\..\..\Project Lib\condor\ncp\clntkel.h"
# End Source File
# Begin Source File

SOURCE="..\..\..\..\..\..\Project Lib\condor\ncp\cpuinfo.h"
# End Source File
# Begin Source File

SOURCE=.\mininnet.h
# End Source File
# Begin Source File

SOURCE="..\..\..\..\..\..\Project Lib\condor\ncp\msgqueue.h"
# End Source File
# Begin Source File

SOURCE="..\..\..\..\..\..\Project Lib\condor\helper\ncpfor_c.h"
# End Source File
# Begin Source File

SOURCE="..\..\..\..\..\..\Project Lib\condor\ncp\ncphead.h"
# End Source File
# Begin Source File

SOURCE="..\..\..\..\..\..\Project Lib\condor\ncp\ncpjob.h"
# End Source File
# Begin Source File

SOURCE="..\..\..\..\..\..\Project Lib\condor\ncp\ncpthpool.h"
# End Source File
# Begin Source File

SOURCE="..\..\..\..\..\..\Project Lib\condor\ncp\netbase.h"
# End Source File
# Begin Source File

SOURCE="..\..\..\..\..\..\Project Lib\condor\ncp\netsock.h"
# End Source File
# Begin Source File

SOURCE="..\..\..\..\..\..\Project Lib\condor\ncp\netstd.h"
# End Source File
# Begin Source File

SOURCE="..\..\..\..\..\..\Project Lib\condor\ncp\ofdump.h"
# End Source File
# Begin Source File

SOURCE=.\scaling_c.h
# End Source File
# Begin Source File

SOURCE="..\..\..\..\..\..\Project Lib\condor\helper\stdfor_c.h"
# End Source File
# Begin Source File

SOURCE="..\..\..\..\..\..\Project Lib\condor\helper\stdnorm.h"
# End Source File
# Begin Source File

SOURCE="..\..\..\..\..\..\Project Lib\condor\ncp\uwkelobj.h"
# End Source File
# Begin Source File

SOURCE="..\..\..\..\..\..\Project Lib\condor\ncp\uwstd.h"
# End Source File
# End Group
# Begin Group "Resource Files"

# PROP Default_Filter "ico;cur;bmp;dlg;rc2;rct;bin;rgs;gif;jpg;jpeg;jpe"
# End Group
# End Target
# End Project
