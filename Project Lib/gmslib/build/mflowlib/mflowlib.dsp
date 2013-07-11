# Microsoft Developer Studio Project File - Name="mflowlib" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Static Library" 0x0104

CFG=mflowlib - Win32 Debug
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "mflowlib.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "mflowlib.mak" CFG="mflowlib - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "mflowlib - Win32 Release" (based on "Win32 (x86) Static Library")
!MESSAGE "mflowlib - Win32 Debug" (based on "Win32 (x86) Static Library")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
F90=df.exe
RSC=rc.exe

!IF  "$(CFG)" == "mflowlib - Win32 Release"

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
# ADD BASE F90 /compile_only /include:"Release/" /nologo /warn:nofileopt
# ADD F90 /compile_only /include:"Release/" /nologo /warn:nofileopt
# SUBTRACT F90 /fpp
# ADD BASE CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_MBCS" /YX /FD /c
# ADD CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_MBCS" /YX /FD /c
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LIB32=link.exe -lib
# ADD BASE LIB32 /nologo
# ADD LIB32 /nologo /out:"../../lib/mflow.lib"

!ELSEIF  "$(CFG)" == "mflowlib - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "Debug"
# PROP BASE Intermediate_Dir "Debug"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "Debug"
# PROP Intermediate_Dir "Debug"
# PROP Target_Dir ""
# ADD BASE F90 /check:bounds /compile_only /debug:full /include:"Debug/" /nologo /warn:argument_checking /warn:nofileopt
# ADD F90 /check:bounds /compile_only /debug:full /define:"_DEBUG" /include:"Debug/" /nologo /warn:argument_checking /warn:nofileopt
# SUBTRACT F90 /fpp
# ADD BASE CPP /nologo /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_MBCS" /YX /FD /GZ /c
# ADD CPP /nologo /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_MBCS" /YX /FD /GZ /c
# ADD BASE RSC /l 0x409 /d "_DEBUG"
# ADD RSC /l 0x409 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LIB32=link.exe -lib
# ADD BASE LIB32 /nologo
# ADD LIB32 /nologo /out:"../../lib/mflowd.lib"

!ENDIF 

# Begin Target

# Name "mflowlib - Win32 Release"
# Name "mflowlib - Win32 Debug"
# Begin Group "Source Files"

# PROP Default_Filter "cpp;c;cxx;rc;def;r;odl;idl;hpj;bat;f90;for;f;fpp"
# Begin Source File

SOURCE=.\bas5.f
# End Source File
# Begin Source File

SOURCE=.\bcf5.f
DEP_F90_BCF5_=\
	".\Debug\DryHandle.mod"\
	".\Debug\TrueLayer.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\chd1.f
# End Source File
# Begin Source File

SOURCE=.\de45.f
# End Source File
# Begin Source File

SOURCE=.\drn5.f
DEP_F90_DRN5_=\
	".\Debug\Observation.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\evt5.f
# End Source File
# Begin Source File

SOURCE=.\fhb1.f
# End Source File
# Begin Source File

SOURCE=.\gfd1.f
# End Source File
# Begin Source File

SOURCE=.\ghb5.f
# End Source File
# Begin Source File

SOURCE=.\hfb1.f
# End Source File
# Begin Source File

SOURCE=.\ibs1.f
# End Source File
# Begin Source File

SOURCE=.\lkmt3.f
# End Source File
# Begin Source File

SOURCE=.\mf96.f
DEP_F90_MF96_=\
	".\lkmt3.inc"\
	
# ADD F90 /fpp
# End Source File
# Begin Source File

SOURCE=.\modsub.f
DEP_F90_MODSU=\
	".\Debug\Observation.mod"\
	".\Debug\TrueLayer.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\pcg2.f
# End Source File
# Begin Source File

SOURCE=.\rch5.f
DEP_F90_RCH5_=\
	".\Debug\Observation.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\res1.f
# End Source File
# Begin Source File

SOURCE=.\riv5.f
DEP_F90_RIV5_=\
	".\Debug\Observation.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\sip5.f
# End Source File
# Begin Source File

SOURCE=.\sor5.f
# End Source File
# Begin Source File

SOURCE=.\str1.f
# End Source File
# Begin Source File

SOURCE=.\tlk1.f
# End Source File
# Begin Source File

SOURCE=.\utl5.f
DEP_F90_UTL5_=\
	".\Debug\Observation.mod"\
	".\Debug\TrueLayer.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\wel5.f
# End Source File
# End Group
# Begin Group "Header Files"

# PROP Default_Filter "h;hpp;hxx;hm;inl;fi;fd"
# End Group
# End Target
# End Project
