# Microsoft Developer Studio Project File - Name="rt3dlib" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Static Library" 0x0104

CFG=rt3dlib - Win32 Debug
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "rt3dlib.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "rt3dlib.mak" CFG="rt3dlib - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "rt3dlib - Win32 Release" (based on "Win32 (x86) Static Library")
!MESSAGE "rt3dlib - Win32 Debug" (based on "Win32 (x86) Static Library")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
F90=df.exe
RSC=rc.exe

!IF  "$(CFG)" == "rt3dlib - Win32 Release"

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
# ADD LIB32 /nologo /out:"../../lib/rt3d.lib"

!ELSEIF  "$(CFG)" == "rt3dlib - Win32 Debug"

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
# ADD LIB32 /nologo /out:"../../lib/rt3dd.lib"

!ENDIF 

# Begin Target

# Name "rt3dlib - Win32 Release"
# Name "rt3dlib - Win32 Debug"
# Begin Group "Source Files"

# PROP Default_Filter "cpp;c;cxx;rc;def;r;odl;idl;hpj;bat;f90;for;f;fpp"
# Begin Source File

SOURCE=.\adv30d.f
# End Source File
# Begin Source File

SOURCE=.\btnrtv2.f
DEP_F90_BTNRT=\
	".\Debug\SSMDECAY_ARRAY.mod"\
	
# ADD F90 /fpp
# End Source File
# Begin Source File

SOURCE=.\dsp30d.f
# End Source File
# Begin Source File

SOURCE=.\fmi30d.f
# End Source File
# Begin Source File

SOURCE=.\gcg30d.f
# End Source File
# Begin Source File

SOURCE=.\jacrxns.f
# End Source File
# Begin Source File

SOURCE=.\rt3dv2.f
# ADD F90 /fpp
# End Source File
# Begin Source File

SOURCE=.\rteqnv2.f
DEP_F90_RTEQN=\
	".\Debug\ARRAY_CONFIG.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\rtrctv2.f
DEP_F90_RTRCT=\
	".\Debug\ARRAY_CONFIG.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\rxns.f
# End Source File
# Begin Source File

SOURCE=.\slvr2dum.f
# End Source File
# Begin Source File

SOURCE=.\solver1.f
# End Source File
# Begin Source File

SOURCE=.\ssmrtv2.f
DEP_F90_SSMRT=\
	".\Debug\SSMDECAY_ARRAY.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\utlrtv2.f
# End Source File
# End Group
# Begin Group "Header Files"

# PROP Default_Filter "h;hpp;hxx;hm;inl;fi;fd"
# End Group
# End Target
# End Project
