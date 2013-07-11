#include "../ncphead.h"

#pragma once

class CClientSession
{
private:
	SOCKET		m_sock;
	int			m_nClntId;
	int			m_nServId;
	int			m_nFileMode;		//file open mode
protected:
	int DoLogOn( const char* strUser, const char* strPass );
	int DoLogOff();
	//file operations
	int DoPassive( short* pport );
	int DoPutFile( const char* strSrc, const char* strDst, bool bOverwrite );
	int DoGetFile( const char* strSrc, const char* strDst );
	int DoRemFile( const char* strName );
//	int DoList();
	int DoChMod( const char* strName, int nMode );
	int DoSoftLink( const char* strSrc, const char* strLink ); 
	int DoFileMode( int nFileMode );

	//directory operations
	int DoMkDir( const char* strDir );
	int DoChDir( const char* strDir );
	int DoRmDir( const char* strDir );

	//other operations
	int DoRunImage( const char* strImage, bool bSync=true );
	int DoGetSysInfo( SYSINFO* pSysInfo );
	int DoKillProcs( );
	int DoSetIdle( BOOL bIdle );

public:
	CClientSession( );
	~CClientSession();

	//connect and disconnect.
	int Connect( SOCKADDR_IN* srvAddr, const char* strUser, const char* strPass );
	int Disconnect();
	void KillConnect();

	//file operation
	int PutFile( const char* strSrc, const char* strDst, bool bOverwrite=true );
	int GetFile( const char* strSrc, const char* strDst );
	int Remove( const char* strName );
	int ChMod( const char* strName, int nMode );
	int SoftLink( const char* strSrc, const char* strLink );	//create a link linking to strSrc.
	int FileMode( int nFileMode );

	//directory operation
	int ChDir( const char* strDir );
	int MkDir( const char* strDir );
	int RmDir( const char* strDir );

	//run executable.
	int RunImage( const char* strImage, bool bSync=true );
	//system information
	int GetSysInfo( SYSINFO* pSysInfo );
	//kill all waiting processes
	int KillProcs( );
	int SetIdle( BOOL bIdle );
};

