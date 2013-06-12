#ifndef _NCPFOR_C_H_
#define _NCPFOR_C_H_

#include <sys/types.h>
#include <stdlib.h>
#include <memory.h>
#include <stdio.h>
#include <string.h>
#include <sys/stat.h>
#include <ctype.h>

#ifdef _WIN32
#include <winsock2.h>
#include <windows.h>
#include <io.h>
#include <direct.h>

typedef long intptr_t;
#else
#include <unistd.h>
#include <dirent.h>

#define _stdcall
#endif

#include "mixlang.h"
#include "clntkel.h"
#include "ncpjob.h"
#include "netstd.h"
#include "ofdump.h"

#define MAXPATH 256

void ncp_initialize();
void ncp_refresh_nodes();
void ncp_cleanup();
void ncp_distribute_jobs( int* pnJobIds, int* pnJobCount );

bool SendDir( CClientSession* pSession, const char* strDir, bool bOverwrite );
void LinkDir( CClientSession* pSession, const char* strDir );

#endif