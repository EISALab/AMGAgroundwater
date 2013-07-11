#ifndef _CPUINFO_H_
#define _CPUINFO_H_

#ifdef _WIN32
#include <windows.h>
typedef unsigned __int64 uint64;

#else
#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>
#include <sys/time.h>
typedef unsigned long long uint64;

#endif
#include "uwkelobj.h"

#ifdef __sparc__
        #define get_tick(val) __asm__ __volatile__("rd %%tick, %%o1\n\tsrlx %%o1,32,%0" : "=r" (val))
#endif
#ifdef __i386__
        #define get_tick(val) __asm__ __volatile__("rdtsc" : "=A" (val))
#endif

#ifdef __ia64__
		#define get_tick(val) __asm__ __volatile__("mov %0=ar.itc" : "=r"(val) :: "memory");
#endif

#ifdef _M_IX86
        #define get_tick(val) __asm rdtsc  __asm mov dword ptr [val], eax
#endif

inline int GetCpuSpeed()
{
	uint64 tick_start, tick_stop;
	double tm_start, tm_stop;

	tm_start = GetMilliTime();
	get_tick(tick_start); 
	MilliSleep(1);
	tm_stop = GetMilliTime();
	get_tick(tick_stop);

	return (int) ( (tick_stop - tick_start) / (tm_stop-tm_start) / 1000 );
}

struct cpu_info
{
	int nCpuType;
	int nCpuMhz;
	int nCpus;
};
typedef struct cpu_info CPU_INFO;

inline void GetCpuInfo( CPU_INFO* pinfo )
{
	pinfo->nCpuMhz = GetCpuSpeed();
	pinfo->nCpuType = 0;

#ifdef _WIN32
	SYSTEM_INFO sysinfo;
	GetSystemInfo( &sysinfo );
	pinfo->nCpus = sysinfo.dwNumberOfProcessors;
#else
	pinfo->nCpus = sysconf( _SC_NPROCESSORS_ONLN );
#endif
}

#endif
