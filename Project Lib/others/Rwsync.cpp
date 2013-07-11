#include "rwsync.h"

ReadWriteSync::ReadWriteSync( LPCSTR Name )
{
	hMutexNoWriter = NULL;
	hEventNoReader = NULL;
	hSemNumReaders = NULL;

	char buffer[80];

	if( ConstructName( "YSQMutexNoWriter", Name, buffer, sizeof(buffer) ) ){
		hMutexNoWriter = CreateMutex( NULL, FALSE, buffer );
	}
	if( ConstructName( "YSQEventNoReader", Name, buffer, sizeof(buffer) ) ){
		hEventNoReader = CreateEvent( NULL, TRUE, TRUE, buffer );
	}
	if( ConstructName( "YSQSemNumReaders", Name, buffer, sizeof(buffer) ) ){
		hSemNumReaders = CreateSemaphore( NULL, TRUE, 0x7fffffff, buffer );
	}

	if( hMutexNoWriter==NULL || hEventNoReader==NULL || hSemNumReaders==NULL ){
		Release();
	}
}

void ReadWriteSync::Release()
{
	if( hMutexNoWriter ){
		CloseHandle( hMutexNoWriter );
	}
	if( hEventNoReader ){
		CloseHandle( hEventNoReader );
	}
	if( hSemNumReaders ){
		CloseHandle( hSemNumReaders );
	}
}

BOOL ReadWriteSync::ConstructName( LPCSTR Prefix, LPCSTR Suffix, LPSTR Name, size_t cbName )
{
	if( strlen(Prefix)+strlen(Suffix)+1 > cbName ) return FALSE;

	strcat( strcpy(Name, Prefix), Suffix );
	return TRUE;
}

ReadWriteSync::ReadLock::ReadLock( ReadWriteSync& t, DWORD timeOut )
	: RWObj( t )
{
	DWORD dw;

	Ok = FALSE;
	dw = WaitForSingleObject( RWObj.WriterHandle(), timeOut );
	if( dw!=WAIT_TIMEOUT ){
		LONG PreviousCount;
		ReleaseSemaphore( RWObj.NumReadersHandle(), 1, &PreviousCount );
		if( PreviousCount==0 ){
			ResetEvent( RWObj.ReaderHandle() );
		}
		ReleaseMutex( RWObj.WriterHandle() );
		Ok = TRUE;
	}
}

void ReadWriteSync::ReadLock::Release()
{
	if( !Ok )return;

	HANDLE Handles[2];
	BOOL LastReader;

	Handles[0]=RWObj.WriterHandle();
	Handles[1]=RWObj.NumReadersHandle();
	WaitForMultipleObjects( 2, Handles, TRUE, INFINITE );
	LastReader = (WaitForSingleObject( RWObj.NumReadersHandle(), 0 )==WAIT_TIMEOUT);

	if( LastReader ){
		SetEvent( RWObj.ReaderHandle() );
	}else{
		ReleaseSemaphore( RWObj.NumReadersHandle(), 1, NULL );
	}

	ReleaseMutex( RWObj.WriterHandle() );
}

ReadWriteSync::WriteLock::WriteLock( ReadWriteSync& t, DWORD timeOut )
	:RWObj( t )
{
	DWORD dw;
	HANDLE Handles[2];

	Ok = FALSE;
	Handles[0]=RWObj.WriterHandle();
	Handles[1]=RWObj.ReaderHandle();
	dw = WaitForMultipleObjects( 2, Handles, TRUE, timeOut );
	if( dw!=WAIT_TIMEOUT ){
		Ok = TRUE;
	}
}

void ReadWriteSync::WriteLock::Release()
{
	if( !Ok )return;

	ReleaseMutex( RWObj.WriterHandle() );
}






