#ifndef _READWRITESYNC
#define _READWRITESYNC
/*
  读写线程同步管理者 ReadWriteSync
  该类用于多个读者、多个写者之间的同步，同步原理见 <WindowsNT 高级编程技术>第9章
  使用方法:
  1.首先声名全局变量 
	如:	ReadWriteSync TheLock("bucket");

  2.在读线程进入同步区之前，设局部读锁，并与同步代码放在{}中
    如：
		{
      		ReadWriteSync::ReadLock Lock( TheLock );
			//以下为保护代码
      		for( int i=0; i<MAX_BALLS; i++ ){
      			int k = g_Bucket[i];
        		nBallColor[k]++;
      		}
		}
  3.在写线程进入同步区之前，设局部写锁，并与同步代码放在{}中
    如:
	while( !g_lTerminate ){
		Sleep( 100*(pScrollBar->GetScrollPos()+1) );
		ReadWriteSync::WriteLock Lock( TheLock );
		//以下为保护代码
		g_Bucket[ rand()%MAX_BALLS ] = (BALLCOLOR)(rand()%10);
    }



*/
#include <windows.h>

class ReadWriteSync
{
public:
	enum { NoLimit=-1 };
	ReadWriteSync( LPCSTR Name );	//Name 同步管理者名称前缀
	~ReadWriteSync( );				
	void Release();					//释放核心对象，析构函数中会自动调用
	BOOL ConstructName( LPCSTR Prefix, LPCSTR Suffix, LPSTR Name, size_t cbName );
				//建立核心对象名称，内部使用
	HANDLE ReaderHandle();
	HANDLE WriterHandle();
	HANDLE NumReadersHandle();

	class ReadLock
	{
	public:
		ReadLock( ReadWriteSync& t, DWORD timeOut=NoLimit );
		~ReadLock();
		void Release();
	private:
		ReadWriteSync& RWObj;
		BOOL Ok;
	};
	class WriteLock
	{
	public:
		WriteLock( ReadWriteSync&, DWORD timeOut=NoLimit );
		~WriteLock();
		void Release();
	private:
		ReadWriteSync& RWObj;
		BOOL Ok;
	};

private:
//	ReadWriteSync( const ReadWriteSync& );
//	const ReadWriteSync& operator = ( const ReadWriteSync& );

	HANDLE hMutexNoWriter;
	HANDLE hEventNoReader;
	HANDLE hSemNumReaders;
};

inline ReadWriteSync::~ReadWriteSync()
{
	Release();
}

inline HANDLE ReadWriteSync::ReaderHandle()
{
	return hMutexNoWriter;
}

inline HANDLE ReadWriteSync::WriterHandle()
{
	return hEventNoReader;
}

inline HANDLE ReadWriteSync::NumReadersHandle()
{
	return hSemNumReaders;
}

inline ReadWriteSync::ReadLock::~ReadLock()
{
	Release();
}

inline ReadWriteSync::WriteLock::~WriteLock()
{
	Release();
}

#endif
