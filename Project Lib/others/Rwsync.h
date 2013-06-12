#ifndef _READWRITESYNC
#define _READWRITESYNC
/*
  ��д�߳�ͬ�������� ReadWriteSync
  �������ڶ�����ߡ����д��֮���ͬ����ͬ��ԭ��� <WindowsNT �߼���̼���>��9��
  ʹ�÷���:
  1.��������ȫ�ֱ��� 
	��:	ReadWriteSync TheLock("bucket");

  2.�ڶ��߳̽���ͬ����֮ǰ����ֲ�����������ͬ���������{}��
    �磺
		{
      		ReadWriteSync::ReadLock Lock( TheLock );
			//����Ϊ��������
      		for( int i=0; i<MAX_BALLS; i++ ){
      			int k = g_Bucket[i];
        		nBallColor[k]++;
      		}
		}
  3.��д�߳̽���ͬ����֮ǰ����ֲ�д��������ͬ���������{}��
    ��:
	while( !g_lTerminate ){
		Sleep( 100*(pScrollBar->GetScrollPos()+1) );
		ReadWriteSync::WriteLock Lock( TheLock );
		//����Ϊ��������
		g_Bucket[ rand()%MAX_BALLS ] = (BALLCOLOR)(rand()%10);
    }



*/
#include <windows.h>

class ReadWriteSync
{
public:
	enum { NoLimit=-1 };
	ReadWriteSync( LPCSTR Name );	//Name ͬ������������ǰ׺
	~ReadWriteSync( );				
	void Release();					//�ͷź��Ķ������������л��Զ�����
	BOOL ConstructName( LPCSTR Prefix, LPCSTR Suffix, LPSTR Name, size_t cbName );
				//�������Ķ������ƣ��ڲ�ʹ��
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
