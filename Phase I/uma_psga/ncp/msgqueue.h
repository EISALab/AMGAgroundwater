#ifndef _MSGQUEUE_H_
#define _MSGQUEUE_H_

#include "uwkelobj.h"
#include "ncphead.h"
#include <list>

class CMessageQueue
{
private:
	HANDLE m_hSema;
	HANDLE m_hMutex;
	std::list< CMessageBase* > m_MsgQueue;
public:
	class CMessageQueueLocker
	{
	private:
		CMessageQueue* m_pMsgQueue;
	public:
		CMessageQueueLocker( CMessageQueue* pMsgQueue ):m_pMsgQueue(pMsgQueue){
			m_pMsgQueue->Lock();
		}
		~CMessageQueueLocker(){
			m_pMsgQueue->Unlock();
		}
	};
public:
	CMessageQueue(){
		m_hMutex = CreateMutex();
		m_hSema = CreateSemaphore();
	};
	~CMessageQueue(){
		CloseHandle( m_hMutex );
		CloseHandle( m_hSema );
	}
	void Lock(){
		LockMutex( m_hMutex );
	}
	void Unlock(){
		UnlockMutex( m_hMutex );
	}
	void Enqueue( CMessageBase* pMsg ){
		CMessageQueueLocker locker(this);
		m_MsgQueue.push_back( pMsg );
	}
	CMessageBase* Dequeue(){
		CMessageQueueLocker locker(this);
		CMessageBase* pMsg = m_MsgQueue.front();
		m_MsgQueue.pop_front();
		return pMsg;
	}
public:
	CMessageBase* GetMessage(){
		WaitForSemaphore( m_hSema );
		return Dequeue();
	}
	void PutMessage( CMessageBase* pMsg ){
		Enqueue( pMsg );
		ReleaseSemaphore( m_hSema );
	}
	bool IsEmpty(){
		CMessageQueueLocker locker(this);
		return m_MsgQueue.empty();
	}
	int GetSize(){
		CMessageQueueLocker locker(this);
		return (int)m_MsgQueue.size();
	}
};

#endif	//_MSGQUEUE_H_
