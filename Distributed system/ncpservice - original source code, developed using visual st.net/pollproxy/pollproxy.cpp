#include "netbase.h"
#include "ncphead.h"
#include "netsock.h"
#include "netstd.h"
#include <set>
#include <map>
#include <vector>
// pollproxy.cpp : Defines the entry point for the console application.
//

#define	NCP_POLLPROXY_PORT	(NCP_STREAM_PORT+1)

class CSimpleTimer
{
	//pair.first is the last tick time, pair.second is time out value.
	std::vector< pair<double, double> > m_Timers;
public:
	CSimpleTimer(){};
	int SetTimer( int nMilliTimeOut, double tmNow ){
		m_Timers.push_back( make_pair<double, double>( tmNow, nMilliTimeOut ) );
		return m_Timers.size()-1;
	}
	bool IsTimeOut( int id, double tmNow ){
		ASSERT( id<m_Timers.size() );
		if( tmNow-m_Timers[id].first >= m_Timers[id].second ){
			m_Timers[id].first = tmNow;
			return true;
		}
		return false;
	}
};

class CPollProxy
{
public:
	HANDLE								m_hMutex;
	std::set<IN_ADDR>					m_HostAddrs;
	std::map<IN_ADDR, CMessageBase*>	m_CachedMessages;
	string								m_strHostFile;

	void Lock(){ LockMutex(m_hMutex); }
	void Unlock(){ UnlockMutex(m_hMutex); }

public:
	CPollProxy(){ m_hMutex = CreateMutex(); }
	~CPollProxy(){ CloseHandle(m_hMutex); }

	void RunRemoteServer();
	void RunLocalServer();

	void DoPoll( CSocket& sock );
	void AckPoll( CSocket& sock, CSockAddrIn& addr );
	void OnAckPoll( CMessageBase* pMsg );

	void ForwardMessages( CSocket& sock );
	void LoadHostFile( const char* strFile );

};

class op_addr_host
{
public:
	set< IN_ADDR >&	 m_HostAddrs;
public:
	op_addr_host( set<IN_ADDR>& addr_set ):m_HostAddrs(addr_set){}
	void operator() (IN_ADDR addrHost)
	{
		if( m_HostAddrs.find(addrHost)==m_HostAddrs.end() ){
			m_HostAddrs.insert( addrHost );
		}
	}
};

CPollProxy pollProxy;


void CPollProxy::LoadHostFile( const char* strNodeFile )
{
	CObjLocker<CPollProxy> locker(this);
	m_HostAddrs.clear();

	if( IsFileExist(strNodeFile) ){
		ParseHostFile( strNodeFile, op_addr_host(m_HostAddrs) );
	}else{
		if( strNodeFile[0]==NULL ){
			m_HostAddrs.insert( GetLocalInAddr() );
		}else{
			ParseHostName( strNodeFile, op_addr_host(m_HostAddrs) );
		}
	}
}

void CPollProxy::ForwardMessages( CSocket& sock )
{
//	cdump<<"Forward message ... "<<endl;
	CObjLocker<CPollProxy> locker(this);

	map<IN_ADDR, CMessageBase*>::iterator pos;
	for( pos=m_CachedMessages.begin(); pos!=m_CachedMessages.end(); ++pos ){
		CMessageBase* pMsg = pos->second;
		SendMessage( sock, pMsg );
		//note, must set the message back to host order, because SendMessage change its byte order.
		pMsg->ntoh();
	}

//	cdump<<"Forward message ... return"<<endl;
}

//poll all the registered hosts
void CPollProxy::DoPoll( CSocket& sock )
{
//	cdump<<"DoPoll..."<<endl;

	vector<IN_ADDR> arrAddrs;
	{
		CObjLocker<CPollProxy> locker(this);
		arrAddrs.resize( m_HostAddrs.size() );
		copy( m_HostAddrs.begin(), m_HostAddrs.end(), arrAddrs.begin() );
	}

	CSockAddrIn srcaddr;
	sock.GetSockName( srcaddr );

	for( int i=0; i<arrAddrs.size(); i++ ){
		CMessageBase msg;
		msg.Init( NCM_POLLSERVER, CSessionAddr(), CSessionAddr(), srcaddr.GetPort() );
		CSockAddrIn dstaddr( arrAddrs[i], NCP_SERVER_PING_PORT );
//		cdump<<"polling host:"<<inet_ntoa(arrAddrs[i])<<endl;
		SendMessageTo( sock, &msg, dstaddr );
	}

//	cdump<<"DoPoll... return"<<endl;
}

void CPollProxy::AckPoll( CSocket& sock, CSockAddrIn& addr )
{
//	cdump<<"AckPoll..."<<endl;

	ASSERT( m_HostAddrs.size()<=1 );
	
	IN_ADDR hostTunnel;
	if( m_HostAddrs.size()>0 ){
		hostTunnel = *(m_HostAddrs.begin());
	}else{
		hostTunnel = GetLocalInAddr();
		//Inet_InAddr( "localhost", hostTunnel );
	}

	CSocket sock_stream;

	CSockAddrIn addrTunnel( hostTunnel, NCP_POLLPROXY_PORT );
	sock_stream.Open( addrTunnel );
//	sock_stream.Open( "localhost", NCP_POLLPROXY_PORT );

	try{
		while( true ){
			CMessageBase* pMsg = RecvMessage<CMessageBase>( sock_stream );
			if( pMsg==NULL )break;

			CSessionAddr srcaddr = pMsg->GetSrcAddr();
//			pMsg->SetSrcAddr( CSessionAddr(GetHostInAddr("localhost"), srcaddr.m_nServId) );
			pMsg->SetSrcAddr( CSessionAddr(hostTunnel, srcaddr.m_nServId) );
			CSessionAddr dstaddr = pMsg->GetDstAddr();
			pMsg->SetDstAddr( CSessionAddr(addr.GetInAddr(), dstaddr.m_nServId) );

			CSockAddrIn _addr = addr;
			SendMessageTo( sock, pMsg, _addr );
		}
	}catch( CSockException* e ){
		cdump<<"Error:"<<e->GetErrorCode()<<"-"<<e->what()<<endl;
		e->Delete();
	}
	sock_stream.Close();

//	cdump<<"AckPoll... return"<<endl;
}

void CPollProxy::OnAckPoll( CMessageBase* pMsg )
{
//	cdump<<"OnAckPoll..."<<endl;

	CObjLocker<CPollProxy> locker(this);

	IN_ADDR addrHost = pMsg->GetSrcAddr().m_addrHost;
	if( m_CachedMessages.find(addrHost)!=m_CachedMessages.end() ){
		CMessageTrash trash( m_CachedMessages[addrHost] );
		m_CachedMessages[addrHost] = pMsg;
	}else{
		m_CachedMessages.insert( make_pair(addrHost, pMsg) );
		//m_CachedMessages[addrHost] = pMsg;
	}

	//reset the tokens to idle
	int nInfos = pMsg->GetResult();
	cdump<<"get poll back from host:"<<GetHostByAddr(addrHost).c_str()<<" with "<<nInfos<<" tokens"<<endl;

//	cdump<<"OnAckPoll... return"<<endl;
}

void CPollProxy::RunLocalServer()
{
	//setup the polling dgram socket
	CSocket sockpoll;
	sockpoll.Create( NCP_SERVER_PING_PORT, SOCK_DGRAM );

#ifdef _WIN32
	SetUdpBlock( sockpoll );
#endif

	try{
		while( true ){
			fd_set rfds;
			FD_ZERO( &rfds );
			FD_SET( (SOCKET)sockpoll, &rfds );
			int maxfds = (SOCKET)sockpoll + 1;

			int nRet = Select( maxfds, &rfds, NULL, NULL, NULL );

			if( nRet==0 ){	//select timeout, should I do a poll for the remote hosts?
				//time out, do nothing.
			}else if( FD_ISSET((SOCKET)sockpoll, &rfds) ){
				//get message on the poll port, process the incoming message
				SOCKADDR_IN srcaddr;
				CMessageBase* pMsg = RecvMessageFrom<CMessageBase>( sockpoll, srcaddr );
				if( pMsg==NULL )break;

				ASSERT( pMsg->GetMessage()==NCM_POLLSERVER );

				//generate the acking address and ack the poll request.
				int nAckPort = pMsg->GetResult();
				CSockAddrIn addr(srcaddr.sin_addr, nAckPort);
				AckPoll( sockpoll, addr );
			}
		}//end while
	}catch( CSockException* e ){
		cdump<<"Error:"<<e->GetErrorCode()<<"-"<<e->what()<<endl;
		e->Delete();
	}
	sockpoll.Close();
}

void CPollProxy::RunRemoteServer()
{
	//setup the session accepting socket.
	CSocket sockacpt;
	sockacpt.Create( NCP_POLLPROXY_PORT );
	sockacpt.Listen();

	//setup the polling dgram socket
	CSocket sockpoll;
	sockpoll.Create( 0, SOCK_DGRAM );

#ifdef _WIN32
	SetUdpBlock( sockpoll );
#endif

	DoPoll( sockpoll );

	//setup the timers, poll timer triggers polling action, login timer triggers login check
	double tmLastTick = GetMilliTime();
	CSimpleTimer timers;
	int idPollTimer = timers.SetTimer( 20*1000, tmLastTick );

	try{
		while( true ){
			fd_set rfds;
			FD_ZERO( &rfds );
			FD_SET( (SOCKET)sockpoll, &rfds );
			FD_SET( (SOCKET)sockacpt, &rfds );
			int maxfds = max( (SOCKET)sockpoll, (SOCKET)sockacpt ) + 1;

			TIMEVAL tmout, *pto;	//wake up every 10 seconds.
			tmout.tv_sec = 10;
			tmout.tv_usec = 0;
			pto = &tmout;

			int nRet = Select( maxfds, &rfds, NULL, NULL, pto );

			double tmNow = GetMilliTime();
			//do poll when poll timer is time out.
			if( timers.IsTimeOut( idPollTimer, tmNow ) ){
				if( !m_strHostFile.empty() )LoadHostFile( m_strHostFile.c_str() );
				DoPoll( sockpoll );
			}

			if( nRet==0 ){	//select timeout, should I do a poll for the remote hosts?
				//time out, do nothing.
			}else if( FD_ISSET((SOCKET)sockpoll, &rfds) ){
				//get message on the poll port, process the incoming message
				SOCKADDR_IN srcaddr;
				CMessageBase* pMsg = RecvMessageFrom<CMessageBase>( sockpoll, srcaddr );
				if( pMsg==NULL )break;

				//this must be an ACK for POLLSERVER, pollproxy doesn't allow other messages
				ASSERT( pMsg->IsAcking(NCM_POLLSERVER) );
				OnAckPoll( pMsg );

			}else if( FD_ISSET( (SOCKET)sockacpt, &rfds) ){
				//client is connecting the poll socket, accept and send the poll messages back.
				CSocket sockclt;
				sockacpt.Accept( sockclt );
				ForwardMessages( sockclt );
				sockclt.Close();
			}
		}//end while
	}catch( CSockException* e ){
		cdump<<"Error:"<<e->GetErrorCode()<<"-"<<e->what()<<endl;
		e->Delete();
	}
	sockpoll.Close();
	sockacpt.Close();
}

void PrintUsage()
{
	cout<<"Usage 1: runs at the remote side (within firewall) to accept poll requests outside of the firewall"<<endl;
	cout<<"pollproxy -p [nodefile_or_hostname] (hostname should be the ncpserv server name)"<<endl;

	cout<<"Usage 2: runs as local proxy (outside of firewall)"<<endl;
	cout<<"pollproxy -s [hostname] (hostname should be the machine runs ssh tunnel)"<<endl;
}

int main(int argc, char* argv[])
//int _tmain(int argc, _TCHAR* argv[])
{
#ifdef _WIN32
	WSADATA wsadata;
	if( WSAStartup(WSVERS, &wsadata)!=0 ){
		exit(1);
	}
#endif
	bool bAsRemote = false;
	if( argc>=2 ){
		if( strcmp(argv[1], "-p")==0 ){
			bAsRemote = true;
		}else if( strcmp(argv[1], "-s")==0 ){
			bAsRemote = false;
		}
	}else{
		PrintUsage();
		exit(-1);
	}

	if( argc>=3 ){
		pollProxy.m_strHostFile = argv[2];
	}
	pollProxy.LoadHostFile( pollProxy.m_strHostFile.c_str() );

	if( bAsRemote ){
		pollProxy.RunRemoteServer();
	}else{
		pollProxy.RunLocalServer();
	}

#ifdef _WIN32
	WSACleanup();
#endif
	return 0;
}


