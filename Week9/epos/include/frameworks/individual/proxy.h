#ifndef __proxy_h
#define __proxy_h

#include "message.h"

__BEGIN_SYS

template<class Imp>
class Proxy: public Message
{
public:
    Proxy() { invoke(CREATE); }
    template<class T1>
    Proxy(T1 &p1) { out(p1); invoke(CREATE); }
    template<class T1, class T2>
    Proxy(T1 &p1, T2 &p2) { out(p1, p2); invoke(CREATE); }
    template<class T1, class T2, class T3>
    Proxy(T1 &p1, T2 &p2, T3 &p3) { out(p1, p2, p3); invoke(CREATE); }
    
    static Proxy* share(const Id & id) {
	Proxy* proxy = new Message(id);
	if(proxy->invoke(SHARE))
	    return 0;
	return proxy;
    }
    static Proxy* share(Proxy* p) { 
	Proxy* proxy = new Message(p);
	if(proxy->invoke(SHARE))
	    return 0;
	return proxy;
    }
    static void free(Proxy* p) {
	p->invoke(DESTROY);
	delete p;
    }

    // Thread
    void suspend() { invoke(THREAD_SUSPEND); }
    void resume() { invoke(THREAD_RESUME); }
    int join(int * e) { int r = invoke(THREAD_JOIN); in(e); return r; }
    int pass() { return invoke(THREAD_PASS); }
    static int yield() { return static_invoke(THREAD_YIELD); }
    static void exit(int e) { out(e); static_invoke(THREAD_EXIT); }

    // Synchronizer
    void lock() { invoke(SYNCHRONIZER_LOCK); }
    void unlock() { invoke(SYNCHRONIZER_UNLOCK); }
    void p() { invoke(SYNCHRONIZER_P); }
    void v() { invoke(SYNCHRONIZER_V); }
    void wait() { invoke(SYNCHRONIZER_WAIT); }
    void signal() { invoke(SYNCHRONIZER_SIGNAL); }
    void broadcast() { invoke(SYNCHRONIZER_BROADCAST); }

    //Communicator
    int send(int r, const void * b, unsigned int l) { }
    int receive(int * s, void * b, unsigned int * l) { }

private:
    int invoke(const Method &m)	{
	cout << "invoke(" << m << ")\n"; 
	method(m);
	trap(this);
	return result();
    }
};

__END_SYS

#endif
