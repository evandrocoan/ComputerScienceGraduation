#ifndef __agent_h
#define __agent_h

#include "traits.h"
#include "message.h"

__BEGIN_SYS

typedef void (Dispatcher)(Message *);

template<class Imp>
class Agent
{
    typedef Stub<Imp, false> _Stub;

public:
    static void create(Message* msg) {
	const _Stub* stub = new _Stub;
	msg->id(
	    stub->id());
    }

    static void share(Message* msg) {
	if(!_Stub::share(msg->id()))
	    msg->result(1);
    }

    static void destroy(Message* msg) { delete _Stub::get(msg->id()); }

    // Thread
    static void suspend(Message* msg) { _Stub::get(msg->id())->suspend(); }
    static void resume(Message* msg) { _Stub::get(msg->id())->resume(); }
    static void join(Message* msg) {
	int e;
	_Stub::get(msg->id())->join(&e);
	out(e);
    }
    static void pass(Message* msg) { _Stub::get(msg->id())->pass(); }
    static void yield(Message* msg) { result(_Stub::yield()); }

    static void exit(Message* msg) {
	int e; 
	in(e);
	_Stub::exit(e);
    }

    // Synchronizer
    static void lock(Message* msg) { _Stub::get(msg->id())->lock(); }
    static void unlock(Message* msg) { _Stub::get(msg->id())->unlock(); }
    static void p(Message* msg) { _Stub::get(msg->id())->p(); }
    static void v(Message* msg) { _Stub::get(msg->id())->v(); }
    static void wait(Message* msg) { _Stub::get(msg->id())->wait(); }
    static void signal(Message* msg) { 	_Stub::get(msg->id())->signal(); }
    static void broadcast(Message* msg) { _Stub::get(msg->id())->broadcast(); }

    //Communicator
};

static Dispatcher* services[LAST_TYPE + 1][10] = {
    { &Agent<Thread>::create,
      &Agent<Thread>::destroy,
      &Agent<Thread>::share,
      &Agent<Thread>::suspend },
    { &Agent<Mutex>::create,
      &Agent<Mutex>::destroy,
      &Agent<Mutex>::share,
      &Agent<Mutex>::lock }
};
    

void trap(Message* msg) {
    if(msg->valid()) {
	Dispatcher* tmp = *services[msg->id().type(), msg->method()];
	tmp(msg);
    }
}

__END_SYS

#endif
