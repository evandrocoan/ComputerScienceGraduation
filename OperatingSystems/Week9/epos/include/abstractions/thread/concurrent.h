#ifndef __concurrent_thread_h
#define __concurrent_thread_h

#include "cooperative.h"

#include <sched.h>

__BEGIN_SYS
__BEGIN_IMP

class Concurrent_Thread: public Cooperative_Thread
{
public:
  Concurrent_Thread(int ( * const e)(int),
		    int a = 0,
		    const Thread_State & s = READY)
    : Cooperative_Thread(e, a, s) {}

  Concurrent_Thread(const Thread_Self & s) : Cooperative_Thread(s) {}

  static int yield() {
    dbtrc << "Thread::yield()\n";
    return sched_yield(); 
  }
};

__END_IMP
__END_SYS

#endif
