#ifndef __cooperative_thread_h
#define __cooperative_thread_h

#include "exclusive.h"

__BEGIN_SYS
__BEGIN_IMP

class Cooperative_Thread: public Exclusive_Thread
{
public:
  Cooperative_Thread(int ( * const e)(int),
		     int a = 0,
		     const Thread_State & s = READY)
    : entry(e), argument(a), state(s) {
    if(pthread_create(&thread, 0, &initializer, this))
      thread = 0;
  }

  Cooperative_Thread(const Thread_Self & s) : Exclusive_Thread(s) {}
    
  void stat(Thread_Status * s) {
    Exclusive_Thread::stat(s);
    s->state = state;
  }

  void suspend() {
    dbtrc << "Thread::suspend()\n";
    state = SUSPENDED;
    while(state != READY);
  }

  void resume() {
    dbtrc << "Thread::resume()\n";
    state = READY;
  }

  int join(int *ret) {
    dbtrc << "Thread::join(" << ret << ")\n";
    return pthread_join(thread, (void **)ret); 
  }

  int pass() {
    dbtrc << "Thread::pass()\n";
    dbwrn << "Thread::pass() not implemented!\n";
    return 1;
  }

private:
  static void * initializer(void * ptr) {
    const Cooperative_Thread * thread = (const Cooperative_Thread *) ptr;
    while(thread->state != READY);
    return (void *)thread->entry(thread->argument);
  }

protected:
  volatile Thread_State state;
  int (* const entry)(int);
  int argument;
};

__END_IMP                                                                   
__END_SYS

#endif
