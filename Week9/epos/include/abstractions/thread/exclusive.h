#ifndef __exclusive_thread_h
#define __exclusive_thread_h

#include "common.h"

#include <pthread.h>
#include <signal.h>

__BEGIN_SYS
__BEGIN_IMP

class Exclusive_Thread: protected Thread_Common
{
protected:
  Exclusive_Thread()
    { dbtrc << "Thread()\n"; }

public:
  Exclusive_Thread(const Thread_Self &)
    { dbtrc << "Thread(SELF)\n"; thread = pthread_self(); }

  ~Exclusive_Thread()
    { dbtrc << "~Thread()\n"; pthread_kill(thread, SIGKILL); }

  void stat(Thread_Status *)
    { dbtrc << "Thread:stat()\n";}
    
  static void exit(int ret)
    { dbtrc << "Thread:exit(" << ret << ")\n"; pthread_exit((void *) ret); }

protected:
  pthread_t thread;
};

__END_IMP
__END_SYS

#endif
