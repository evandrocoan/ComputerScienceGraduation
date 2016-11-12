#ifndef __condition_h
#define __condition_h

#include "mutex.h"

#include <pthread.h>

__BEGIN_SYS
__BEGIN_IMP

class Condition: public Mutex
{
public:
  Condition()
    { dbtrc << "Condition()\n"; pthread_cond_init(&cond, NULL); }
  ~Condition()
    { dbtrc << "~Condition()\n"; pthread_cond_destroy(&cond); }
  void signal()
    { dbtrc << "Condition::signal()\n"; pthread_cond_signal(&cond); }
  void broadcast()
    { dbtrc << "Condition::broadcast()\n"; pthread_cond_broadcast(&cond); }
  void wait()
    { dbtrc << "Condition()::wait\n"; pthread_cond_wait(&cond, &mutex); }

private:
  pthread_cond_t cond;
};

__END_IMP
__END_SYS

#endif
