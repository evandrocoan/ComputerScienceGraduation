#ifndef __semaphore_h
#define __semaphore_h

#include "common.h"

#include <semaphore.h>

__BEGIN_SYS
__BEGIN_IMP

class Semaphore: private Synchronizer_Common
{
public:
  Semaphore(int value = 1) { sem_init(&sem, 0, value); }
  ~Semaphore() { sem_destroy(&sem); }
    void p() { sem_wait(&sem); }
  void v() { sem_post(&sem); }
  void lock() { p(); }
  void unlock() { v(); }

private:
  sem_t sem;
};

__END_IMP
__END_SYS

#endif
