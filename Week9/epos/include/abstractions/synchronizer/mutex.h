#ifndef __mutex_h
#define __mutex_h

#include "common.h"

#include <pthread.h>

__BEGIN_SYS
__BEGIN_IMP

class Mutex: private Synchronizer_Common
{
public:
  Mutex() { dbtrc << "Mutex()\n"; pthread_mutex_init(&mutex, NULL); }
  ~Mutex() { dbtrc << "~Mutex()\n"; pthread_mutex_destroy(&mutex); }
  void lock() { dbtrc << "Mutex::lock()\n"; pthread_mutex_lock(&mutex); }
  void unlock() { dbtrc << "Mutex::unlock()\n"; pthread_mutex_unlock(&mutex); }

protected:
  pthread_mutex_t mutex;
};

__END_IMP
__END_SYS

#endif
