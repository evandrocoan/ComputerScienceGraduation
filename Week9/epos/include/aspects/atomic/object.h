#ifndef __object_atomic_h
#define __object_atomic_h

#include <abstractions/synchronizer/mutex.h>

__BEGIN_SYS

class Object_Atomic
{
protected:
  void lock() { mutex.lock(); }
  void unlock() { mutex.unlock(); }

  static void static_lock() { static_mutex.lock(); }
  static void static_unlock() { static_mutex.unlock(); }

private:
  __IMP(Mutex) mutex;
  static __IMP(Mutex) static_mutex;
};

__IMP(Mutex) Object_Atomic::static_mutex;

__END_SYS

#endif
