#ifndef __system_atomic_h
#define __system_atomic_h

#include <abstractions/synchronizer/mutex.h>

__BEGIN_SYS

class System_Atomic
{
protected:
  void lock() { mutex.lock(); }
  void unlock() { mutex.unlock(); }

  static void static_lock() { mutex.lock(); }
  static void static_unlock() { mutex.unlock(); }

private:
  static __IMP(Mutex) mutex;
};

__IMP(Mutex) System_Atomic::mutex;

__END_SYS

#endif
