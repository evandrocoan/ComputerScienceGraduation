#ifndef __atomic_h
#define __atomic_h

__BEGIN_SYS
__BEGIN_INT

class Atomic
{
protected:
  void lock() __DEF;
  void unlock() __DEF;
  static void static_lock() __DEF;
  static void static_unlock() __DEF;
};

__END_INT
__END_SYS

#ifdef __SYSTEM_ATOMIC_H
#include __SYSTEM_ATOMIC_H
#endif

#ifdef __CLASS_ATOMIC_H
#include __CLASS_ATOMIC_H
#endif

#ifdef __OBJECT_ATOMIC_H
#include __OBJECT_ATOMIC_H
#endif

#endif
