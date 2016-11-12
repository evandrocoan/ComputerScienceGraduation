#ifndef __synchronizer_h
#define __synchronizer_h

#include <config.h>

__BEGIN_SYS
__BEGIN_INT

class Synchronizer
{
public:
  Synchronizer() __DEF;
  Synchronizer(int value) __DEF;
  Synchronizer(const Synchronizer & synchronizer) __DEF;
  Synchronizer(const Id & id) __DEF;

  const Id & id() __DEF;
  bool valid() __DEF;

  void lock() __DEF;
  void unlock() __DEF;

  void wait() __DEF;
  void signal() __DEF;
  void broadcast() __DEF;

  void p() __DEF;
  void v() __DEF;
};

class Mutex
{
public:
  Mutex() __DEF;
  Mutex(const Mutex & mutex) __DEF;
  Mutex(const Id & id) __DEF;

  const Id & id() __DEF;
  bool valid() __DEF;

  void lock() __DEF;
  void unlock() __DEF;
};

class Condition
{
public:
  Condition() __DEF;
  Condition(const Condition & condition) __DEF;
  Condition(const Id & id) __DEF;

  const Id & id() __DEF;
  bool valid() __DEF;

  void lock() __DEF;
  void unlock() __DEF;
  void wait() __DEF;
  void signal() __DEF;
  void broadcast() __DEF;
};

class Semaphore
{
public:
  Semaphore(int value = 1) __DEF;
  Semaphore(const Semaphore & semaphore) __DEF;
  Semaphore(const Id & id) __DEF;

  const Id & id() __DEF;
  bool valid() __DEF;

  void p() __DEF;
  void v() __DEF;
};

__END_INT
__END_SYS

#ifdef __MUTEX_H
#include __MUTEX_H
#endif

#ifdef __CONDITION_H
#include __CONDITION_H
#endif

#ifdef __SEMAPHORE_H
#include __SEMAPHORE_H
#endif

#endif
