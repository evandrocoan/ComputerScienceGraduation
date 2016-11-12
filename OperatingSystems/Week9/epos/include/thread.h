#ifndef __thread_h
#define __thread_h

#include <config.h>

__BEGIN_SYS

// States of a thread
enum Thread_State {
  RUNNING,
  READY,
  SUSPENDED
};

// Priorities of a thread
enum Thread_Priority {
  THREAD_HIGHEST_PRIORITY = 0,
  THREAD_NORMAL_PRIORITY = 15,
  THREAD_LOWEST_PRIORITY = 31
};


// Status of a thread
struct Thread_Status {
  Thread_State state;
  Thread_Priority priority;
};

// Thread self reference
enum Thread_Self {SELF};

__BEGIN_INT

class Thread
{
public:
  Thread(int (* const entry)(int), int arg = 0,
	 const Thread_State & state = READY) __DEF;
  Thread(const Thread_Self &) __DEF;
  Thread(const Thread & thread) __DEF;
  Thread(const Id & id) __DEF;

  const Id & id() __DEF;
  bool valid() __DEF;

  void stat(Thread_Status * status) __DEF;
  void suspend() __DEF;
  void resume() __DEF;
  int join(int * exit_status) __DEF;
  int pass() __DEF;

  static void exit(int exit_status = 0) __DEF;
  static int yield() __DEF;
};

class Exclusive_Thread
{
public:
  Exclusive_Thread(const Thread_Self &) __DEF;
  Exclusive_Thread(const Thread & Thread) __DEF;
  Exclusive_Thread(const Id & id) __DEF;
    
  const Id & id() __DEF;
  bool valid() __DEF;

  void stat(Thread_Status * status) __DEF;

  static void exit(int exit_status = 0) __DEF;
};

class Cooperative_Thread
{
public:
  Cooperative_Thread(int (* const entry)(int), int arg = 0,
		     const Thread_State & state = READY) __DEF;
  Cooperative_Thread(const Thread_Self &) __DEF;
  Cooperative_Thread(const Thread & thread) __DEF;
  Cooperative_Thread(const Id & id) __DEF;
    
  const Id & id() __DEF;
  bool valid() __DEF;

  void stat(Thread_Status * status) __DEF;
  void suspend() __DEF;
  void resume() __DEF;
  int join(int * exit_status) __DEF;
  int pass() __DEF;

  static void exit(int exit_status = 0) __DEF;
};

class Concurrent_Thread
{
public:
  Concurrent_Thread(int (* const entry)(int), int arg = 0,
	 const Thread_State & state = READY) __DEF;
  Concurrent_Thread(const Thread_Self &) __DEF;
  Concurrent_Thread(const Thread & thread) __DEF;
  Concurrent_Thread(const Id & id) __DEF;
    
  const Id & id() __DEF;
  bool valid() __DEF;

  void stat(Thread_Status * status) __DEF;
  void suspend() __DEF;
  void resume() __DEF;
  int join(int * exit_status) __DEF;
  int pass() __DEF;

  static void exit(int exit_status = 0) __DEF;
  static int yield() __DEF;
};

__END_INT
__END_SYS

#ifdef __EXCLUSIVE_THREAD_H
#include __EXCLUSIVE_THREAD_H
#endif

#ifdef __COOPERATIVE_THREAD_H
#include __COOPERATIVE_THREAD_H
#endif

#ifdef __CONCURRENT_THREAD_H
#include __CONCURRENT_THREAD_H
#endif

#endif
