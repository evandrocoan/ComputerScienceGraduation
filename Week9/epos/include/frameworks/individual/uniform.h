#ifndef __uniform_h
#define __uniform_h

// struct Default_Config
// {
// //  static const Type type = ANY_TYPE;
//   static const bool autonomous = false;
//   static const bool allocated = false;
// //  static const Unit units = 0;
//   static void * const table = 0;
//   static const bool shared = false;
//   static const bool checked = false;
//   static const bool atomic = true;
//   static const bool remote = false;
// };

// template <class Imp>
// struct Config: public Default_Config {};

// template <>
// struct Config<__IMP(Exclusive_Thread)>: public Config<void>
// {
//   static const bool atomic = false;
// };

// template <>
// struct Config<__IMP(Cooperative_Thread)>: public Config<void>
// {
//   static const bool atomic = false;
// };

// template <>
// struct Config<__IMP(Concurrent_Thread)>: public Config<void>
// {
//   static const bool atomic = false;
// };

// template <>
// struct Config<__IMP(Mutex)>: public Default_Config
// {
//   static const bool atomic = false;
// };

// template <>
// struct Config<__IMP(Condition)>: public Default_Config
// {
//   static const bool atomic = false;
// };

// template <>
// struct Config<__IMP(Semaphore)>: public Default_Config
// {
//   static const bool atomic = false;
// };

//============================================================================
// ATOMIC							
//============================================================================
#assert CONF_ATOMIC (False)

#if #CONF_ATOMIC (True)
#define __ATOMIC Atomic
#define __ATOMIC_H <aspects/atomic.h>
#if ! #CONF_SYNCHRONIZER (Mutex)
#assert CONF_SYNCHRONIZER (Mutex) 
#endif
#endif

//============================================================================
// SHARED							
//============================================================================
#assert CONF_SHARED (False)

#if #CONF_SHARED (True)
#define __SHARED Shared
#define __SHARED_H <aspects/shared.h>
#endif

#endif
