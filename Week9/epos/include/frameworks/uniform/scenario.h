#ifndef __scenario_h
#define __scenario_h

#include <aspects/debugged.h>

#ifndef __DEBUGGED
__BEGIN_SYS
__INT(Debugged) dbtrc;
__INT(Debugged) dberr;
__INT(Debugged) dbwrn;
__INT(Debugged) dbinf;
__END_SYS
#else
__BEGIN_SYS
__DEBUGGED dbtrc;
__DEBUGGED dberr;
__DEBUGGED dbwrn;
__DEBUGGED dbinf;
__END_SYS
#endif


#include <aspects/id.h>
#include <aspects/atomic.h>
#include <aspects/shared.h>
#include <aspects/protected.h>
#include <aspects/allocated.h>
#include <aspects/timed.h>

__BEGIN_SYS

template<class Imp>
class Scenario: public Id
#ifdef __ATOMIC
, private __ATOMIC
#endif
#ifdef __SHARED
, private __SHARED
#endif
#ifdef __PROTECTED
, private __PROTECTED
#endif
#ifdef __ALLOCATED
, private __ALLOCATED
#endif
#ifdef __TIMED
, private __TIMED
#endif
{
protected:
  static Adapter<Imp> * alloc()
    { return (Adapter<Imp> *)::operator new(sizeof(Adapter<Imp>)); }

  static Adapter<Imp> * share(const Id & id)
    { return alloc(); }

  static Adapter<Imp> * share(Adapter<Imp> * adapter)
    { return adapter; }

  static void free(Adapter<Imp> * obj)
    { delete obj; }

  void enter() {
#ifdef __ATOMIC
    lock();
#endif
  }

  void leave() {
#ifdef __ATOMIC
    unlock();
#endif
  }

  static void static_enter() {
#ifdef __ATOMIC
    static_lock();
#endif
  }

  static void static_leave() {
#ifdef __ATOMIC
    static_unlock();
#endif
  }
};

__END_SYS

#endif
