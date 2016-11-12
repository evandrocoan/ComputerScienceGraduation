#ifndef __adapter_h
#define __adapter_h

#include "scenario.h"

__BEGIN_SYS

template<class Imp>
class Adapter: public Scenario<Imp>, public Imp
{
public:
  Adapter() { static_leave(); }
  template<class T1>
  Adapter(T1 &a1): Imp(a1) { static_leave(); }
  template<class T1, class T2>
  Adapter(T1 &a1, T2 &a2): Imp(a1, a2) { static_leave(); }
  template<class T1, class T2, class T3>
  Adapter(T1 &a1, T2 &a2, T3 &a3): Imp(a1, a2, a3) { static_leave(); }

  void * operator new(unsigned int size) {
    static_enter();
    dbtrc << "Adapter::new()\n";
    Adapter * obj = Scenario<Imp>::alloc();
    if(!obj) dberr << "Warning: allocation failed!\n";
    return obj;
  }

  static Adapter * get(const Id & id) {
    static_enter();
    dbtrc << "Adapter::get(" << id.unit() << ")\n";
    Adapter * obj = Scenario<Imp>::get(id);
    if(!obj) dberr << "Warning: allocation failed!\n";
    static_leave();
    return obj;
  }

  static Adapter * share(const Id & id) {
    static_enter();
    dbtrc << "Adapter::share(" << id.unit() << ")\n";
    Adapter * obj = Scenario<Imp>::share(id);
    if(!obj) dberr << "Warning: allocation failed!\n";
    static_leave();
    return obj;
  }

  static Adapter * share(Adapter * adapter) {
    static_enter();
    dbtrc << "Adapter::share(" << adapter << ")\n";
    Adapter * obj = Scenario<Imp>::share(adapter);
    if(!obj) dberr << "Warning: allocation failed!\n";
    static_leave();
    return obj;
  }

  static void free(Adapter * adapter) {
    static_enter();
    dbtrc << "Adapter::free(" << adapter << ")\n"; 
    Scenario<Imp>::free(adapter);
    static_leave();
  }

  const Id & id() { return Scenario<Imp>::id(); }
//    { enter(); const Id & id = Scenario<Imp>::id(); leave(); return id; }

//  bool valid() 
//    { enter(); bool valid = Scenario<Imp>::valid(); leave(); return valid; }

  template<class T1>
  void stat(T1 *s) { enter(); Imp::stat(s); leave(); }

  // Thread functions do not enter() and leave() scenarios!
  void suspend() { Imp::suspend(); }
  void resume() { Imp::resume(); }
  int join(int * e) { return Imp::join(e); }
  int pass() { return Imp::pass(); }
  static void exit(int ret) { Imp::exit(ret); }
  static int yield() { return Imp::yield(); }

  // Synchronizer functions do not enter() and leave() scenarios!
  void lock() { Imp::lock(); }
  void unlock() { Imp::unlock(); }
  void p() { Imp::p(); }
  void v() { Imp::v(); }
  void wait() { Imp::wait(); }
  void signal() { Imp::signal(); }
  void broadcast() { Imp::broadcast(); }

  //Communicator
  int send(int r, const void * b, unsigned int l)
    { enter(); Imp::send(r, b, l); leave(); }
  int receive(int * s, void * b, unsigned int * l)
    { enter(); Imp::send(s, b, l); leave(); }
};

__END_SYS

#endif
