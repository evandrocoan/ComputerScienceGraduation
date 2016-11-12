#ifndef __stub_h
#define __stub_h

#include "adapter.h"
#include "proxy.h"

__BEGIN_SYS

template<class Imp, bool roi>
class Stub: public Adapter<Imp>
{
public:
  Stub() {}
  template<class T1>
  Stub(T1 &a1): Adapter<Imp>(a1) {}
  template<class T1, class T2>
  Stub(T1 &a1, T2 &a2): Adapter<Imp>(a1, a2) {}
  template<class T1, class T2, class T3>
  Stub(T1 &a1, T2 &a2, T3 &a3): Adapter<Imp>(a1, a2, a3) {}
 
  static Stub* share(const Id & id)
    { return reinterpret_cast<Stub*>(Adapter<Imp>::share(id)); }
  static Stub* share(Adapter<Imp>* adapter)
    { return reinterpret_cast<Stub*>(Adapter<Imp>::share(adapter)); }
};

template<class Imp>
class Stub<Imp, true>: public Proxy<Imp>
{
public:
  Stub() {}
  template<class T1>
  Stub(T1 &a1): Proxy<Imp>(a1) {}
  template<class T1, class T2>
  Stub(T1 &a1, T2 &a2): Proxy<Imp>(a1, a2) {}
  template<class T1, class T2, class T3>
  Stub(T1 &a1, T2 &a2, T3 &a3): Proxy<Imp>(a1, a2, a3) {}
 
  static Stub* share(const Id & id)
    { return reinterpret_cast<Stub*>(Proxy<Imp>::share(id)); }
  static Stub* share(Adapter<Imp>* proxy)
    { return reinterpret_cast<Stub*>(Proxy<Imp>::share(proxy)); }
};

__END_SYS

#endif
