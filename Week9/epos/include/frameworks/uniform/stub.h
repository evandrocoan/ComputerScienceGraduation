#ifndef __stub_h
#define __stub_h

#include "adapter.h"

__BEGIN_SYS

template<class Imp>
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
 
  static Stub * share(const Id & id)
    { return (Stub *)Adapter<Imp>::share(id); }

  static Stub * share(Adapter<Imp> * adapter)
    { return (Stub *)Adapter<Imp>::share(adapter); }
};

// template<class Handled>
// class Stub
// {
//   enum {CREATE = FIRST_METHOD, DESTROY, OPERATION, LAST_METHOD = OPERATION};

// public:
//   Stub()
//     {cout << "Handle()\n";
//     invoke(CREATE);}
//   ~Stub()
//     {cout << "~Handle()\n";
//     invoke(DESTROY);}
//   int operation(int n)
//     {cout << "Handle::operation(" << n << ")\n";
//     parm(n); return invoke(OPERATION);}
// };

__END_SYS

#endif
