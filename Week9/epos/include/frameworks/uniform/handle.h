#ifndef __handle_h
#define __handle_h

#include "stub.h"

__BEGIN_SYS

template<class Imp>
class Handle
{
public:
  Handle() { stub = new Stub<Imp>; }
  template<class T1>
  Handle(T1 a1) { stub = new Stub<Imp>(a1); }
  template<class T1, class T2>
  Handle(T1 a1, T2 a2) { stub = new Stub<Imp>(a1, a2); }
  template<class T1, class T2, class T3>
  Handle(T1 a1, T2 a2, T3 a3) { stub = new Stub<Imp>(a1, a2, a3); }
  Handle(const Id & id) { stub = Stub<Imp>::share(id); }
  Handle(const Handle & h) { stub = Stub<Imp>::share(h.stub); }

  ~Handle() { if(stub) Stub<Imp>::free(stub); }
  
  bool valid() const { return (stub && stub->valid()); }
  const Id & id() const { return stub->id(); }
  template<class T1>
  void stat(T1 *s) const { stub->stat(s); }

  // Thread
  void suspend() { stub->suspend(); }
  void resume() { stub->resume(); }
  int join(int * e) { return stub->join(e); }
  int pass() { return stub->pass(); }
  static int yield() { return Stub<Imp>::yield();}
  static void exit(int e = 0) { Stub<Imp>::exit(e);}

  // Synchronizer
  void lock() { stub->lock(); }
  void unlock() { stub->unlock(); }
  void p() { stub->p(); }
  void v() { stub->v(); }
  void wait() { stub->wait(); }
  void signal() { stub->signal(); }
  void broadcast() { stub->broadcast(); }

  //Communicator
  int send(int r, const void * b, unsigned int l) { stub->send(r, b, l); }
  int receive(int * s, void * b, unsigned int * l) { stub->send(s, b, l); }

protected:
  Stub<Imp> * stub;
};

__END_SYS

#endif
