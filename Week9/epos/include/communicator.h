#ifndef __communicator_h
#define __communicator_h

#include <config.h>

__BEGIN_SYS
__BEGIN_INT

class Communicator
{
public:
  Communicator() __DEF;
  Communicator(int value) __DEF;
  Communicator(const Communicator & communicator) __DEF;
  Communicator(const Id & id) __DEF;

  const Id & id() __DEF;
  bool valid() __DEF;

  void send() __DEF;
  void receive() __DEF;
};

class Port
{
public:
  Port() __DEF;
  Port(const Port & port) __DEF;
  Port(const Id & id) __DEF;

  const Id & id() __DEF;
  bool valid() __DEF;

  void send() __DEF;
  void receive() __DEF;
};

__END_INT
__END_SYS

#ifdef __PORT_H
#include __PORT_H
#endif

#endif
