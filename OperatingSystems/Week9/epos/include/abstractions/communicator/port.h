#ifndef __port_h
#define __port_h

#include "common.h"

#include <sys/socket.h>
#include <netinet/in.h>
//#include <sys/types.h>

__BEGIN_SYS
__BEGIN_IMP

class Port: private Communicator_Common
{
public:
  Port() { 
    dbtrc << "Port()\n"; 
    if((sckt = socket(PF_INET, SOCK_DGRAM, IPPROTO_UDP)) < 0)
      return;
    sockaddr_in addr = {AF_INET, INADDR_ANY, 0};
    if(bind(sckt, (sockaddr *)&addr, sizeof(addr)) < 0)
      return;
  }
  ~Port() { dbtrc << "~Port()\n"; }
  void send(int d, const void * b, unsigned int l)
    { dbtrc << "Port::send(" << d << ", " << b << ", " << l << ")\n"; }
  void receive() { dbtrc << "Port::unlock()\n"; }

protected:
  int sckt;
};

__END_IMP
__END_SYS

#endif
