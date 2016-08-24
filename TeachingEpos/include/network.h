// EPOS Network Abstraction Declarations

#ifndef __network_h
#define __network_h

#include <nic.h>

__BEGIN_SYS

class Network
{
protected:
    Network() {}

public:
    static void init();
};

__END_SYS

#endif
