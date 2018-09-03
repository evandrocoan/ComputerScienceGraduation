// EPOS System Abstraction Initialization

#include <system.h>
#include <alarm.h>

__BEGIN_SYS

void System::init()
{
    if(Traits<Alarm>::enabled)
        Alarm::init();
}

__END_SYS
