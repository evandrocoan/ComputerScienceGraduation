// EPOS System Abstraction Initialization

#include <system.h>
#include <alarm.h>

__BEGIN_SYS

void System::init()
{
    db<System>(TRC) << "System::init() Alarm::init() and Thread::init()" << endl;

    Alarm::init();
}

__END_SYS
