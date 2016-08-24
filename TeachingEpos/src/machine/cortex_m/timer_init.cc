// EPOS Cortex_M Timer Mediator Initialization

#include <timer.h>
#include <ic.h>

__BEGIN_SYS

void Cortex_M_Timer::init()
{
    db<Init, Timer>(TRC) << "Timer::init()" << endl;

    Engine::init(FREQUENCY);
    IC::int_vector(IC::INT_TIMER, int_handler);
    Engine::enable();
}

__END_SYS
