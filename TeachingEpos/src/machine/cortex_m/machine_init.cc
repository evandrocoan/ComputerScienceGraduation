// EPOS Cortex_M Mediator Initialization

#include <machine/cortex_m/machine.h>

__BEGIN_SYS

void Cortex_M::init()
{
    db<Init, Cortex_M>(TRC) << "Cortex_M::init()" << endl;

    Cortex_M_Model::init();

    if(Traits<Cortex_M_IC>::enabled)
        Cortex_M_IC::init();
    if(Traits<Cortex_M_Timer>::enabled)
        Cortex_M_Timer::init();
    if(Traits<Cortex_M_USB>::enabled)
        Cortex_M_USB::init();
#ifndef __no_networking__
    if(Traits<Cortex_M_IEEE802_15_4>::enabled)
        Cortex_M_IEEE802_15_4::init();
#endif
}

__END_SYS
