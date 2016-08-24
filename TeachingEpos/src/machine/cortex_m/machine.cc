// EPOS Cortex_M Mediator Implementation

#include <machine/cortex_m/machine.h>
#include <display.h>

__BEGIN_SYS

void Cortex_M::panic()
{
    CPU::int_disable();
    if(Traits<Display>::enabled)
        Display::puts("PANIC!\n");
    if(Traits<System>::reboot)
        reboot();
    else
        CPU::halt();
}

void Cortex_M::reboot()
{
    db<Machine>(WRN) << "Machine::reboot()" << endl;
    Cortex_M_Model::reboot();
}

__END_SYS
