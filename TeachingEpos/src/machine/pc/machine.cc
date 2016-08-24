// EPOS PC Mediator Implementation

#include <machine/pc/machine.h>
#include <machine/pc/timer.h>
#include <machine/pc/keyboard.h>
#include <machine/pc/nic.h>

__BEGIN_SYS

// Class attributes
volatile unsigned int PC::_n_cpus;

// Class methods
void PC::panic()
{
    CPU::int_disable(); 
    Display::position(24, 73);
    Display::puts("PANIC!");
    if(Traits<System>::reboot)
        Machine::reboot();
    else
        CPU::halt();
}

void PC::reboot()
{
    for(int i = 0; (i < 300) && (i8042::status() & i8042::IN_BUF_FULL); i++)
        i8255::ms_delay(1);

    // Sending 0xfe to the keyboard controller port causes it to pulse
    // the reset line
    i8042::command(i8042::REBOOT);

    for(int i = 0; (i < 300) && (i8042::status() & i8042::IN_BUF_FULL); i++)
        i8255::ms_delay(1);
}

PC::ID PC::id()
{
    NIC nic;
    return ID(reinterpret_cast<const unsigned char *>(&(nic.address()))); 
}

__END_SYS
