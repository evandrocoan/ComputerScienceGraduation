// EPOS eMote3 Interrupt Controller Initialization

#include <cpu.h>
#include <ic.h>

__BEGIN_SYS

void Cortex_M_IC::init()
{
    db<Init, IC>(TRC) << "IC::init()" << endl;

    CPU::int_disable(); // will be reenabled at Thread::init()
    db<Init, IC>(TRC) << "IC::init:CCR = " << scs(CCR) << endl;
    scs(CCR) |= BASETHR; // BUG
    db<Init, IC>(TRC) << "IC::init:CCR = " << scs(CCR) << endl;

    disable(); // will be enabled on demand as handlers are registered

    // Set all interrupt handlers to int_not()
    for(Interrupt_Id i = 0; i < INTS; i++)
        _int_vector[i] = int_not;
    _int_vector[INT_HARD_FAULT] = hard_fault;
}

__END_SYS
