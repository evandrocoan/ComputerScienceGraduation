// EPOS Semaphore Abstraction Implementation

#include <semaphore.h>

__BEGIN_SYS

Semaphore::Semaphore(int v): _value(v)
{
    db<Synchronizer>(TRC) << "Semaphore(value=" << _value << ") => " << this << endl;
}


Semaphore::~Semaphore()
{
    db<Synchronizer>(TRC) << "~Semaphore(this=" << this << ")" << endl;
}


void Semaphore::p()
{
    db<Synchronizer>(TRC) << "Semaphore::p(this=" << this << ", value=" << _value
            << ", =" << &_queue << ")" << endl;

    // Disables all interrupts because the scheduler can put another thread to run in the middle of
    // our operation. This is the solution adopted for single core processors. This solution is not
    // suitable for multi-core processors because we need to disable interrupts for all processors,
    // which could degrade performance greatly and can be cumbersome. For multi-core processors,
    // hardware support is required with assembly instructions specifically for synchronization.
    begin_atomic();
    if(fdec(_value) < 1)
        sleep(); // implicit end_atomic()
    else
        end_atomic();
}


void Semaphore::v()
{
    db<Synchronizer>(TRC) << "Semaphore::v(this=" << this << ",value=" << _value << ")" << endl;

    begin_atomic();
    if(finc(_value) < 0)
        wakeup();  // implicit end_atomic()
    else
        end_atomic();
}

__END_SYS
