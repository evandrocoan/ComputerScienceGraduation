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
    db<Synchronizer>(TRC) << "Semaphore::p(this=" << this << ",value=" << _value << ")" << endl;

    // Disable all interrupts
    begin_atomic();

    if(_value > 0)
    {
        fdec(_value);
        end_atomic();
        return;
    }

    threads_em_espera.insert(&Thread::_running->_link);
    sleep();
    end_atomic();
}


void Semaphore::v()
{
    db<Synchronizer>(TRC) << "Semaphore::v(this=" << this << ",value=" << _value << ")" << endl;

    // Disable all interrupts
    begin_atomic();

    if(threads_em_espera.empty())
    {
        finc(_value);
    }
    else
    {
        EPOS::S::U::List_Elements::Doubly_Linked_Ordered<EPOS::S::Thread, unsigned int>* running_thread_link = threads_em_espera.remove();

        // put thread on the ready queue
        Thread::_ready.insert(running_thread_link);
    }
    end_atomic();
}

__END_SYS
