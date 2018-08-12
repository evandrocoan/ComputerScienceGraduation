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
            << ", _threads_em_espera=" << &_threads_em_espera << ")" << endl;

    // Disable all interrupts
    begin_atomic();

    if(_value > 0)
    {
        fdec(_value);
        end_atomic();
        return;
    }

    Thread * running_thread = sleep();
    _threads_em_espera.insert(&running_thread->_link);

    if(!Thread::_ready.empty()) {
        Thread * prev = running_thread;

        Thread::_running = Thread::_ready.remove()->object();
        Thread::_running->_state = Thread::RUNNING;

        Thread::dispatch(prev, Thread::_running);
    } else
    {
        db<Semaphore>(WRN) << "Thread::sleep WARNING: The system will be in a deadlock state "
                << "if no hardware interruptions exit the idle state." << endl;

        Thread::idle();
    }

    end_atomic();
}


void Semaphore::v()
{
    // Disable all interrupts
    begin_atomic();

    db<Synchronizer>(TRC) << "Semaphore::v(this=" << this << ",value=" << _value
            << ", _threads_em_espera=" << _threads_em_espera.size() << ")" << endl;

    if(_threads_em_espera.empty())
    {
        finc(_value);
    }
    else
    {
        // put thread on the ready queue
        EPOS::S::U::List_Elements::Doubly_Linked_Ordered<EPOS::S::Thread, unsigned int>* running_thread_link = _threads_em_espera.remove();
        running_thread_link->object()->wake();
    }

    wakeup();
    end_atomic();
}

__END_SYS
