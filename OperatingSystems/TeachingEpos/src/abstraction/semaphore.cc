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
            << ", _threads_waiting=" << &_threads_waiting << ")" << endl;

    // Disables all interrupts because the scheduler can put another thread to run in the middle of
    // our operation. This is the solution adopted for single core processors. This solution is not
    // suitable for multi-core processors because we need to disable interrupts for all processors,
    // which could degrade performance greatly and can be cumbersome. For multi-core processors,
    // hardware support is required with assembly instructions specifically for synchronization.
    begin_atomic();

    // If the semaphore still has free resources, decrement the counter, re-enable the hardware
    // interrupts and let the thread continue running.
    if(_value > 0)
    {
        fdec(_value);
        end_atomic();
        return;
    }

    // Otherwise, set the running thread state as sleeping
    sleep();

    Thread * running_thread = Thread::running();

    // Add the current running thread to the list of threads waiting for the resource to be released
    _threads_waiting.insert(&running_thread->_link);

    // Call the scheduler to remove the current thread from running, without adding it to the
    // _ready threads queue. Otherwise, the thread will keep running when it must not.
    Thread::dispatch_hidden(); // implicit unlock

    // Re-enable interruptions, allowing the scheduler to continue working
    end_atomic();
}


void Semaphore::v()
{
    // Disable all interrupts
    begin_atomic();

    db<Synchronizer>(TRC) << "Semaphore::v(this=" << this << ",value=" << _value
            << ", _threads_waiting=" << _threads_waiting.size() << ")" << endl;

    // If there are no more queues on hold, increment the available counter for the semaphore
    if(_threads_waiting.empty())
    {
        finc(_value);
    }
    else
    {
        // Otherwise, get a thread from the this semaphore waiting list and put it to run
        EPOS::S::U::List_Elements::Doubly_Linked_Ordered<EPOS::S::Thread, unsigned int>* running_thread_link = _threads_waiting.remove();

        // put thread on the ready queue
        running_thread_link->object()->wake();
    }

    end_atomic();
}

__END_SYS
