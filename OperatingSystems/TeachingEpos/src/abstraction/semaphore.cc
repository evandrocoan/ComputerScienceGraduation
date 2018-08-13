// EPOS Semaphore Abstraction Implementation

#include <semaphore.h>

__BEGIN_SYS

Semaphore::Semaphore(int v): _value(v)
{
    db<Synchronizer>(TRC) << "Semaphore(value=" << _value << ") => " << this << endl;
}


Semaphore::~Semaphore()
{
    begin_atomic();
    db<Synchronizer>(TRC) << "~Semaphore(this=" << this << ")" << endl;
    EPOS::S::U::List_Elements::Doubly_Linked_Ordered<EPOS::S::Thread, unsigned int>* element_link;

    // In case this semaphore is deleted, free all locked elements
    while( _threads_waiting.size() > 0 )
    {
        element_link = _threads_waiting.remove();
        free_thread( element_link->object() );
    }

    end_atomic();
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

    // Allow the thread to be removed from _threads_waiting list in case the user deleted the thread
    running_thread->_locked_list = &_threads_waiting;
    db<Synchronizer>(TRC) << "Semaphore::p() Setting running _locked_list=" << running_thread->_locked_list << endl;

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

    // If there are no more elements on the queue, increment the available counter for the semaphore
    if(_threads_waiting.empty())
    {
        finc(_value);
    }
    else
    {
        // Otherwise, get a thread from the this semaphore waiting list and put it to run
        EPOS::S::U::List_Elements::Doubly_Linked_Ordered<EPOS::S::Thread, unsigned int>* running_thread_link = _threads_waiting.remove();

        // Put thread on the ready queue
        free_thread( running_thread_link->object() );
    }

    end_atomic();
}

void Semaphore::free_thread(Thread * running_thread)
{
    db<Synchronizer>(TRC) << "Semaphore::free_thread(running_thread=" << running_thread << ")" << endl;

    // Put thread on the ready queue
    running_thread->wake();

    // Unset the _locked_list variable, signalizing to the thread destructor it does not need to
    // remove itself form this synchronizer anymore
    running_thread->_locked_list = 0;
    db<Synchronizer>(TRC) << "Semaphore::free_thread() Setting running _locked_list=" << running_thread->_locked_list << endl;
}


__END_SYS
