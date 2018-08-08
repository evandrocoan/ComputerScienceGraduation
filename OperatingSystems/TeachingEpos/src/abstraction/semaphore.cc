// EPOS Semaphore Abstraction Implementation

#include <semaphore.h>

__BEGIN_SYS

Semaphore::Semaphore(int v): _value(v)
{
    db<Synchronizer>(TRC) << "Semaphore(value=" << this->_value << ") => " << this << endl;
}


Semaphore::~Semaphore()
{
    db<Synchronizer>(TRC) << "~Semaphore(this=" << this << ")" << endl;
}


void Semaphore::p()
{
    db<Synchronizer>(TRC) << "Semaphore::p(this=" << this << ",value=" << this->_value << ")" << endl;

    // Disable all interrupts
    this->begin_atomic();

    if(this->_value > 0)
    {
        fdec(this->_value);
        this->end_atomic();
        return;
    }

    this->threads_em_espera.insert(Thread::_running->_link);
    sleep();
    this->end_atomic();
}


void Semaphore::v()
{
    db<Synchronizer>(TRC) << "Semaphore::v(this=" << this << ",value=" << this->_value << ")" << endl;

    // Disable all interrupts
    this->begin_atomic();

    if(this->threads_em_espera.empty())
    {
        finc(this->_value);
    }
    else
    {
        auto running_thread = this->threads_em_espera.remove();

        // put thread on the ready queue
        Thread::_ready.insert(running_thread->_link);
    }
    this->end_atomic();
}

__END_SYS
