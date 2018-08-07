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
    reg(IER, 0);

    if(_value > 0)
    {
        fdec(_value);
        reg(IER, 1);
        return;
    }

    this->threads_em_espera.insert(Thread::_running);
    sleep();
    reg(IER, 1);
}


void Semaphore::v()
{
    db<Synchronizer>(TRC) << "Semaphore::v(this=" << this << ",value=" << _value << ")" << endl;

    // Disable all interrupts
    reg(IER, 0);

    if(this->queue.empty())
    {
        finc(_value);
    }
    else
    {
        auto running_thread = this->threads_em_espera.remove();
        wakeup();
    }
}

__END_SYS
