// EPOS Semaphore Abstraction Declarations

#ifndef __semaphore_h
#define __semaphore_h

#include <utility/handler.h>
#include <synchronizer.h>

__BEGIN_SYS

class Semaphore: protected Synchronizer_Common
{
public:
    Semaphore(int v = 1);
    ~Semaphore();

    /**
     * It is critical that semaphore operations be executed atomically. We must guarantee that no two
     * processes can execute wait() and signal() operations on the same semaphore at the same time. This
     * is a critical-section problem; and in a single-processor environment, we can solve it by simply
     * inhibiting interrupts during the time the wait() and signal() operations are executing. This
     * scheme works in a single-processor environment because, once interrupts are inhibited,
     * instructions from different processes cannot be interleaved. Only the currently running process
     * executes until interrupts are reenabled and the scheduler can regain control.
     * > Silberschatz, Operating System Concepts 9th Edition
     */
    void p();
    void v();

    /**
     * Permite trocar um mutex por semáforo, somente mudando a instanciação de `Mutex table` para
     * `Semaphore table`, Assim, ficou fácil trocar entre um semáforo e o mutex.
     */
    void lock()   { p(); }
    void unlock() { v(); }

private:
    volatile int _value;
};


// An event handler that triggers a semaphore (see handler.h)
class Semaphore_Handler: public Handler
{
public:
    Semaphore_Handler(Semaphore * h) : _handler(h) {}
    ~Semaphore_Handler() {}

    void operator()() { _handler->v(); }

private:
    Semaphore * _handler;
};

__END_SYS

#endif
