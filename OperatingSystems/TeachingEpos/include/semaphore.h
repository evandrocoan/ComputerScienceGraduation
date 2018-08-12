// EPOS Semaphore Abstraction Declarations

#ifndef __semaphore_h
#define __semaphore_h

#include <synchronizer.h>
#include <thread.h>
#include <utility/queue.h>

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

    void lock()   { p(); }
    void unlock() { v(); }

private:
    volatile int _value;
    Thread::Queue _threads_waiting;

    void free_thread(Thread*);
};


__END_SYS

#endif
