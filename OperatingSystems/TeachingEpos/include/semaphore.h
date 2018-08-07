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
	Ordered_Queue<Thread> threads_em_espera;
    void p();
    void v();

private:
    volatile int _value;
};


__END_SYS

#endif
