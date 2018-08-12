// EPOS Synchronizer Abstractions Common Package

#ifndef __synchronizer_h
#define __synchronizer_h

#include <cpu.h>
#include <thread.h>

__BEGIN_SYS

class Synchronizer_Common
{
protected:
    Synchronizer_Common() {}

    // Atomic operations
    bool tsl(volatile bool & lock) { return CPU::tsl(lock); }
    int finc(volatile int & number) { return CPU::finc(number); }
    int fdec(volatile int & number) { return CPU::fdec(number); }

    // Thread operations
    void begin_atomic() { Thread::lock(); }
    void end_atomic() { Thread::unlock(); }

    /**
     * Put the current curring thread to sleep, by scheduling the next thread ready to run.
     *
     * If there are no threads ready to run, set the CPU to run in a idle state, see: CPU::idle()
     */
    void sleep()
    {
        begin_atomic();
        db<Synchronizer_Common>(TRC) << "Synchronizer_Common::sleep()" << endl;
        Thread * running_thread = Thread::running();
        running_thread->sleep();
    }

    void wakeup() { end_atomic(); }
    void wakeup_all() { end_atomic(); }
};

__END_SYS

#endif

