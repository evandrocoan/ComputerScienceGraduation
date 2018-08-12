// EPOS Thread Abstraction Declarations

#ifndef __thread_h
#define __thread_h

#include <utility/queue.h>
#include <cpu.h>
#include <machine.h>
#include <system/kmalloc.h>

extern "C" { void __exit(); }

__BEGIN_SYS

class Thread
{
    friend class Init_First;
    friend class System;
    friend class Semaphore;
    friend class Synchronizer_Common;
    friend class Alarm;
    friend class IA32;

protected:
    static const bool preemptive = Traits<Thread>::preemptive;
    static const bool reboot = Traits<System>::reboot;

    static const unsigned int QUANTUM = Traits<Thread>::QUANTUM;
    static const unsigned int STACK_SIZE = Traits<Application>::STACK_SIZE;

    typedef CPU::Log_Addr Log_Addr;
    typedef CPU::Context Context;

public:
    // Thread State
    enum State {
        RUNNING,
        READY,
        SUSPENDED,
        WAITING,
        FINISHING
    };

    // Thread Priority
    typedef unsigned int Priority;
    enum {
        HIGH = 0,
        NORMAL = 15,
        LOW = 31
    };

    // Thread Configuration
    struct Configuration {
        Configuration(const State & s = READY, const Priority & p = NORMAL, unsigned int ss = STACK_SIZE)
        : state(s), priority(p), stack_size(ss) {}

        State state;
        Priority priority;
        unsigned int stack_size;
    };

    // Thread Queue
    typedef Ordered_Queue<Thread, Priority> Queue;

public:
    template<typename ... Tn>
    Thread(int (* entry)(Tn ...), Tn ... an);
    template<typename ... Tn>
    Thread(const Configuration & conf, int (* entry)(Tn ...), Tn ... an);
    ~Thread();

    const volatile State & state() const { return _state; }

    const volatile Priority & priority() const { return _link.rank(); }
    void priority(const Priority & p);

    int join();
    void pass();

    /**
     * Stops/start the current thread from/to running. This must to be called in a atomic block.
     */
    void sleep();
    void wake();

    void suspend();
    void resume();

    static Thread * volatile self() { return running(); }

    /**
     * In computer science, yield is an action that occurs in a computer program during
     * multithreading, of forcing a processor to relinquish control of the current running thread,
     * and sending it to the end of the running queue, of the same scheduling priority.
     * https://en.wikipedia.org/wiki/Yield_(multithreading)
     */
    static void yield();
    static void exit(int status = 0);

protected:
    void constructor_prolog(unsigned int stack_size);
    void constructor_epilog(const Log_Addr & entry, unsigned int stack_size);

    static Thread * volatile running() { return _running; }

    static void lock() { CPU::int_disable(); }
    static void unlock() { CPU::int_enable(); }
    static bool locked() { return CPU::int_disabled(); }

    static void reschedule();
    static void time_slicer(const IC::Interrupt_Id & interrupt);

    /**
     * Change the current CPU thread context.
     *
     * Dynamic: a Dynamic Criterion is recalculated at run-time to constantly reflect the police in
     * force. There are two moments at which a Dynamic Criterion can be recalculated: at `dispatch`
     * and at release. For Aperiodic Threads, for which no period is defined, it is done when the
     * Thread leaves the CPU (i.e. another Thread is `dispatched`). For Periodic Threads,
     * recalculating at `dispatch` would not be adequate, since jobs of other Threads will still be
     * released before the next activation and they may influence on the calculations. Therefore,
     * Periodic Threads subjected to Dynamic Criteria are reevaluated before the release of each
     * job. Earliest Deadline First is Dynamic Criterion. https://epos.lisha.ufsc.br/EPOS+2+User+Guide
     *
     * @param `prev` the thread currently running
     * @param `next` the thread which will be running
     */
    static void dispatch(Thread * prev, Thread * next);

    /**
     * Call the scheduler to remove the current thread from running, without adding it to the _ready
     * threads queue.
     */
    static void dispatch_hidden();

    /**
     * Halts the CPU.
     *
     * @return what?
     */
    static int idle();

    /**
     * Called when you kill your system somehow.
     */
    static void death()
    {
        unlock();

        while( true )
        {
            db<Thread>(ERR) << "Thread::yield(running=" << _running << "); ";
            db<Thread>(ERR) << "ERROR: You killed your system as the only running thread is going to sleep indefinitely!" << endl;
        }
    }

private:
    static void init();

protected:
    char * _stack;
    Context * volatile _context;
    volatile State _state;
    Queue::Element _link;

    static Scheduler_Timer * _timer;

private:
// public:
    static Thread * volatile _running;
    static Queue _ready;
    static Queue _suspended;
};


template<typename ... Tn>
inline Thread::Thread(int (* entry)(Tn ...), Tn ... an)
: _state(READY), _link(this, NORMAL)
{
    constructor_prolog(STACK_SIZE);
    _context = CPU::init_stack(_stack + STACK_SIZE, &__exit, entry, an ...);
    constructor_epilog(entry, STACK_SIZE);
}

template<typename ... Tn>
inline Thread::Thread(const Configuration & conf, int (* entry)(Tn ...), Tn ... an)
: _state(conf.state), _link(this, conf.priority)
{
    constructor_prolog(conf.stack_size);
    _context = CPU::init_stack(_stack + conf.stack_size, &__exit, entry, an ...);
    constructor_epilog(entry, conf.stack_size);
}

__END_SYS

#endif
