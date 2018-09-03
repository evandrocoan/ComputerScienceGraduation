// EPOS Thread Abstraction Initialization

#include <system/kmalloc.h>
#include <system.h>
#include <thread.h>
#include <alarm.h>

__BEGIN_SYS

int Thread::init()
{
    db<Thread>(TRC) << "Thread::init()" << endl;
    db<Thread>(TRC) << "Machine::n_cpus()=" << Machine::n_cpus() << endl;

    Thread* _idle;

    for( int cpu_count = Machine::n_cpus(); cpu_count > 0; cpu_count-- )
    {
        _idle = new (kmalloc(sizeof(Thread))) Thread(Thread::Configuration(Thread::READY, Thread::IDLE), &Thread::idle);

        db<Thread>(TRC) << "The idle thread pointer is: " << _idle << endl;
    }

    // The installation of the scheduler timer handler must precede the
    // creation of threads, since the constructor can induce a reschedule
    // and this in turn can call timer->reset()
    // Letting reschedule() happen during thread creation is harmless, since
    // MAIN is created first and dispatch won't replace it nor by itself
    // neither by IDLE (which has a lower priority)
    if(preemptive)
        _timer = new (kmalloc(sizeof(Scheduler_Timer))) Scheduler_Timer(QUANTUM, time_slicer);

    db<Thread>(TRC) << "done Thread::init()! _timer=" << _timer << endl;
    return 0;
}

__END_SYS
