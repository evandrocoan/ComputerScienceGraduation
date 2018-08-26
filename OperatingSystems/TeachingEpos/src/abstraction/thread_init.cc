// EPOS Thread Abstraction Initialization

#include <system/kmalloc.h>
#include <system.h>
#include <thread.h>
#include <alarm.h>

__BEGIN_SYS

void Thread::init()
{
    db<Thread>(TRC) << "Thread::init()" << endl;

    db<Thread>(TRC) << "Starting the Thread::setup_idle()" << endl;
    Thread* _idle;

    for( int cpu_count = Machine::n_cpus(); cpu_count > 0; cpu_count-- )
    {
        _idle = new (kmalloc(sizeof(Thread))) Thread(Configuration(READY, IDLE), &Thread::idle);

        db<Thread>(TRC) << "The idle thread pointer is: " << _idle << endl;
    }

    // The installation of the scheduler timer handler must precede the
    // creation of threads, since the constructor can induce a reschedule
    // and this in turn can call timer->reset()
    // Letting reschedule() happen during thread creation is harmless, since
    // MAIN is created first and dispatch won't replace it nor by itself
    // neither by IDLE (which has a lower priority)
    if(Thread::preemptive)
    {
        _timer = new (kmalloc(sizeof(Scheduler_Timer))) Scheduler_Timer(QUANTUM, time_slicer);
        _timer->disable();
    }
}

__END_SYS
