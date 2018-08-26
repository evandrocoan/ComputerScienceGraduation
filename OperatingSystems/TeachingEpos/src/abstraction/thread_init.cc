// EPOS Thread Abstraction Initialization

#include <system/kmalloc.h>
#include <system.h>
#include <thread.h>
#include <alarm.h>

__BEGIN_SYS

void Thread::init()
{
    db<Thread>(TRC) << "Thread::init()" << endl;
    setup_idle();

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
