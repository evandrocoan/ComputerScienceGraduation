// EPOS Spin Lock Utility Declarations

#ifndef __spin_h
#define __spin_h

#include <cpu.h>

__BEGIN_UTIL

// Forwarder to the running thread id
class This_Thread
{
public:
    static unsigned int id();
    static void not_booting() { _not_booting = true; }

private:
    static bool _not_booting; 
};

/**
 * In software engineering, a spinlock is a lock which causes a thread trying to acquire it to
 * simply wait in a loop ("spin") while repeatedly checking if the lock is available. Since the
 * thread remains active but is not performing a useful task, the use of such a lock is a kind of
 * busy waiting. Once acquired, spinlocks will usually be held until they are explicitly released,
 * although in some implementations they may be automatically released if the thread being waited on
 * (that which holds the lock) blocks, or "goes to sleep".
 * 
 * Because they avoid overhead from operating system process rescheduling or context switching,
 * spinlocks are efficient if threads are likely to be blocked for only short periods. For this
 * reason, operating-system kernels often use spinlocks. However, spinlocks become wasteful if held
 * for longer durations, as they may prevent other threads from running and require rescheduling.
 * The longer a thread holds a lock, the greater the risk that the thread will be interrupted by the
 * OS scheduler while holding the lock. If this happens, other threads will be left "spinning"
 * (repeatedly trying to acquire the lock), while the thread holding the lock is not making progress
 * towards releasing it. The result is an indefinite postponement until the thread holding the lock
 * can finish and release it. This is especially true on a single-processor system, where each
 * waiting thread of the same priority is likely to waste its quantum (allocated time where a thread
 * can run) spinning until the thread that holds the lock is finally finished.
 * https://en.wikipedia.org/wiki/Spinlock
 */
// Recursive Spin Lock
class Spin
{
public:
    Spin(): _level(0), _owner(0) {}

    void acquire() {
        int me = This_Thread::id();

        while(CPU::cas(_owner, 0, me) != me);
        _level++;

        db<Spin>(TRC) << "Spin::acquire[SPIN=" << this
        	      << ",ID=" << me
        	      << "]() => {owner=" << _owner 
        	      << ",level=" << _level << "}" << endl;
    }

    void release() {
    	if(--_level <= 0)
            _owner = 0;

        db<Spin>(TRC) << "Spin::release[SPIN=" << this
        	      << "]() => {owner=" << _owner 
        	      << ",level=" << _level << "}" << endl;
    }

private:
    volatile unsigned int _level;
    volatile int _owner;
};

__END_UTIL

#endif
