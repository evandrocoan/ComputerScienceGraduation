// EPOS Thread Abstraction Implementation

#include <system/kmalloc.h>
#include <machine.h>
#include <thread.h>

// This_Thread class attributes
__BEGIN_UTIL
bool This_Thread::_not_booting;
__END_UTIL

__BEGIN_SYS

// Class attributes
volatile unsigned int Thread::_thread_count;
Scheduler_Timer * Thread::_timer;
bool _clear_queue;
bool _delete_me;

Thread* volatile Thread::_running;
Thread::Queue Thread::_deletion_queue;
Thread::Queue Thread::_ready;
Thread::Queue Thread::_suspended;

// Methods
void Thread::constructor_prolog(unsigned int stack_size)
{
    lock();

    _thread_count++;

    _stack = reinterpret_cast<char *>(kmalloc(stack_size));
}


void Thread::constructor_epilog(const Log_Addr & entry, unsigned int stack_size)
{
    db<Thread>(TRC) << "Thread(entry=" << entry
                    << ",state=" << _state
                    << ",priority=" << _link.rank()
                    << ",stack={b=" << reinterpret_cast<void *>(_stack)
                    << ",s=" << stack_size
                    << "},context={b=" << _context
                    << "," << *_context << "}) => " << this << endl;

    switch(_state) {
        case RUNNING: break;
        case READY: _ready.insert(&_link); break;
        case SUSPENDED: _suspended.insert(&_link); break;
        case WAITING: break;
        case FINISHING: break;
    }

    if(preemptive && (_state == READY) && (_link.rank() != IDLE) && (_thread_count > Machine::n_cpus() && _timer))
        reschedule();
    else
        unlock();
}


Thread::~Thread()
{
    lock();

    db<Thread>(TRC) << "~Thread(this=" << this
                    << ",state=" << _state
                    << ",priority=" << _link.rank()
                    << ",stack={b=" << reinterpret_cast<void *>(_stack)
                    << ",context={b=" << _context
                    << "," << *_context << "})" << endl;

    // Precondition: no delete Thread::self()
    assert(running() != this);

    if(_state != FINISHING)
        _thread_count--;

    if(_state == READY)
        _ready.remove(&this->_link);

    if(_state == SUSPENDED)
        _suspended.remove(&this->_link);

    if(_waiting)
        _waiting->remove(this);

    if(_joining)
        _joining->resume();

    unlock();

    kfree(_stack);
}


int Thread::join()
{
    lock();

    db<Thread>(TRC) << "Thread::join(this=" << this << ",state=" << _state << ")" << endl;

    // Precondition: no Thread::self()->join()
    assert(running() != this);

    // Precondition: a single joiner
    assert(!_joining);

    if(_state != FINISHING) {
        _joining = running();
        _joining->suspend();
    } else
        unlock();

    return *reinterpret_cast<int *>(_stack);
}


/**
 * Hands the CPU over to this Thread. This function can be used to implement user-level schedulers.
 * A Thread can be created with a higher priority to act as the scheduler. EPOS scheduler will
 * always elect it, but it can in turn pass() the CPU to another Thread. Accounting is done for the
 * Thread receiving the CPU, but timed scheduling criteria are not reset. In this way, the calling
 * Thread is charged only for the time it took to hand the CPU over to another Thread, which
 * inherits the CPU without further intervention from EPOS’ scheduler. https://epos.lisha.ufsc.br/EPOS+2+User+Guide
 */
void Thread::pass()
{
    lock();

    db<Thread>(TRC) << "Thread::pass(this=" << this << ")" << endl;

    Thread * prev = _running;
    prev->_state = READY;
    _ready.insert(&prev->_link);

    _ready.remove(this);
    _state = RUNNING;
    _running = this;

    dispatch(prev, this);

    unlock();
}


void Thread::suspend()
{
    lock();

    db<Thread>(TRC) << "Thread::suspend(this=" << this << ")" << endl;

    if(_running != this)
        _ready.remove(this);

    _state = SUSPENDED;
    _suspended.insert(&_link);

    if(_running == this) {
        _running = _ready.remove()->object();
        _running->_state = RUNNING;

        dispatch(this, _running);
    }

    unlock();
}


void Thread::resume()
{
    lock();

    db<Thread>(TRC) << "Thread::resume(this=" << this << ")" << endl;

   _suspended.remove(this);
   _state = READY;
   _ready.insert(&_link);

   unlock();
}


// Class methods
void Thread::yield()
{
    lock();

    db<Thread>(TRC) << "Thread::yield(running=" << _running << ")" << endl;

    Thread * prev = _running;
    prev->_state = READY;
    _ready.insert(&prev->_link);

    _running = _ready.remove()->object();
    _running->_state = RUNNING;

    dispatch(prev, _running);

    unlock();
}


void Thread::exit(int status)
{
    lock();

    db<Thread>(TRC) << "Thread::exit(status=" << status << ") [running=" << running() << "]" << endl;

    Thread * prev = _running;
    prev->_state = FINISHING;
    *reinterpret_cast<int *>(prev->_stack) = status;

    _thread_count--;

    if(prev->_joining) {
        prev->_joining->resume();
        prev->_joining = 0;
    }

    lock();

    _running = _ready.remove()->object();
    _running->_state = RUNNING;

    if( prev->_delete_me )
    {
        db<Thread>(TRC) << "Thread::exit(_delete_me=" << prev << ")" << endl;

        // reset the clear queue flag because we just added a new non-exiting element
        _clear_queue = false;
        _deletion_queue.insert(&prev->_link);
    }

    dispatch(prev, _running);

    unlock();
}

void Thread::delete_me()
{
    db<Thread>(TRC) << "Thread::delete_me(this=" << this << ")" << endl;
    this->_delete_me = true;
}

void Thread::sleep(Queue * q)
{
    db<Thread>(TRC) << "Thread::sleep(running=" << running() << ",q=" << q << ")" << endl;

    // lock() must be called before entering this method
    assert(locked());

    Thread * prev = running();
    prev->_state = WAITING;
    prev->_waiting = q;
    q->insert(&prev->_link);

    _running = _ready.remove()->object();
    _running->_state = RUNNING;

    dispatch(prev, _running);

    unlock();
}


void Thread::wakeup(Queue * q)
{
    db<Thread>(TRC) << "Thread::wakeup(running=" << running() << ",q=" << q << ")" << endl;

    // lock() must be called before entering this method
    assert(locked());

    if(!q->empty()) {
        Thread * t = q->remove()->object();
        t->_state = READY;
        t->_waiting = 0;
        _ready.insert(&t->_link);
    }

    unlock();

    if(preemptive)
        reschedule();
}


void Thread::wakeup_all(Queue * q)
{
    db<Thread>(TRC) << "Thread::wakeup_all(running=" << running() << ",q=" << q << ")" << endl;

    // lock() must be called before entering this method
    assert(locked());

    while(!q->empty()) {
        Thread * t = q->remove()->object();
        t->_state = READY;
        t->_waiting = 0;
        _ready.insert(&t->_link);
    }

    unlock();

    if(preemptive)
        reschedule();
}


void Thread::reschedule()
{
    db<Thread>(TRC) << "Thread::reschedule()" << endl;
    yield();
}


void Thread::time_slicer(const IC::Interrupt_Id & i)
{
    db<Thread>(TRC) << "Thread::time_slicer(Interrupt_Id=" << i << ")" << endl;

    // Isso estava sendo chamado antes que a primeira thread do sistema fosse criada/escalonada e
    // causava com que a idle thread iniciasse a execução.
    // Ok, mas precisamos explicar melhor, talvez tenha relação com os comentários do arquivo thread_init.cc
    // if(_initialized)
    reschedule();
}


void Thread::dispatch(Thread * prev, Thread * next)
{
    if(prev != next) {
        if(!_deletion_queue.empty())
        {
            db<Thread>(INF) << "Thread::_deletion_queue=" << &_deletion_queue 
                    << ", size=" << _deletion_queue.size() 
                    << ", _clear_queue=" << _clear_queue 
                    << endl;

            if( _clear_queue )
            {
                Thread * clear;
                _clear_queue = false;

                while( !_deletion_queue.empty() )
                {
                    clear = _deletion_queue.remove()->object();
                    delete clear;
                }
            }
            else
            {
                _clear_queue = true;
            }
        }

        if(prev->_state == RUNNING)
            prev->_state = READY;
        next->_state = RUNNING;

        db<Thread>(TRC) << "Thread::dispatch(prev=" << prev << ",next=" << next << ")" << endl;
        db<Thread>(INF) << "prev={" << prev << ",ctx=" << *prev->_context << "}" << endl;
        db<Thread>(INF) << "next={" << next << ",ctx=" << *next->_context << "}" << endl;

        CPU::switch_context(&prev->_context, next->_context);
    }
    else
        db<Thread>(TRC) << "Thread::dispatch(prev=" << prev << ",next=" << next << ")" << endl;

    unlock();
}


int Thread::idle()
{
    db<Thread>(TRC) << "STARTING THE IDLE THREAD..." << endl;

    while(_thread_count > Machine::n_cpus()) { // someone else besides idle
        if(Traits<Thread>::trace_idle) db<Thread>(TRC) << "Thread::idle(this=" << running() << ")" << endl;

        CPU::int_enable();
        CPU::halt();
    }

    CPU::int_disable();
    db<Thread>(WRN) << "The last thread has exited!" << endl;
    if(reboot) {
        db<Thread>(WRN) << "Rebooting the machine ..." << endl;
        Machine::reboot();
    } else {
        db<Thread>(WRN) << "Halting the machine ..." << endl;
        CPU::halt();
    }

    return 0;
}

__END_SYS

// Id forwarder to the spin lock
__BEGIN_UTIL
unsigned int This_Thread::id()
{
    return _not_booting ? reinterpret_cast<volatile unsigned int>(Thread::self()) : Machine::cpu_id() + 1;
}
__END_UTIL
