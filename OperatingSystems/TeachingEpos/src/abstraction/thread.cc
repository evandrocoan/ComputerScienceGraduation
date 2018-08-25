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
bool Thread::_initialized;
Scheduler_Timer * Thread::_timer;

Thread* volatile Thread::_running;
Thread* volatile Thread::_idle;
Thread::Queue Thread::_ready;
Thread::Queue Thread::_suspended;

// Methods
void Thread::constructor_prolog(unsigned int stack_size)
{
    lock();

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
        case SUSPENDED: _suspended.insert(&_link); break;
        default: _ready.insert(&_link);
    }

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

    _ready.remove(this);
    _suspended.remove(this);

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

    // Corrigido o problema da idle thread nunca executar por que a running thread era inserido na
    // fila de _ready antes de se retirar a nova thread, causando com que a thread atual sempre
    // fosse reescalonada por que ela sempre terá uma prioridade maior do que a idle thread.
    Thread * prev = _running;
    _ready.remove(this);

    _state = RUNNING;
    prev->_state = READY;

    _ready.insert(&prev->_link);
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

    // Corrigido o problema da idle thread nunca executar por que a running thread era inserido na
    // fila de _ready antes de se retirar a nova thread, causando com que a thread atual sempre
    // fosse reescalonada por que ela sempre terá uma prioridade maior do que a idle thread.
    Thread * prev = _running;
    _running = _ready.remove()->object();

    prev->_state = READY;
    _running->_state = RUNNING;

    _ready.insert(&prev->_link);
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

    if(prev->_joining) {
        prev->_joining->resume();
        prev->_joining = 0;
    }

    lock();

    // _ready.size() == 1 somente quando a única thread existente é idle
    // futuramente fazer _ready.size() == CPUS_COUNT por que serão criados uma thread para cada CPU
    if(_ready.size() < 2 && _suspended.empty() && _initialized) {
        db<Thread>(WRN) << "The last thread in the system has exited!\n";

        // Thread::kill_idle_thread();
        // kill_idle_thread() chama ~Thread que no final reativa as interrupções
        // lock();

        if(reboot) {
            db<Thread>(WRN) << "Rebooting the machine ...\n";
            Machine::reboot();
        } else {
            db<Thread>(WRN) << "Halting the CPU ...\n";
            CPU::halt();
        }
    } else {
        _running = _ready.remove()->object();
        _running->_state = RUNNING;

        dispatch(prev, _running);
    }

    unlock();
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
    if(_initialized)
    {
        reschedule();
    }
}


void Thread::dispatch(Thread * prev, Thread * next)
{
    db<Thread>(TRC) << "Thread::dispatch(prev=" << prev << ",next=" << next << ")" << endl;

    if(prev != next) {
        if(prev->_state == RUNNING)
            prev->_state = READY;
        next->_state = RUNNING;

        db<Thread>(TRC) << "Thread::dispatch(prev=" << prev << ",next=" << next << ")" << endl;
        db<Thread>(INF) << "prev={" << prev << ",ctx=" << *prev->_context << "}" << endl;
        db<Thread>(INF) << "next={" << next << ",ctx=" << *next->_context << "}" << endl;

        CPU::switch_context(&prev->_context, next->_context);
    }

    unlock();
}


int idle_function() 
{
    db<Thread>(TRC) << "STARTING THE IDLE THREAD..." << endl;

    while(true)
    {
        db<Thread>(TRC) << "IDLE_FUNCTION()" << endl;
        db<Thread>(INF) << "THERE ARE NO RUNNABLE THREADS AT THE MOMENT!" << endl;
        db<Thread>(INF) << "HALTING THE CPU ..." << endl;

        CPU::int_enable();
        CPU::halt();
    }

    return 0;
}


void Thread::setup_idle_thread()
{
    db<Thread>(TRC) << "Starting the Thread::setup_idle_thread()" << endl;
    if( _idle ) return;

    // Initializa a idle thread com estado running para que ela não seja colocada em nenhuma outra
    // lista de threads pelo construtor.
    _idle = new (kmalloc(sizeof(Thread))) Thread(Thread::Configuration(Thread::RUNNING, Thread::IDLE), &idle_function);
    db<Thread>(TRC) << "The idle thread pointer is: " << _idle << endl;

    // Se descomentar essa linha, a thread principal não executa
    _idle->_state = Thread::READY;
    _ready.insert(&_idle->_link);
}


void Thread::kill_idle_thread() 
{
    db<Thread>(TRC) << "Starting the Thread::kill_idle_thread()" << endl;

    if( _idle )
    {
        delete _idle;
        _idle = 0;
    }
}


__END_SYS

// Id forwarder to the spin lock
__BEGIN_UTIL
unsigned int This_Thread::id()
{
    return _not_booting ? reinterpret_cast<volatile unsigned int>(Thread::self()) : Machine::cpu_id() + 1;
}
__END_UTIL
