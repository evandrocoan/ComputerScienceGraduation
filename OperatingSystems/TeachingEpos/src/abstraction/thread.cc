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
Scheduler_Timer * Thread::_timer;

Thread* volatile Thread::_running;
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

    // RESPECTIVO A IMPLEMENTAÇÃO ONDE UMA THRAED POR SER JOINADAS POR MUITAS
    // Como agora colocamos im IF, as thraed pode ser acordada e não ocorrerão erros,
    // no entando, o valor de retorno da thread joinada não é mais confiável.
    if(_joined){
        _join.broadcast();
    }
        
    unlock();

    kfree(_stack);
}


int Thread::join()
{
    // Lock e unlock funcionam em sistemas multicore porque não fazem apenas suspender interrupções,
    // também possuem spins locks para oferecer garantias para esse tipo de ambiente.
    lock();

    db<Thread>(TRC) << "Thread::join(this=" << this << ",state=" << _state << ")" << endl;


    // IMPLEMENTAÇÃO COM VARIÁVEL DE CONDIÇÃO - UMA THREAD PODE SER JOINADA POR MÚLTIPLAS THREADS
    // Escolhemos utilizar uma variável de condição para implementar o join pois esse tipo de técnica de
    // sincronização faz exatamente o que precisamos que uma técnica desse tipo faça para os seguintes eventos
    // ocorram após uma thread (joinadora) (necessáriamente a thread em execução) joinar outra thread (joinada).

    // 1 - A thread joinadora verifica se a thread joinada já terminou, se sim então join retorna o valor de
    // retorno (ou saída) da thread joinada.
    // 2 - Se não então, a thread joinadora precisa ser adicionada, nesse caso por si mesma, a uma fila de 
    // espera e ser colocada para dormir, novamente por si mesma, até que a thread joinada termine.

    /* Explicar porque decidimos utilizar if e não while (por causa do delete.)*/
   // if(_state != FINISHING)  
        // Como dito anteriormente, escolhemos por utilizar uma variável de condição para implementar o join.
        // Para que a thread joinadora seja inserida na lita de espera e colocada para dormir ela executa um
        // _join.wait();
        // Agora ela só será acordada e removida da lista de espera quando a thread joinada executar 
        // Thread::exit(), onde será realizado um _join.signal();
        // OBS: Como a thread joinadora chama o método join do objeto da thread joianda, as operações wait() e
        // signal() atuam na mesma variável de condição _join.
        
        // Precisamos avaliar a necessidade de adicionar um mutex, que precisa ser compartilhado por ambas
        // thraed joinadora e joinada e que controlará o acesso a signal e wait. Essa abordagem corretamente
        // mesmo em sistemas multicores, com várias threads rodando simultâneamnte.
        // Só não sei ainda onde ficaria esse mutex, atribute da thread?
        // Talvez não seja necessário pelo modo como essa variável de condição é implementada, mas ainda
        // precisamos avaliar isso, pois não tenho certeza se ela, nesse estado atual do código, funciona.
        
        _joined = true;
        _join.wait();  

    unlock();


    return *reinterpret_cast<int *>(_stack);
}


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

    if((_running == this) && !_ready.empty()) {
        _running = _ready.remove()->object();
        _running->_state = RUNNING;

        dispatch(this, _running);
    } else
        idle(); // implicit unlock()

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

    if(!_ready.empty()) {
        Thread * prev = _running;
        prev->_state = READY;
        _ready.insert(&prev->_link);

        _running = _ready.remove()->object();
        _running->_state = RUNNING;

        dispatch(prev, _running);
    } else
        idle();

    unlock();
}


void Thread::exit(int status)
{
    lock();

    db<Thread>(TRC) << "Thread::exit(status=" << status << ") [running=" << running() << "]" << endl;

    while(_ready.empty() && !_suspended.empty())
        idle(); // implicit unlock();

    lock();

    if(!_ready.empty()) {
        Thread * prev = _running;
        prev->_state = FINISHING;
        *reinterpret_cast<int *>(prev->_stack) = status;
        // Quando uma thread está sendo terminada, ela precisa acordar as threads que a joinaram e
        // estão esperando pelo seu término, para fazer isso ela executa um _join.broadcast(). 
        // Múltiplos joins permitidos para a mesma thread.
        prev->_join.broadcast(); 

        _running = _ready.remove()->object();
        _running->_state = RUNNING;

        dispatch(prev, _running);
    } else {
        db<Thread>(WRN) << "The last thread in the system has exited!" << endl;
        if(reboot) {
            db<Thread>(WRN) << "Rebooting the machine ..." << endl;
            Machine::reboot();
        } else {
            db<Thread>(WRN) << "Halting the CPU ..." << endl;
            CPU::halt();
        }
    }

    unlock();
}

void Thread::sleep(Queue * q)
{
    db<Thread>(TRC) << "Thread::sleep(running=" << running() << ",q=" << q << ")" << endl;

    // lock() must be called before entering this method
    assert(locked());

    while(_ready.empty())
        idle();

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
    yield();
}


void Thread::time_slicer(const IC::Interrupt_Id & i)
{
    reschedule();
}


void Thread::dispatch(Thread * prev, Thread * next)
{
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


int Thread::idle()
{
    db<Thread>(TRC) << "Thread::idle()" << endl;

    db<Thread>(INF) << "There are no runnable threads at the moment!" << endl;
    db<Thread>(INF) << "Halting the CPU ..." << endl;

    CPU::int_enable();
    CPU::halt();

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
