// EPOS Thread Abstraction Declarations

#ifndef __thread_h
#define __thread_h

#include <utility/queue.h>
#include <cpu.h>
#include <machine.h>
#include <system/kmalloc.h>

extern "C" { void __exit(); }

// Protótipo de ponteiro para resolver o problema da dependência circular entre:
//   Thread -> Synchronizer_Common -> Condition
//   Condition <- Synchronizer_Common <- Thread
//   
// A dependência circular acontece por que Condition deriva da interface Synchronizer_Common que
// inclui este header `thread.h`. Isso é um problema por que em C++, para se poder instanciar um
// objecto, i.e., chamar seu construtor, é necessário conhecer a implementação completa da classe.
// Entretanto, no caso em a classe A inclui a classe B e a classe B inclui a classe A, temos um
// impasse por que quando a classe A for incluir a classe B, ela não conseguirá por que a definição
// da classe A ainda não está completa e a classe B precisa dela completa. Assim a classe A não
// consegue incluir a classe B.
// 
// A solução para esse problema em C++ é fazer com que a classe B não precise da definição completa
// da classe A. Isso é possível declarando no header da classe B, o protótipo da classe A e não
// inicializando a classe A no header da classe B, mas sim em seu .cpp, e então utiliza-se classe A
// como um ponteiro e não como um objeto na stack.
// 
// class A
// {
//     B b; // aqui, a definição da classe B não é conhecida
//     A() b() {}
// };
// 
// class B
// {
//     A a;
//     B() a() {}
// };

class Condition;

__BEGIN_SYS

class Thread
{
    friend class Init_First;
    friend class System;
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
    // Colocar o FINISHING dessa enumeração como o primeiro estado evita lock infinito no caso de
    // alguém chamar join() após excluir essa thread. 
    // 
    // Porque depois de chamar delete, o valor de _state será 0, por que o o destrutor da
    // classe thread zero todos os dados na memória que pertencem a thread (?). Isso é útil para
    // proteger dados confidenciais como chaves criptográfica que podem estar alocadas no espaço
    // de endereçamento da thread. 
    // 
    // Então, como _state sempre será 0, adiciona-se o FINISHING como o primeiro elemento nesta
    // enumeração, fazendo com que a condição de junção (_state! = FINISHING) falhe e o join não
    // bloqueie a execução da thread.
    // 
    // Havia um ciclo infinito porque a implementação de join() era apenas uma chamada yield(), que
    // será executada para sempre, já que o thread é excluída e não irá mudar ou fazer mais nada.
    // 
    // Entretanto, colocar o valor de FINISHING no início da enumeração não é um solução definitiva
    // por que caso a memória volte a ser alocada por um outro processo, estariamos lendo um valor
    // provavelmente diferente de zero. Assim, o problema original voltaria a ocorrer.
    // 
    // Uma solução definitiva para impedir esse tipo de problema, seria lançar uma exceção quando o
    // um aplicativo tentar acessar um endereço de memória através de um ponteiro que foi deletado.
    enum State {
        FINISHING,
        RUNNING,
        READY,
        SUSPENDED,
        WAITING
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

    static void sleep(Queue * q);
    static void wakeup(Queue * q);
    static void wakeup_all(Queue * q);

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
     * Halts the CPU.
     *
     * @return what?
     */
    static int idle();

    /**
     * Called when you kill your system somehow. ???
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
    static void add_to_ready(Thread*);
    static void add_to_suspended(Thread*);
    static void wakeup_joined_threads(Thread*);

protected:
    char * _stack;
    Context * volatile _context;
    volatile State _state;

    /**
     * When this thread is locked by some synchronizer, this variable is set pointing to it's
     * synchronizer list, allowing the thread destructor to remove itself from the synchronizer list.
     *
     * This works because a thread can only be blocked by one synchronizer at time, as if the thread
     * is blocked, there is no way it can call another synchronizer to block it again.
     */
    Queue * _waiting;

    bool _joined;  // Boolean que indica se essa thread está sendo joinada por outra(s).
    Condition* _join; // Variável de condição utilizada para realizar joins. // Problemas de dependência circular
    
    Queue::Element _link;
    static Scheduler_Timer * _timer;

private:
// public:
    static Thread * volatile _running;
    static Queue _ready;
    static Queue _suspended;
};

/**
 * Colocamos _joined e _join entre _waiting e _link por que essa é a ordem na qual eles estão
 * declarados na classe, e C++ solta um warning: 
 *     warning: 'EPOS::S::Thread::_link' will be initialized after
 * 
 * > Having you member initialization list in some other order may be confusing to the programmer
 * who might not be aware of which order is followed, or might not be aware that the members were
 * declared in a different order and therefore might expect the order of member initialization to be
 * the order of member initialization list - which it isn't in your case. The purpose of the warning
 * is to highlight this fact. This fact may be very important in cases where initialization of one
 * member depends on another. https://stackoverflow.com/questions/47464164/will-be-initialized-after-warning-fix
 */
template<typename ... Tn>
inline Thread::Thread(int (* entry)(Tn ...), Tn ... an)
: _state(READY), _waiting(0), _joined(0), _link(this, NORMAL)
{
    constructor_prolog(STACK_SIZE);
    _context = CPU::init_stack(_stack + STACK_SIZE, &__exit, entry, an ...);
    constructor_epilog(entry, STACK_SIZE);
}

template<typename ... Tn>
inline Thread::Thread(const Configuration & conf, int (* entry)(Tn ...), Tn ... an)
: _state(conf.state), _waiting(0), _joined(0), _link(this, conf.priority)
{
    constructor_prolog(conf.stack_size);
    _context = CPU::init_stack(_stack + conf.stack_size, &__exit, entry, an ...);
    constructor_epilog(entry, conf.stack_size);
}

__END_SYS

#endif
