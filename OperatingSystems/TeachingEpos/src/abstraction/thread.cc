// EPOS Thread Abstraction Implementation

#include <system/kmalloc.h>
#include <machine.h>
#include <thread.h>
#include <utility/malloc.h>
#include <condition.h>

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
    // Como agora colocamos im IF, as threads podem ser acordadas e não ocorrerão erros,
    // no entanto, o valor de retorno da thread joinada não é mais confiável, por que
    // depois de deletar um object em C++, não se pode tentar acessar seus atributos ou
    // métodos, por que eles já foram deletados :/
    // 
    // Quando deletamos uma thread, temos que liberar todas as threads que estão esperando por
    // ela terminar, por que agora que deletamos ela, ela nunca mais vai terminar.
    if(_joined){
        _join->broadcast();

        // Libera a memória alocada para pela variável de condição depois que todas as threads em
        // espera são acordadas.
        // 
        // Não encontrei nenhum outro lugar do EPOS fazendo a deleção dos objectos criados pelo
        // operador new replacement. Por isso não estou chamando ele aqui.
        // 
        // Entretanto, como agora o objeto não é mais alocada na piscina de memória do EPOS, e sim
        // na heap, temos que fazer a deleção de seu ponteiro.
        delete _join;
    }

    unlock();

    kfree(_stack);
}


int Thread::join()
{
    // Lock e unlock funcionam em sistemas multicore porque não fazem apenas suspender interrupções,
    // também possuem spins locks para oferecer garantias de atomicidade ambientes desse tipo.
    lock();

    db<Thread>(TRC) << "Thread::join(this=" << this << ",state=" << _state << ")" << endl;

    // IMPLEMENTAÇÃO COM VARIÁVEL DE CONDIÇÃO - UMA THREAD PODE SER JOINADA POR MÚLTIPLAS THREADS
    // Escolhemos utilizar uma variável de condição para implementar o join pois esse tipo de
    // técnica de sincronização faz exatamente o que precisamos que uma técnica desse tipo faça para
    // os seguintes eventos ocorram após uma thread (joinadora) (necessariamente a thread em
    // execução) joinar outra thread (joinada). Essa implementação é a sugerida pelo seguinte livro:
    // ???????????????????????????
    // 
    // 1 - A thread joinadora verifica se a thread joinada já terminou, se sim então join retorna o
    // valor de retorno (ou saída) da thread joinada.
    // 
    // 2 - Se não então, a thread joinadora precisa ser adicionada, nesse caso por si mesma, a uma
    // fila de espera e ser colocada para dormir, novamente por si mesma, até que a thread joinada
    // termine.
    // 
    // Utilizamos um if e não while como no programa original por que agora utilizamos a variável de
    // condição que faz o bloqueio da execução da thread até que ela termine, assim, fica
    // desnecessário utilizar um while(_state != FINISHING), pois ele sempre irá falhar na segunda
    // tentativa quando thread terminar, já que seu estado será trocado para FINISHING.
    // 
    // Antes de bloquear a execução com a variável de condição, verificamos se a thread atual já não
    // finalizou sua execução ou se foi deletada. A verificação de se ela foi deletado é feita
    // implicitamente por que quando isso acontece, o valor de seu estado é setado para 0 que é
    // igual ao valor de FINISHING, assim sabemos sempre quando ela foi deletada.
    // 
    // Quando um thread finaliza sua execução, possui seu valor de _state setado para zero. Mas este
    // valor também é zerado quando a thread é deletada com operador delete, mesmo que ela não tenha
    // sido finaliza ainda, indo para estado de FINISHING. Isso acontece por que o destrutor da
    // classe thread zera todos os dados no espaço de endereçamento da thread, para proteger suas
    // informações, o que causo o valor do estado em _state virar 0, igual a valor da enumeração
    // FINISHING
    // 
    // Utilizamos `Thread::running() != this` para impedir que uma thread dê join em si mesma, i.e.,
    // this->join(). Por que se ela fizer isso, o programa iria parar para sempre já que ela sairia
    // de dentro das filas do sistema operacional e entraria dentro da sua si na fila da variável de
    // condição. E uma vez lá dentro ela sairá jamais e seria somente um pedaço de memória perdida,
    // i.e., leaked memory.
    if(_state != FINISHING)
    {
        if( Thread::running() == this )
        {
            db<Thread>(ERR) << "ERROR: Thread trying to join itself, join(this=" << this << ")" << endl;
        }
        else
        {
            // Lazy initialization, somente inicializa a variável _join quando alguém for dar join
            // Assim, salvamos memória, caso nunca ninguém de join(), por exemplo, em um sistema que
            // a thread principal somente inicializa alguns serviços contínuos que nunca terminam.
            if( !_joined )
            {
                // Colocamos a construção do _join aqui em vez de na initialization list no header thread.h
                // por causa do erro: 
                //      forward declaration of 'struct EPOS::S::Condition'
                //      https://stackoverflow.com/questions/9840109/error-forward-declaration-of-struct
                // 
                // Não podemos inicializar a classe Condition() no arquivo thread.h por que lá, somente
                // incluímos o protótipo da classe Condition() e não a sua declaração completa, devido a
                // dependência cíclica: 
                //   Thread -> Synchronizer_Common -> Condition
                //   Condition <- Synchronizer_Common <- Thread
                //  
                // Tentamos fazer ajoianda instanciação da variável _join com `new Condition`, mas não
                // compilava dando o seguinte erro na hora da que o linker executa:
                //      /bin/eposcc -Wa,--32 -c -ansi -O2  -o doctesting.o doctesting.cc
                //      /bin/eposcc --library  --gc-sections  -o doctesting doctesting.o
                //      /lib/libsys_ia32.a(thread.o): In function `EPOS::S::Thread::constructor_prolog(unsigned int)':
                //      thread.cc:(.text._ZN4EPOS1S6Thread18constructor_prologEj+0x15): undefined reference to `operator new(unsigned long)'
                //
                // Então, pesquisamos sobre isso no google e encontramos o seguinte link:
                //      By default, the Arduino IDE and libraries does not use the operator new and operator
                //      delete. It does support malloc() and free(). So the solution is to implement new
                //      and delete operators for yourself, to use these functions.
                //      http://forum.arduino.cc/index.php?topic=41485.0
                //     
                // Concluo que o EPOS tem implementado o operador de new, por que utilizamos ele nas
                // aplicações de exemplo como producer_consumer.cc, mas por algum motivo ele não está
                // disponível durante a inicialização do sistema ou para o sistema.
                // 
                // Olhando como as outras partes do sistema que fazem new encontrei o arquivo thread_init.cc com
                // a seguinte linha:
                //      _timer = new (kmalloc(sizeof(Scheduler_Timer))) Scheduler_Timer(QUANTUM, time_slicer);
                //
                // Então utilizei a mesma sintaxe aqui, e o new funcionou sem erros:
                // _join = new (kmalloc(sizeof(Condition))) Condition();
                // 
                // O método kmalloc faz o que? E por que new tem 2 parâmetros separados por espaço?
                // 
                // Depois de ler os seguintes links:
                //     http://www.cplusplus.com/reference/new/operator%20new/
                //     https://stackoverflow.com/questions/39496343/why-isnt-new-implemented-with-template
                //     https://stackoverflow.com/questions/8186018/how-to-properly-replace-global-new-delete-operators
                // 
                // Descobri que EPOS tem sua própria implementação de new definida nos arquivos types.h e malloc.h
                // Depois de ler: https://stackoverflow.com/questions/222557/what-uses-are-there-for-placement-new
                // 
                // Depois de pesquisar muito, entendi por que o new tradicional não está funcionando. 
                // Por que como EPOS não tem acesso ao STL/STD libraries, ele não tem um operator de
                // new definido. Assim, temos que incluir/criar a definição de um operador de new.
                // EPOS define o operador de new tradicional no arquivo `utility/malloc.h` que não
                // estava incluído aqui nesse arquivo. Mas agora que adicionei seu include aqui,
                // consigo utilizar normalmente o operator de new tradicional.
                // 
                // Portanto, a resposta encontra na internet anteriormente estava correta, o erro
                // "undefined reference to `operator new(unsigned long)`" acontece quando não existe a
                // definição de um operador de new.
                // 
                // Não utilizamos a piscina do sistema operacional por que não entendemos qual a
                // vantagem ou desvantagem de utilizar essa piscina, por que não entendemos o seu
                // funcionamento.
                // 
                // O link https://forum.arduino.cc/index.php?topic=74145.0 diz isso sobre new placement:
                //     Em um sistema embarcado, você sempre precisa saber o limite máximo de tudo o que
                //     está tentando fazer e, de alguma forma, é necessário impor esse limite. O
                //     comportamento "mais suave" do tradicional new e do delete normalmente não é
                //     aplicável, então eu prefiro usar buffers fixos e novos posicionamentos.
                // 
                // Assim, não sabemos se devemos utilizar o placement new ou o new tradicional.
                _join = new Condition();

                // Consigo entender que EPOS tem uma piscina de alocação de memória, e essa é a sintaxe
                // do C++ para alocar os objetos nessa piscina e esse operador new é chamado de
                // `placement new operator`. Isto é, EPOS pré-aloca uma região de memória e utiliza o
                // placement new operator para alocar os objectos nessa memória´ao invés de invocar o
                // operador de new convencional e alocar um novo pedaço de memória na heap. i.e.,
                // placement new operator constrói um objecto em um buffer pré-alocado.
                // 
                // No link: https://www.avrfreaks.net/forum/avr-c-micro-how?name=PNphpBB2&file=viewtopic&t=59453
                // Ele diz que para programar para um arduíno e compilar com g++ 4, é preciso definir
                // meus próprio operator de new e delete.
                // 
                // A de acordo com http://www.devx.com/tips/Tip/12582 a vantagem do placement new
                // operator é não há perigo de falha de alocação, pois a memória já foi alocada e a
                // construção de um objeto em um buffer pré-alocado leva menos tempo.
                // 
                // Assim, para sistemas de tempo real, não queremos que ocorra alocação e dealocação de
                // memória dinamicamente por que não há garantias de quanto tempo isso irá tomar. Então,
                // com o placement new podemos pré-alocar um grande pedaço de memória previamente e
                // utilizar-mos o placement new para colocar esses novos objectos nela.
            }

            // Como dito anteriormente, escolhemos por utilizar uma variável de condição para
            // implementar o join. Para que a thread joinadora seja inserida na lista de espera e
            // colocada para dormir quando executar um _join->wait();
            // 
            // Agora ela só será acordada e removida da lista de espera quando a thread joinada executar
            // Thread::exit(), onde será realizado um _join->signal();
            // 
            // OBS: Como a thread joinadora chama o método join do objeto da thread joinada, as
            // operações wait() e signal() atuam na mesma variável de condição _join
            _joined = true;
            _join->wait(); 
        }
    }

    unlock();

    return *reinterpret_cast<int *>(_stack);
}


void Thread::pass()
{
    lock();

    db<Thread>(TRC) << "Thread::pass(this=" << this << ")" << endl;

    Thread * prev = _running;
    add_to_ready(prev);

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

    add_to_suspended(this);

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
   add_to_ready(this);

   unlock();
}


void Thread::add_to_suspended(Thread* prev)
{
    // Veja a explicação em add_to_ready(). Depois daquele problema, forçasse aqui a verificação
    // do estado anterior da thread antes de adicionar ela na fila de _suspended.
    if(prev->_state != FINISHING) 
    {
        prev->_state = SUSPENDED;
        _suspended.insert(&prev->_link);
    }
    else
    {
        db<Thread>(ERR) << "ERROR: A FINISHING thread is trying to be suspended (this=" << prev << ")!" << endl;
    }
}

void Thread::add_to_ready(Thread* prev)
{
    // A atual thread _running estará no estado FINISHING durante a execução, quando ele executar o
    // método broadcast() no método exit() para acorda as threads que joinaram a thread _running.
    // 
    // Adicionamos _running != FINISHING para solucionar o problema do método yield() ser invocado
    // para uma thread que tenha seu estado como FINISHING, quando flag preemptive esteja ativada em
    // wakeup_all(), fazendo com que uma thread fosse escolada novamente mesmo após ter invocado
    // exit(), i.e., uma thread que já terminou era escalonada novamente pelo escalonador por que o
    // método yield() coloca ela na fila de _ready.
    if(prev->_state != FINISHING) 
    {
        prev->_state = READY;
        _ready.insert(&prev->_link);
    }
    else
    {
        db<Thread>(ERR) << "ERROR: A finished thread is trying to run again (this=" << prev << ")!" << endl;
    }
}


// Class methods
void Thread::yield()
{
    lock();

    db<Thread>(TRC) << "Thread::yield(running=" << _running << ") state=[" << _running->_state << "]" << endl;

    if(!_ready.empty()) {
        Thread * prev = _running;
        add_to_ready(prev);

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

    // Uma thread pode querer "entrar no exit" caso a fila de pronto não esteja vazia e existam
    // outras threads para executar. Ou caso existam threads esperando pelo seu término por terem
    // joinado ela anteriormente. Caso hajam threads nela para executar, bloqueada na variável de
    // condição, escondidas do sistema operacional. 
    if(!_ready.empty() || _running->_joined) {
        Thread * prev = _running;
        prev->_state = FINISHING;
        *reinterpret_cast<int *>(prev->_stack) = status;

        // Quando uma thread termina sua execução, ela precisa acordar as threads que a joinaram e
        // estão esperando pelo seu término, para fazer isso ela executa um _join->broadcast().
        // Múltiplos joins são permitidos para a mesma thread.
        db<Thread>(TRC) << "Thread : [running=" << running() << "] [_joined=" << prev->_joined << "]" << endl;

        // Com esse broadcast nós acordamos as threads que deram join em prev e estão esperando por
        // seu término. Caso nenhuma thread tenha dado join em prev, broadcast não acorda nenhuma
        // thread.
        if (_running->_joined){
            prev->_join->broadcast();
            prev->_joined = false;
        }

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
        t->_waiting = 0;
        add_to_ready(t);
    }

    unlock();

    // Não vejo nenhum motivo para isso estar aqui, a não ser para dar dor de cabeça e causar o
    // problema descrito na função add_to_ready(Thread*).
    // 
    // Se você for querer ativar isso aqui, vai ter que desativar a mensagem de erro na função
    // add_to_ready(Thread*), por que ela vai começar a aparecer nas condições descritas lá.
    // 
    // if(preemptive)
    //     reschedule();
}


void Thread::wakeup_all(Queue * q)
{
    db<Thread>(TRC) << "Thread::wakeup_all(running=" << running() << ",q=" << q << ")" << endl;

    // lock() must be called before entering this method
    assert(locked());

    while(!q->empty()) {
        Thread * t = q->remove()->object();
        t->_waiting = 0;
        add_to_ready(t);
    }

    unlock();

    // Não vejo nenhum motivo para isso estar aqui, a não ser para dar dor de cabeça e causar o
    // problema descrito na função add_to_ready(Thread*).
    // 
    // Se você for querer ativar isso aqui, vai ter que desativar a mensagem de erro na função
    // add_to_ready(Thread*), por que ela vai começar a aparecer nas condições descritas lá.
    // 
    // if(preemptive)
    //     reschedule();
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
