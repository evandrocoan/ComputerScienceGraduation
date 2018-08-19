#include <utility/ostream.h>
#include <thread.h>
#include <alarm.h>
#include <semaphore.h>

using namespace EPOS;
OStream cout;

int func_a(char);
Thread * a;
Semaphore table;

/**
 * Este teste possui uma thread global chamada A, e quando o programa principal main() executa, ele
 * constrói a thread A, e então com o auxílio de um semáforo inicializado em 1, bloqueia-se a
 * execução o programa main() até que a thread A termine sua execução.
 *
 * A thread A executa um simples código que faz um join() nela própria. Na implementação original,
 * isso causaria com que o sistema operacional ficasse bloqueado permanentemente. Na nova
 * implementação, ele simplesmente ignora a chamada ao join e emite uma mensagem de erro, informando
 * que não é permitido dar join em si próprio.
 *
 * Exemplo de saída:
 * Starting thread manual uniting tests...
 * 
 * Thread running=func_97...
 * ERROR: Thread trying to join itself, join(this=0x0ffadfd8)
 * I successfully joined myself with status 0
 * Thread A exited with status 97
 * 
 * Ending thread manual uniting tests...
 * The last thread in the system has exited!
 * Rebooting the machine ...
 * 
 */
int main()
{
    cout << "Starting thread manual uniting tests..." << endl << endl;

    a = new Thread(&func_a, 'a');
    table.lock();

    int status_a = a->join();
    cout << "Thread A exited with status " << status_a << endl;

    delete a;
    cout << endl << "Ending thread manual uniting tests..." << endl;
}

int func_a(char character)
{
    cout << "Thread running=func_" << (int)character << "..." << endl;

    int status_a = a->join();
    cout << "I successfully joined myself with status " << status_a << endl;

    table.unlock();
    return character;
}
