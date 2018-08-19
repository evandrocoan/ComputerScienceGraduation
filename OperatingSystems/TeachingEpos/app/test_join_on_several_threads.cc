#include <utility/ostream.h>
#include <thread.h>
#include <alarm.h>
#include <semaphore.h>

using namespace EPOS;
OStream cout;

Semaphore table;
Thread* master_thread;

int func_a(char);
int func_b(char);

/**
 * Neste teste, é criado uma thread global chamada master_thread e mais 5 working threads que sua
 * função é fazer join na thread master_thread, i.e., esperar por ela terminar. A master_thread
 * trata-se somente de um delay de 0.5 segundos. Assim que esse delay terminar, a master_thread
 * terminar, todas as working threads que estão esperando pelo master_thread terminar, serão
 * acordadas. Utilizou-se um semáforo nas working threads para impedir que o texto de saída delas
 * ficassem misturados com os das outras working threads, quando o escalonador fizer a troca de
 * contexto.
 *
 * Exemplo de saída:
 * Starting thread manual uniting tests...
 * 
 * Thread running=func_97...
 * Thread running=func_98...
 * Thread running=func_99...
 * Thread running=func_100...
 * Thread running=func_101...
 * Master Thread running=func_90...
 * join(97) Master Thread exited with status 90
 * join(98) Master Thread exited with status 90
 * join(99) Master Thread exited with status 90
 * join(100) Master Thread exited with status 90
 * join(101) Master Thread exited with status 90
 * 
 * Ending thread manual uniting tests...
 * The last thread in the system has exited!
 * Rebooting the machine ...
 */
int main()
{
    cout << "Starting thread manual uniting tests..." << endl << endl;
    master_thread = new Thread(&func_a, 'Z');
    Thread * threads[5];

    for(int i = 0; i < 5; i++)
        threads[i] = new Thread(&func_b, static_cast<char>('a' + i));

    int status_a = master_thread->join();

    for(int i = 0; i < 5; i++) {
        threads[i]->join();
        delete threads[i];
    }

    delete master_thread;
    cout << endl << "Ending thread manual uniting tests..." << endl;
}

int func_a(char character)
{
    Alarm::delay(500000);
    cout << "Master Thread running=func_" << (int)character << "..." << endl;
    return character;
}

int func_b(char character)
{
    cout << "Thread running=func_" << (int)character << "..." << endl;
    int status_a = master_thread->join();
    table.lock();
    cout << "join(" << (int)character << ") Master Thread exited with status " << status_a << endl;
    table.unlock();
    return character;
}
