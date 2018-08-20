#include <utility/ostream.h>
#include <thread.h>
#include <alarm.h>
#include <semaphore.h>

using namespace EPOS;
OStream cout;

int func_a(char);

/**
 * Simples teste que cria uma thread e faz join depois que ela já terminou sua execução. Para fazer
 * isso, cria-se a thread que executa um simples função que termina rapidamente. Enquanto isso, a
 * thread principal faz um delay de 0.5 segundos, considerados suficientes para que a thread A
 * já tenha terminado sua execução quando a thread principal realizar o join().
 *
 * Exemplo de saída:
 * Starting thread manual Unit Tests...
 * 
 * Thread running=func_97...
 * Thread A exited with status 97
 * 
 * Ending thread manual Unit Tests...
 * The last thread in the system has exited!
 * Rebooting the machine ...
 */
int main()
{
    cout << "Starting thread manual Unit Tests..." << endl << endl;

    Thread * a = new Thread(&func_a, 'a');
    Alarm::delay(500000);

    int status_a = a->join();
    cout << "Thread A exited with status " << status_a << endl;

    delete a;
    cout << endl << "Ending thread manual Unit Tests..." << endl;
}

int func_a(char character)
{
    cout << "Thread running=func_" << (int)character << "..." << endl;
    return character;
}
