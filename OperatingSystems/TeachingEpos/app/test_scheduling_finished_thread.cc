#include <utility/ostream.h>
#include <thread.h>
#include <alarm.h>
#include <semaphore.h>

using namespace EPOS;
OStream cout;

int func_a(char);

/**
 * Um thread pode estar escondida do sistema operacional dentro da variável de condição da thread A.
 * Assim, caso o sistema não esteja preparado para esta situação, o sistema irá terminar antes que a
 * thread main() finalize a sua execução.
 *
 * Para fazermos testarmos se isso funciona corretamente, somente criamos uma thread A que
 * inicialmente executa um delay de 0.5 segundos forçando a thread main() esperar pela thread A.
 * Assim que isso acontece, a thread main() é presa dentro da variável de condição da thread A, e
 * assim sendo escondido do sistema operacional.
 *
 * Em uma execução incorreta, a thread main() não retorna a execução quando a thread A termina, e o
 * sistema termina a execução prematuramente, sem que a thread principal termine a sua execução.
 * 
 * Exemplo de saída INCORRETA:
 * Starting thread manual uniting tests...
 * 
 * Thread running=func_97...
 * The last thread in the system has exited!
 * Rebooting the machine ...
 *
 * Exemplo de saída CORRETA:
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

    int status_a = a->join();
    cout << "Thread A exited with status " << status_a << endl;

    delete a;
    cout << endl << "Ending thread manual Unit Tests..." << endl;
}

int func_a(char character)
{
    Alarm::delay(500000);
    cout << "Thread running=func_" << (int)character << "..." << endl;
    return character;
}
