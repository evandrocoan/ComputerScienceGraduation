#include <utility/ostream.h>
#include <thread.h>
#include <alarm.h>
#include <semaphore.h>

using namespace EPOS;
OStream cout;

Semaphore table;
int func_a(char);

/**
 * Este teste cria 2 threads na thread main() e espera 0.5 segundos. Nesse tempo, tanto a thread A
 * e thread B vão executando e fazendo seu trabalho. O trabalho que as threads A e B fazem é
 * imprimir um texto na tela pelo tempo de 1 segundo. Depois de passados 0.5 segundos, a thread
 * main() deletada a thread B e faz join na thread A. Uma vez que a thread A termina, é a vez de
 * fazer join na thread B. Como ela já foi deletada, essa operação é indefinida.
 *
 * Se este código for executado na implementação original, isso faria com que o sistema ficasse em
 * uma espera ocupada infinitamente. Nessa nova implementação, ele simplesmente continua como se nada
 * tivesse acontecido. Entretanto, vemos lixo no valor de retorno da thread B.
 *
 * Exemplo de saída:
 * Starting thread manual Unit Tests...
 * 
 * Thread running=func_97...
 * aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
 * aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
 * Thread A exited with status 97
 * Thread B exited with status 1527039107
 * 
 * Ending thread manual Unit Tests...
 * The last thread in the system has exited!
 * Rebooting the machine ...
 */
int main()
{
    cout << "Starting thread manual Unit Tests..." << endl << endl;

    Thread * a = new Thread(&func_a, 'a');
    Thread * b = new Thread(&func_a, 'b');

    Alarm::delay(500000);
    delete b;

    int status_a = a->join();
    int status_b = b->join();

    cout << "Thread A exited with status " << status_a << endl;
    cout << "Thread B exited with status " << status_b << endl;

    delete a;

    cout << endl << "Ending thread manual Unit Tests..." << endl;
}

int func_a(char character)
{
    table.lock();
    cout << "Thread running=func_" << (int)character << "..." << endl;

    for(int i = 0; i < 2; i++)
    {
        for(int i = 0; i < 79; i++)
        {
            cout << character;
        }

        cout << endl;

        // Se trocarmos esse delay de 0.5 segundos para 0.0005 segundos, isso fará com que o
        // programa tente fazer join na thread que depois que ela foi deletada, e teve seu
        // status colocado para FINISHING.
        // Alarm::delay(5000);
        Alarm::delay(500000);
    }

    table.unlock();
    return character;
}
