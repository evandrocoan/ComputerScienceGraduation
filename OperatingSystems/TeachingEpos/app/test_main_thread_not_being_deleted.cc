#include <utility/ostream.h>
#include <thread.h>
#include <alarm.h>
#include <semaphore.h>

using namespace EPOS;
OStream cout;

int func_a(char);

int main()
{
    cout << "Starting thread manual Unit Tests..." << endl << endl;

    Thread * a = new Thread(&func_a, 'a');

    cout << endl << "Ending thread manual Unit Tests..." << endl;
}

int func_a(char character)
{
    Alarm::delay(10000);
    cout << "Thread running=func_" << (int)character << "..." << endl;
    return character;
}
