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
    Alarm::delay(10000);

    cout << endl << "Ending thread manual Unit Tests..." << endl;
}

int func_a(char character)
{
    cout << "I am deleting mysef right now: func_" << (int)character << "..." << endl;
    Thread::self()->delete_me();
    return 0;
}
