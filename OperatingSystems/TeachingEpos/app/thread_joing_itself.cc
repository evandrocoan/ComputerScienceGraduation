// EPOS Thread Test Program

#include <utility/ostream.h>
#include <thread.h>
#include <alarm.h>
#include <semaphore.h>

using namespace EPOS;
OStream cout;

int func_a(char);
Thread * a;

int main()
{
    cout << "Starting thread manual uniting tests..." << endl << endl;

    a = new Thread(&func_a, 'a');
    int status_a = a->join();
    cout << "Thread A exited with status " << status_a << endl;

    delete a;
    cout << endl << "Ending thread manual uniting tests..." << endl;
}

int func_a(char character)
{
    cout << "Thread running=func_" << (int)character << "..." << endl;
    a->join();

    return 0;
}
