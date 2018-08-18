// EPOS Thread Test Program

#include <utility/ostream.h>
#include <thread.h>
#include <alarm.h>
#include <semaphore.h>

using namespace EPOS;
OStream cout;

Semaphore table;
int func_a(char);

int main()
{
    cout << "Starting thread manual uniting tests..." << endl << endl;

    // cout << "Thread::_ready.size " << Thread::_ready.size() << endl;
    // cout << "Thread::_suspended.size " << Thread::_suspended.size() << endl;

    Thread * a = new Thread(&func_a, 'a');
    Thread * b = new Thread(&func_a, 'b');

    int status_a = a->join();
    int status_b = b->join();

    cout << "Thread A exited with status " << status_a << endl;
    cout << "Thread B exited with status " << status_b << endl;

    delete a;
    delete b;

    cout << endl << "Ending thread manual uniting tests..." << endl;
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
        Alarm::delay(500000);
    }

    table.unlock();
    return character;
}
