#include <iostream>
#include <synchronizer.h>
#include <thread.h>

using namespace System;

int coordinator(int n)
int worker(int n);

const int n_threads = 20;
const int iterations = 100;

Condition cond;
Semaphore sem(-19);
int number;

int main()
{
    Thread * thread[n_threads];

    thread[0] = new Thread(&do_something, n_threads);
    for(int i = 1; i < n_threads; i++)
	thread[i] = new Thread(&do_something, i);

//   for(int i = 0; i < n_threads; i++)
//     thread[i]->resume();

    for(int i = 0; i < n_threads; i++) {
	int ret;
	if(thread[i]->join(&ret)) {
	    cerr << "Error: cannot join thread!\n";
	    return 1;
	}
	cout << "Worker " << i << " returned " << ret << "\n";
    }
    
    return 0;
}

int coordinator(int n)
{
    cout << "I'm the coordinator!";

    for(int i = 0; i < iterations; i++) {
	cond.lock();
	sem.p();
	cond.broadcast();
	cond.unlock();
    }
    
}

int worker(int n)
{
    cout << "I'm worker " << n << "\n";

    for(int i = 0; i < iterations; i++) {
	cond.lock();
	sem.v();
	cond.wait();
	cout << "I, worker " << n << ", passed the barrier!\n";
	cond.unlock();
    }

    cout << "I, worker " << n << ", will die now\n";

    return n;
}

