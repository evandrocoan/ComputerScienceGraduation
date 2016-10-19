#include <iostream>
#include <synchronizer.h>
#include <thread.h>

using namespace System;

int coordinator(int n);
int worker(int n);

const int n_threads = 20;
const int iterations = 2;

Condition cond[2];
Semaphore sem0(0), sem1(0);
int number;

int main()
{
    Thread * thread[n_threads];

    thread[0] = new Thread(&coordinator, n_threads);
    for(int i = 1; i < n_threads; i++)
	thread[i] = new Thread(&worker, i);

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
    cout << "I'm the coordinator!\n";

    for(int i = 0; i < iterations; i++) {
	for(int i = 1; i < n_threads; i++) {
	    sem0.p();
	}
	cout << "All workers are waiting on the barrier!\n";
	for(int i = 1; i < n_threads; i++) {
	    sem1.v();
	}
    }
    
    return n;
}

int worker(int n)
{
    cout << "I'm worker " << n << "\n";

    for(int i = 0; i < iterations; i++) {
	sem0.v();
	sem1.p();
	cout << "I, worker " << n << ", passed the barrier!\n";
    }

    cout << "I, worker " << n << ", will die now\n";

    return n;
}

