#include <iostream>
#include <synchronizer.h>
#include <thread.h>
using namespace std;
using namespace System;

const int n_threads = 2;
const int iterations = 100;

int thread0(int n);
int thread1(int n);

int x;
int y;
Condition cond;

int main()
{
    Thread * thread[n_threads];

    thread[0] = new Thread(&thread0, 0, SUSPENDED);
    thread[1] = new Thread(&thread1, 1, SUSPENDED);

    for(int i = 0; i < n_threads; i++)
	thread[i]->resume();

    for(int i = 0; i < n_threads; i++) {
	int ret;
	if(thread[i]->join(&ret)) {
	    cerr << "Error: cannot join thread!\n";
	    return 1;
	}
	cout << "Thread " << i << " returned " << ret << "\n";
    }
    
    return 0;
}


int thread0(int n)
{
    cout << "I'm thread " << n << "\n";

    do {
	cond.lock();
	while(x >= y)
	    cond.wait();
	x *= 2;
	cout << "X = " << x << ", Y = " << y << "\n";
	cond.unlock();
    } while(1);

    return 0;
}

int thread1(int n)
{
    cout << "I'm thread " << n << "\n";

     do {
	 cond.lock();
	 x++; y += 2;
	 if(x < y)
	     cond.signal();
	 cond.unlock();
     } while(1);

     return 0;
}

