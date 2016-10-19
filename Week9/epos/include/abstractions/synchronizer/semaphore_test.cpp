#include <iostream>
#include <synchronizer.h>
#include <thread.h>

using namespace System;
using namespace std;

Semaphore fork[5];
static const int n_iterations = 1000;

int philosopher(int n)
{
    int first = (n < 4)?n:0;
    int second = (n < 4)?n + 1:4;
    for(int i = 0; i < n_iterations; i++) {
	cout << "Philosopher " << n << " thinking ...\n";
	fork[first].p();   // get first fork
	fork[second].p();   // get second fork
	cout << "Philosopher " << n << " eating ...\n";
	fork[first].v();   // release first fork
	fork[second].v();   // release second fork
    }
}

int main()
{
    Thread * phil[5];
    for(int i = 0; i < 5; i++)
	phil[i] = new Thread(&philosopher, i);

    for(int i = 0; i < 5; i++) {
	int ret;
	phil[i]->join(&ret);
    }

  return 0;
}


