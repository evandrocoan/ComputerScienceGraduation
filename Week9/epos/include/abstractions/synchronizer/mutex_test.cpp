#include <iostream>
#include <synchronizer.h>
#include <thread.h>

using namespace System;

int philosopher(int n);

Mutex fork[5];
const int iterations = 100;

int main()
{
  cout << "The Philosopher's Dinner\n";

  for(int i = 0; i < 5; i++)
    fork[i].lock();

  Thread * phil[5];
  for(int i = 0; i < 5; i++)
    phil[i] = new Thread(&philosopher, i);

  cout << "Philosophers are alife and hungry!\n";

  for(int i = 0; i < 1000000000; i++);
  for(int i = 0; i < 5; i++)
    fork[i].unlock();

  cout << "The dinner is served!\n";

  for(int i = 0; i < 5; i++) {
    int ret;
    if(phil[i]->join(&ret)) {
      cerr << "Error: cannot join thread!\n";
      return 1;
    }
    cout << "Philosopher " << i << " ate " << ret << " times \n";
  }

  cout << "The end!\n";

  return 0;
}

int philosopher(int n)
{
  cout << "I'm the philosopher " << n << "\n";

  for(int i = iterations; i > 0; i--) {
    if(n < 4) {
      cout << "Philosopher " << n << " thinking ...\n";

      fork[n].lock(); // get left fork
      cout << "Philosopher " << n << " got left fork ...\n";
      fork[(n + 1) % 5].lock(); // get right fork
      cout << "Philosopher " << n << " got right fork ...\n";

      cout << "Philosopher " << n << " eating ...\n";

      fork[n].unlock(); // release left fork
      cout << "Philosopher " << n << " released left fork ...\n";
      fork[(n + 1) % 5].unlock(); // release right fork
      cout << "Philosopher " << n << " released right fork ...\n";
    } else {
      cout << "Philosopher " << n << " thinking ...\n";

      fork[(n + 1) % 5].lock(); // get right fork
      cout << "Philosopher " << n << " got left fork ...\n";
      fork[n].lock(); // get left fork
      cout << "Philosopher " << n << " got right fork ...\n";

      cout << "Philosopher " << n << " eating ...\n";

      fork[(n + 1) % 5].unlock(); // release right fork
      cout << "Philosopher " << n << " released left fork ...\n";
      fork[n].unlock(); // release left fork
      cout << "Philosopher " << n << " released right fork ...\n";
    }
    Thread::yield();
  }

  return iterations;
}

