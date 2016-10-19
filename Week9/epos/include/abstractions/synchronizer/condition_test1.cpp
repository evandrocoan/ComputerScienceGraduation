#include <iostream>
#include <synchronizer.h>
#include <thread.h>

using namespace System;

int do_something(int n);

const int n_threads = 2;
const int iterations = 100;

Condition cond;
int number;

int main()
{
  Thread * thread[n_threads];

  for(int i = 0; i < n_threads; i++)
    thread[i] = new Thread(&do_something, i, SUSPENDED);

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

int do_something(int n)
{
  cout << "I'm thread " << n << "\n";

  while(number < 1000) {
    if(n == 1) {
      cond.lock();
      number++;
      if((number % 100) == 0)
	cond.signal();
      cond.unlock();
      Thread::yield();
    } else {
      cond.lock();
      while((number % 100) != 0)
	cond.wait();
      cout << "Thread[" << n <<"]: number = " << number << "\n";
      number++;
      cond.unlock();
      Thread::yield();
    }
  }

  return number;
}

