#include <iostream>
#include <thread.h>

using namespace System;

Thread self(SELF);

int do_something(int n);

int main()
{
  Thread * thread[10];
  for(int i = 0; i < 10; i++) {
    thread[i] = new Thread(&do_something, i, SUSPENDED);
    if(!thread[i]->valid()) {
      cerr << "Error: cannot create thread!\n";
      return 1;
    }
  }

  for(int i = 9; i >= 0; i--)
    thread[i]->resume();

  self.suspend();

  cout << "Waiting for thread termination!\n";
  for(int i = 0; i < 10; i++) {
    int ret;
    if(thread[i]->join(&ret)) {
      cerr << "Error: cannot join thread!\n";
      return 1;
    }
    cout << "Thread[" << i << "] returned " << ret << "\n";
  }

  Thread_Status status;
  self.stat(&status);
  cout << "Thread state is " << status.state << "\n";

  return 0;
}

int do_something(int n)
{
  if(n == 5) {
    for(int i = 0; i < 100000000; i++);   
    self.resume();
  }

  cout << "do_something(" << n << ")\n";

  Thread::exit(n + 1); // return would do
}

