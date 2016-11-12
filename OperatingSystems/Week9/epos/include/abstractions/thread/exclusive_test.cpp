#include <iostream>
#include <thread.h>

using namespace System;

int main()
{
  Thread self(SELF);

  Thread_Status status;
  self.stat(&status);
  cout << "Thread state is " << status.state << "\n";

  Thread::exit(8); // return would do
}
