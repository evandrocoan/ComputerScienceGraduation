#include <iostream>
#include <communicator.h>

using namespace System;

int main()
{
  Port port;

  port.send(1, "Hello World\n", 20);

  return 0;
}
