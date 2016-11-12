#define interface(INT, OPS...) \
namespace Interface {\
  class INT {\
  protected:\
    INT();\
    OPS;\
  };\
}

#define realizes private Interface::

interface(Abstraction, int op1(); void op2(int));

class Abstraction: realizes Abstraction
{
public:
  int op1();
  void op2(int);
private:
  int data;
};

#include <iostream>
using namespace std;

int main()
{
  cout << "Size of interface: " << sizeof(Interface::Abstraction) << "\n";
  cout << "Size of implementation: " << sizeof(Abstraction) << "\n";
}
