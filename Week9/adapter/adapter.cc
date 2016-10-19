#include <stream.h>
#include <stdlib.h>

using namespace std;

class Adaptee
{
public:
  void operation(int n) {cout << "n = " << n << "\n";};
};

class Target
{
public:
  virtual void operation(const char *n) = 0;
};

class Adapter: public Target, private Adaptee
{
public:
  void operation(const char *n) {Adaptee::operation(atoi(n));};
};

int main()
{
  Target *target = new Adapter;
  target->operation("1");
  delete target;
}
