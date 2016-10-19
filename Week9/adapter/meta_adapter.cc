#include <stream.h>
#include <stdlib.h>

using namespace std;

class Adaptee
{
public:
  void operation(int n) {cout << "n = " << n << "\n";};
};

template<class Adaptee>
class MetaAdapter: public Adaptee
{
public:
  void operation(const char *n) {Adaptee::operation(atoi(n));};
};

typedef MetaAdapter<Adaptee> Adapter;
typedef Adapter Target;

int main()
{
  Target *target = new Adapter;
  target->operation("1");
  delete target;
}
