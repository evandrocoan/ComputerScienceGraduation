class Interface
{
public:
  virtual int op1(int p1);
  virtual int op2();

private:
  int a1;
};

class Abstraction: private Interface
{
public:
  int op1() {cout << "Abstraction::op1()" << endl;}

private:
  int a1;
};

int main()
{
  Interface *obj = new Abstraction;

  obj->op1();
  obj->op2();
}
