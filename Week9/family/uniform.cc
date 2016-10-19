class Common
{
protected:
    Common() {}

    // Family commonalities
    int common;
};

class Member_A: protected Common
{
public:
    Member_A();
    virtual ~Member_A();
    virtual int operation1(void);
    virtual void operation2(int i);

private:
    // Implementation declarations
    int data1;
};

class Member_B: public Member_A
{
public:
    Member_B();
    ~Member_B();
    int operation1(void);
    void operation2(int i);

private:
    // Implementation declarations
    int data2;
};

#include <iostream>
using namespace std;

Member_A::Member_A()
{ cout << "Member_A()\n"; }
Member_A::~Member_A()
{ cout << "~Member_A()\n"; }
int Member_A::operation1(void)
{ cout << "Member_A::operation1()\n"; return 1; }
void Member_A::operation2(int i)
{ cout << "Member_A::operation2(" << i << ")\n"; }

Member_B::Member_B()
{ cout << "Member_B()\n"; common = 1; }
Member_B::~Member_B()
{ cout << "~Member_B()\n"; }
int Member_B::operation1(void)
{ cout << "Member_B::operation1()\n"; return 1; }
void Member_B::operation2(int i)
{ cout << "Member_B::operation2(" << i << ")\n"; }

typedef Member_A Family;

int main()
{
    Family* instance = new Member_A;
    instance->operation2(instance->operation1());
    delete instance;

    instance = new Member_B;
    instance->operation2(instance->operation1());
    delete instance;
}
