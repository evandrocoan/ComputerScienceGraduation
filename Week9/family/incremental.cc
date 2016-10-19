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
    ~Member_A();
    int operation1(void);

private:
    // Implementation declarations
    int data1;
};

class Member_B: public Member_A
{
public:
    void operation2(int i);

private:
    // Implementation declarations
    int data2;
};

typedef Member_B Member_AB;

#include <iostream>
using namespace std;

Member_A::Member_A()
{ cout << "Member_A()\n"; }
Member_A::~Member_A()
{ cout << "~Member_A()\n"; }
int Member_A::operation1(void)
{ cout << "Member_A::operation1()\n"; return 1; }

void Member_B::operation2(int i)
{ cout << "Member_B::operation2(" << i << ")\n"; }

int main()
{
    Member_A* instance_a = new Member_A;
    instance_a->operation1();
    delete instance_a;
    
    Member_B* instance_b = new Member_B;
    instance_b->operation2(instance_b->operation1());
    delete instance_b;

    Member_AB* instance_ab = new Member_AB;
    instance_ab->operation2(instance_ab->operation1());
    delete instance_ab;
}
