namespace Interface
{
    class Family    // Inflated interface
    {
    public:
	Family();
	Family(int i);
	
	int operation1(void);
	void operation2(int i);
    };
}

class Common
{
protected:
    Common() {}

    // Family commonalities
    int common;
};

class Member_A: protected Common    // Partial realization
{
public:
    Member_A();
    ~Member_A();
    int operation1(void);

private:
    // Implementation declarations
    int data1;
};

class Member_B: public Member_A    // Full realization
{
public:
    Member_B(int i = 0);
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

Member_B::Member_B(int i) : data2(i)
{ cout << "Member_B(" << i << ")\n"; }
void Member_B::operation2(int i)
{ cout << "Member_B::operation2(" << i << ")\n"; }

#define FAMILY MEMBER_B
#define MEMBER_A 1
#define MEMBER_B 2
#define MEMBER_AB 3
#if FAMILY == MEMBER_A
typedef Member_A Family;
#elif FAMILY == MEMBER_B
typedef Member_B Family;
#else
typedef Interface::Family Family;
#endif

int main()
{
    Family instance;
    instance.operation1();
    instance.operation2(2);
}
