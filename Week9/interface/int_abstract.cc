namespace Interface
{
    class Abstraction
    {
    public:
	virtual int operation1(void) = 0;
	virtual void operation2(int) = 0;
    };
}

class Abstraction: private Interface::Abstraction
{
public:
    int operation1(void);
    void operation2(int);

private:
    // Implementation declarations
    int data;
};

#include <iostream>
using namespace std;

int Abstraction::operation1(void)
{ cout << "Abstraction::operation1()\n"; return 1; }
void Abstraction::operation2(int i)
{ cout << "Abstraction::operation2(" << i << ")\n"; }

int main()
{
    Abstraction* instance = new Abstraction;
    instance->operation2(instance->operation1());
    
    cout << "Size of interface: " << sizeof(Interface::Abstraction) << "\n";
    cout << "Size of implementation: " << sizeof(Abstraction) << "\n";
    cout << "Size of instance: " << sizeof(*instance) << "\n";

    delete instance;
    return 0;
}
