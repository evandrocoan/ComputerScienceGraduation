namespace Interface 
{
    class Abstraction
    {
    protected:
	Abstraction() {}

    private:
	int operation1(void);
	void operation2(int);
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
    Abstraction* abs = new Abstraction;
    abs->operation1();
    abs->operation2(1);
    
    cout << "Size of interface: " << sizeof(Interface::Abstraction) << "\n";
    cout << "Size of implementation: " << sizeof(Abstraction) << "\n";
    cout << "Size of instance: " << sizeof(*abs) << "\n";

    delete abs;
    return 0;
}
