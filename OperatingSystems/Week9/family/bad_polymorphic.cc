class Common
{
protected:
    Common() {}

    // Family commonalities
    int common;
};

class Synchronizer: protected Common
{
protected:
    Synchronizer();

public:
    virtual ~Synchronizer();
    virtual void p();
    virtual void v();
    virtual void wait();
    virtual void signal();
    virtual void broadcast();

protected:
    // Implementation declarations
    int data1;
};

class Semaphore: public Synchronizer
{
public:
    Semaphore(int value = 1);
    ~Semaphore();
    void p();
    void v();

private:
    // Implementation declarations
    int data2;
};

class Condition: public Synchronizer
{
public:
    Condition();
    ~Condition();
    void wait();
    void signal();
    void broadcast();

private:
    // Implementation declarations
    int data2;
};

#include <iostream>
using namespace std;

Synchronizer::Synchronizer() { cout << "Synchronizer()\n"; }
Synchronizer::~Synchronizer() { cout << "~Synchronizer()\n"; }
void Synchronizer::p() { cout << "Error: Synchronizer::p()\n"; }
void Synchronizer::v() { cout << "Error: Synchronizer::v()\n"; }
void Synchronizer::wait() { cout << "Error: Synchronizer::wait()\n"; }
void Synchronizer::signal() { cout << "Error: Synchronizer::signal()\n"; }
void Synchronizer::broadcast() { cout << "Error: Synchronizer::broadcast()\n"; }

Semaphore::Semaphore(int) { cout << "Semaphore()\n"; }
Semaphore::~Semaphore() { cout << "~Semaphore()\n"; }
void Semaphore::p() { cout << "Semaphore::p()\n"; }
void Semaphore::v() { cout << "Semaphore::v()\n"; }

Condition::Condition() { cout << "Condition()\n"; }
Condition::~Condition() { cout << "~Condition()\n"; }
void Condition::wait() { cout << "Condition::wait()\n"; }
void Condition::signal() { cout << "Condition::signal()\n"; }
void Condition::broadcast() { cout << "Condition::broadcast()\n"; }

int main()
{
    Synchronizer* sync = new Condition;
    sync->wait();    // OK
    sync->p();    // No meaning, but no compiling error
    delete sync;
    
    sync = new Semaphore;
    sync->p();    // OK
    sync->wait();    // No meaning, but no compiling error
    delete sync;
}
