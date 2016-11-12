#ifndef OUT
#include <iostream>
using namespace std;
#else
class Null
{
public:
  Null & operator << (const void *p) {return *this; }
  Null & operator << (const char *s) {return *this; }
  Null & operator << (int i) {return *this; }
} cout, cerr;
#endif                                                                         

void enter(void) { cout << "enter()\n"; }
void leave(void) { cout << "leave()\n"; }

template <class T>
class Call_Proxy
{
public:
    Call_Proxy(T* pp) : p(pp) {}
    ~Call_Proxy() { leave(); }
    T* operator->() { return p; }
    
private:
    T* p;
};

template <class T>
class Wrap
{
public:
    Wrap(T* pp) : p(pp) {}
    Call_Proxy<T> operator->() { enter(); return Call_Proxy<T>(p); }
    
private:
    T* p;
};

class A_Class
{
public:
    int operation1() { cout << "operation1()\n"; }
    int operation2(int i) { cout << "operation2(" << i << ")\n"; }
};

int main()
{
    Wrap<A_Class> obj(new A_Class);
    obj->operation1();
    obj->operation2(2);
    
    return 0;
}
