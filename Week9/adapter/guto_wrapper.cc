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

template<class OBJ, class RET>  // no arguments, return
RET invoke(OBJ* obj, RET(OBJ:: * func)())
{ enter(); RET r = (obj->*func)(); leave(); return r; }

template<class OBJ>  // no arguments, no return
void invoke(OBJ* obj, void (OBJ:: * func)())
{ enter(); (obj->*func)(); leave(); }

template<class OBJ, class RET, class ARG>  // one argument, return
RET invoke(OBJ* obj, RET(OBJ:: * func)(ARG), ARG arg)
{ enter(); RET r = (obj->*func)(arg); leave(); return r; }

template<class OBJ, class ARG>  // one argument, no return
void invoke(OBJ* obj, void(OBJ:: * func)(ARG), ARG arg)
{ enter(); (obj->*func)(arg); leave(); }

class A_Class
{
public:
    int operation1() { cout << "operation1()\n"; }
    int operation2(int i) { cout << "operation2(" << i << ")\n"; }
};

int main()
{
    A_Class* obj = new A_Class;
    invoke(obj, &A_Class::operation1);
    invoke(obj, &A_Class::operation2, 1);
    delete obj;
    
    return 0;
}
