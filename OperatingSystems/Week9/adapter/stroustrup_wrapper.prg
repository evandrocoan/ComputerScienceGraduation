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
    Call_Proxy<T> operator->()
	{ enter(); return Call_Proxy<T>(p); }
    
private:
    T* p;
};

class A_Class
{
public:
    int operation1();
    int operation2(int);
};

Wrap<A_Class> obj(new A_Class);
obj->operation1();
obj->operation2(2);
