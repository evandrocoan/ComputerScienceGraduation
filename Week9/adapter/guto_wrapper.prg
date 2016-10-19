template<class OBJ, class RET>  // no arguments
RET invoke(OBJ* obj, RET(OBJ:: * func)())
{ enter(); RET r = (obj->*func)(); leave(); return r; }

template<class OBJ>  // no arguments, no return
void invoke(OBJ* obj, void (OBJ:: * func)())
{ enter(); (obj->*func)(); leave(); }

template<class OBJ, class RET, class ARG>  // one argument
RET invoke(OBJ* obj, RET(OBJ:: * func)(ARG), ARG arg)
{ enter(); RET r = (obj->*func)(arg); leave(); return r; }

template<class OBJ, class ARG>  // one argument, no return
void invoke(OBJ* obj, void(OBJ:: * func)(ARG), ARG arg)
{ enter(); (obj->*func)(arg); leave(); }

class A_Class
{
public:
    int operation1();
    int operation2(int i);
};

A_Class* obj = new A_Class;
invoke(obj, &A_Class::operation1);
invoke(obj, &A_Class::operation2, 1);
