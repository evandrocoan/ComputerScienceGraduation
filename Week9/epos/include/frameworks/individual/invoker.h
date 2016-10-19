#ifndef __invoker_h
#define __invoker_h

__BEGIN_SYS

class Invoker;
extern void trap(Invoker*);

enum Method
{
    CREATE = 0,
    DESTROY = 1,
    SHARE = 2,
    FAMILY = 3,
    THREAD_SUSPEND = FAMILY,
    THREAD_RESUME,
    THREAD_JOIN,
    THREAD_PASS,
    THREAD_YIELD,
    THREAD_EXIT,
    SYNCHRONIZER_LOCK = FAMILY,
    SYNCHRONIZER_UNLOCK,
    SYNCHRONIZER_P,
    SYNCHRONIZER_V,
    SYNCHRONIZER_WAIT,
    SYNCHRONIZER_SIGNAL,
    SYNCHRONIZER_BROADCAST
};

class Invoker
{
public:
    int invoke(const Method &m)	{
	cout << "invoke(" << m << ")\n"; 
	_method = m;
	trap(this);
	return _method;
    }

    template<class T1>
    int invoke(const Method &m, const T1 &p1)
	{ parm(p1); return invoke(m); }
    
    template<class T1, class T2>
    int invoke(const Method &m, const T1 &p1, const T2 &p2)
	{ parm(p1, p2); return invoke(m); }
    
    template<class T1, class T2, class T3>
    int invoke(const Method &m, const T1 &p1, const T2 &p2, const T3 &p3)
	{ parm(p1, p2, p3); return invoke(m); }
    
    const Method &method() { return _method; }
    void result(const Method &r) { _method = r; }

    const Id &id() { return _id; }

private:
    template<class T1>
    void parm(const T1 &v1)
	{reinterpret_cast<T1 &>(parms[0]) = v1;}

    template<class T1, class T2>
    void parm(const T1 &v1, const T2 &v2)
	{reinterpret_cast<T1 &>(parms[0]) = v1;
	reinterpret_cast<T2 &>(parms[sizeof(T1)]) = v2;}

    template<class T1, class T2, class T3>
    void parm(const T1 &v1, const T2 &v2, const T3 &v3)
	{reinterpret_cast<T1 &>(parms[0]) = v1;
	reinterpret_cast<T2 &>(parms[sizeof(T1)]) = v2;
	reinterpret_cast<T3 &>(parms[sizeof(T1) + sizeof(T2)]) = v3;}

    Id _id;
    Method _method;
    int length;
    char parms[20];
};

__END_SYS

#include "agent.h"

#endif
