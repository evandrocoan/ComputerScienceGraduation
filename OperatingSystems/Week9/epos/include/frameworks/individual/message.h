#ifndef __message_h
#define __message_h

__BEGIN_SYS

class Message;
extern void trap(Message*);

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

class Message: public Id
{
public:
    int invoke(const Method &m)	{
	cout << "invoke(" << m << ")\n"; 
	_method = m;
	trap(this);
	return _method;
    }

    void method(const Method &m) { _method = m; }
    const Method &method() { return _method; }

    void result(const Method &r) { _method = r; }
    const Method &result() { return _method; }

//    const Id &id() { return _id; }

protected:
    template<class T1>
    void out(const T1 &v1)
	{reinterpret_cast<T1 &>(parms[0]) = v1;}

    template<class T1, class T2>
    void out(const T1 &v1, const T2 &v2)
	{reinterpret_cast<T1 &>(parms[0]) = v1;
	reinterpret_cast<T2 &>(parms[sizeof(T1)]) = v2;}

    template<class T1, class T2, class T3>
    void out(const T1 &v1, const T2 &v2, const T3 &v3)
	{reinterpret_cast<T1 &>(parms[0]) = v1;
	reinterpret_cast<T2 &>(parms[sizeof(T1)]) = v2;
	reinterpret_cast<T3 &>(parms[sizeof(T1) + sizeof(T2)]) = v3;}

    template<class T1>
    void in(T1 &v1)
	{v1 = reinterpret_cast<T1 &>(parms[0]);}
    
    template<class T1, class T2>
    void in(T1 &v1, T2 &v2)
	{v1 = reinterpret_cast<T1 &>(parms[0]);
	v2 = reinterpret_cast<T2 &>(parms[sizeof(T1)]);}
    
    template<class T1, class T2, class T3>
    void in(T1 &v1, T2 &v2, T3 &v3)
	{v1 = reinterpret_cast<T1 &>(parms[0]);
	v2 = reinterpret_cast<T2 &>(parms[sizeof(T1)]);
	v3 = reinterpret_cast<T3 &>(parms[sizeof(T1) + sizeof(T2)]);}               
    Method _method;
    int length;
    char parms[20];
};

__END_SYS

#endif
