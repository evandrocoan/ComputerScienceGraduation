#ifndef __traits_h
#define __traits_h

__BEGIN_SYS

enum Type
{
    THREAD = 0,
    EXCLUSIVE_THREAD = THREAD,
    COOPERATIVE_THREAD = THREAD,
    CONCURRENT_THREAD = THREAD,
    MUTEX,
    SEMAPHORE,
    CONDITION,
    STREAM,
    PORT,
    MAILBOX,
    LAST_TYPE = MAILBOX,
    ANY_TYPE = 0xffff
};

template<class Imp>
class Traits
{
public:
    static const bool share = false;
    static const bool allocate = false;
    static const bool atomic = false;
    static const bool remote = true;
    static const bool time = false;
    static const bool debug = false;
    static const bool protect = false;

private:
    static const Type type = ANY_TYPE;
};

template<>
struct Traits<Exclusive_Thread>: public Traits<void> {
    static const Type type = EXCLUSIVE_THREAD; 
};

template<>
struct Traits<Cooperative_Thread>: public Traits<void> {
    static const Type type = COOPERATIVE_THREAD; 
};

template<>
struct Traits<Concurrent_Thread>: public Traits<void> {
    static const Type type = CONCURRENT_THREAD; 
};

template<>
struct Traits<Mutex>: public Traits<void> {
    static const Type type = MUTEX; 
};

template<>
struct Traits<Semaphore>: public Traits<void> {
    static const Type type = SEMAPHORE; 
};

template<>
struct Traits<Condition>: public Traits<void> {
    static const Type type = CONDITION; 
};

template<>
struct Traits<Stream>: public Traits<void> {
    static const Type type = STREAM; 
};

template<>
struct Traits<Port>: public Traits<void> {
    static const Type type = PORT; 
};

template<>
struct Traits<Mailbox>: public Traits<void> {
    static const Type type = MAILBOX; 
};


__END_SYS

#endif
