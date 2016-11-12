#ifndef __traced_h
#define __traced_h 

__BEGIN_SYS

typedef enum
{
  DB_ABORT,
  DB_ERROR,
  DB_WARNING,
  DB_INFO,
  DB_TRACE
} Debug_Level;
 
typedef enum
{
  DB_SETUP,
  DB_INIT,
  DB_SYSTEM,
  DB_GUEST,
  DB_QUEUE,
  DB_SYSCALL,
  DB_TIMER,
  DB_CHRONOMETER,
  DB_NETWORK,
  DB_THREAD,
  DB_SEMAPHORE
} Debug_Module;

class Traced
{
public:
  Traced & operator << (const void *p) { cout << p; return *this; }
  Traced & operator << (const char *s) { cout << s; return *this; }
  Traced & operator << (int i) { cout << i; return *this; }
};

__END_SYS

#endif

