#ifndef __debugged_h
#define __debugged_h 

__BEGIN_SYS
__BEGIN_INT

class Debugged
{
public:
  Debugged & operator << (const void *p) {}
  Debugged & operator << (const char *s) {}
  Debugged & operator << (int i) {}
};

__END_INT
__END_SYS

#ifdef __WATCHED_H
#include __WATCHED_H
#endif

#ifdef __TRACED_H
#include __TRACED_H
#endif

#ifdef __PROFILED_H
#include __PROFILED_H
#endif

#endif

