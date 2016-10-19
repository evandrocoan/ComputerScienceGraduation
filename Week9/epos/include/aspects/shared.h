#ifndef __shared_h
#define __shared_h

__BEGIN_SYS
__BEGIN_INT

class Shared
{
protected:
  int inc_refs();
  int dec_refs();
};

__END_INT
__END_SYS

#ifdef __REFERENCED_H
#include __REFERENCED_H
#endif

#ifdef __ENROLLED_H
#include __ENROLLED_H
#endif

#endif
