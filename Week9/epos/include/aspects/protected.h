#ifndef __protected_h
#define __protected_h

__BEGIN_SYS
__BEGIN_INT

class Protected
{
protected:
  bool grant(const Id & id);
};

__END_INT
__END_SYS

#ifdef __CHECKED_H
#include __CHECKED_H
#endif

#ifdef __PERMITTED_H
#include __PERMITTED_H
#endif

#endif
