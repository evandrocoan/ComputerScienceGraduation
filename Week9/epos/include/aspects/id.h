#ifndef __id_h
#define __id_h

#include <config.h>

__BEGIN_SYS

// Identifies the type of an abstraction
typedef unsigned short Type;
enum {ANY_TYPE = 0xffff};

// Identifies an instance of an abstraction
typedef unsigned short Unit;
enum {ANY_UNIT = 0xffff};

// Designate the permissions over an instace
typedef unsigned short Rights;
enum {ALL_RIGHTS = 0xffff};

__BEGIN_INT

class Id
{
public:
  Id(const Type & type) __DEF;
  Id(const Type & type, const Unit & unit) __DEF;
  Id(const Type & type, const Unit & unit, const Rights & rights) __DEF;
};

class Pointer_Id {};

class Local_Id
{
public:
  Local_Id(const Type & type, const Unit & unit = ANY_UNIT) __DEF;
};

class Global_Id
{
public:
  Global_Id(const Type & type, const Unit & unit = ANY_UNIT) __DEF;
};

class Protected_Id
{
public:
  Protected_Id(const Type & type, const Unit & unit = ANY_UNIT, 
	       const Rights & rights = ALL_RIGHTS) __DEF;
};

__END_INT
__END_SYS

#ifdef __POINTER_ID_H
#include __POINTER_ID_H
#endif

#ifdef __LOCAL_ID_H
#include __LOCAL_ID_H
#endif

#ifdef __GLOBAL_ID_H
#include __GLOBAL_ID_H
#endif

#ifdef __PROTECTED_ID_H
#include __PROTECTED_ID_H
#endif

#endif
