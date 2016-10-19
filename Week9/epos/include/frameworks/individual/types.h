#ifndef __config_h
#define __config_h

#include "id.h"
#include "adapter.h"

class Default_Config
{
public:
  static const bool needs_allocator = false;
  static const bool needs_sharing = false;
  static const bool needs_atomizer = false;
  static const bool needs_remoter = false;
};

template <class Implementor>
class Config: public Default_Config
{
public:
  static const Type type;
  static const int units = 0;
  static void * const table = 0;
};

class Member_Imp;

template 
class Config<Member_Imp>
{
public:
  static const Type type = 1;
  static const int units = 0;
  static Adapter<int> * const table = 0;
};


//const Type Config<Member_Imp>::type = 1;

#endif
