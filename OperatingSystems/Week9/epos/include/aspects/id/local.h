#ifndef __local_id_h
#define __local_id_h

#include "pointer.h"

__BEGIN_SYS
__BEGIN_IMP

class Local_Id
{
public:
  Local_Id(const Type & t, const Unit & u): _type(t), _unit(u) {}

  const Local_Id & id() const { return *this; }
  bool valid() const { return (_unit != ANY_UNIT); }
//  const Type & type() const { return _type; }
//  const Unit & unit() const { return _unit; }

protected:
  Local_Id() { dbwrn << "Warning: empty id!\n"; } // for new Scenario

  void id(const Type & t, const Unit & u) { _type = t; _unit = u; }

private:
  Type _type;
  Unit _unit;
};

__END_IMP
__END_SYS

#endif
