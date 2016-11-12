#ifndef __pointer_id_h
#define __pointer_id_h

#include <aspects/id.h>
#include "common.h"

__BEGIN_SYS
__BEGIN_IMP

class Pointer_Id: protected Id_Common
{
protected:
  Pointer_Id() {}
  Pointer_Id(const Pointer_Id *) {}

public:
  Pointer_Id id() const { return (Pointer_Id)this; }
  bool valid() const { return this; }
};

__END_IMP
__END_SYS

#endif
