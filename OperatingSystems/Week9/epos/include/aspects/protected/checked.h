#ifndef __checked_h
#define __checked_h

#include <stdlib.h>

__BEGIN_SYS

class Checked
{
protected:
  Checked(): check(rand()) { dbtrc << "Checked()\n"; }
  bool grant(const Id & id) { dbtrc << "Checked::grant()\n"; return true; }

private:
  int check;
};

__END_SYS

#endif
