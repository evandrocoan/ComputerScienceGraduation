//============================================================================
// ABSTRACTION PROTECTED ID IMPLEMENTOR
//
// Desc: 
//
// Date: 10 Dec 1999			Auth: Guto
//============================================================================
#ifndef __abstraction_id_protected_h
#define __abstraction_id_protected_h 1

//============================================================================
// DEPENDENCIES
//============================================================================
#include <system/debug.h>
#include <system/abstraction_id/global.h>

BEGIN_SYSTEM_NAMESPACE

//============================================================================
// PROTECTED_ID 
//============================================================================
class Protected_Id_Imp: public Global_Id_Imp
{
public:
  Protected_Id_Imp() {}
  Protected_Id_Imp(Abstraction_Type t, Abstraction_Unit u = ANY_UNIT,
		   Abstraction_Rights r = ALL_RIGHTS):
    Global_Id_Imp(t, u), rights(r), check(8) {}
    
  bool grant(Abstraction_Type t, Abstraction_Rights r = ALL_RIGHTS) const
    {return ((rights & r) && Global_Id_Imp::grant(t));}

Abstraction_Rights get_rights() const {return rights;}
Abstraction_Check get_check() const {return check;}
    
  const char *dump() const
    {return debug_dump("{h=%d,t=%d,u=%d,r=%x,c=%x}", get_host(), get_type(),
		       get_unit(), rights, check);}
    
private:
  Abstraction_Rights rights;
  Abstraction_Check check;
};

END_SYSTEM_NAMESPACE

#endif // __abstraction_id_protected_h
