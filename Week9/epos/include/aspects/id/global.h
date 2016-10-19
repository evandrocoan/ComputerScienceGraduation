#ifndef __global_id_h
#define __global_id_h

#include "local.h"

#include <node.h>

extern Node *node;

__BEGIN_SYS
__BEGIN_IMP

class Global_Id: public Local_Id
{
public:
  Global_Id(const Type & t, const Unit & u) :
    Local_Id(t, u), host(node->get_node_id()){}
    
protected:
  Global_Id(): host(node->get_node_id()) {}
    
private:
  Node_Id host;
};

__END_IMP
__END_SYS

#endif
