#pragma once

#include <iostream>
#include "abstraction.h"
#include "stub.h"



class Abstraction: public AbstractionInterface
{
public:
    Abstraction()
    {
        this->stub = new OS::Stub();
    }
    
    void operation()
    {
        std::cout << "USER::IMPLEMENTATION is Calling void operation()" << std::endl;
        this->stub->operation();
    }
    
private:
    OS::Stub* stub;
    
};

