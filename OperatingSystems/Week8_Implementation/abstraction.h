

#pragma once

#include <iostream>
#include "AbstractionInterface.h"


namespace OS
{
    class Abstraction: public AbstractionInterface
    {
    public:
        Abstraction()
        {
            
        }
        
        void operation()
        {
            std::cout << "OS::IMPLEMENTATION is Calling void operation()" << std::endl;
        }
    };

}
