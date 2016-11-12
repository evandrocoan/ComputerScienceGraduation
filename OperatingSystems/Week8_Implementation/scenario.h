

#pragma once

#include <iostream>
#include "abstraction.h"


namespace OS
{


    class Scenario
    {
    public:
        Scenario()
        {
            
        }
        
        void enter()
        {
            std::cout << "void OS::Scenario::enter()" << std::endl;
        }
        
        void leave()
        {
            std::cout << "void OS::Scenario::leave()" << std::endl;
        }
    };

}
