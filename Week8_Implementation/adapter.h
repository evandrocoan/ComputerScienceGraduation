

#pragma once

#include <iostream>
#include "AbstractionInterface.h"

#include "abstraction.h"
#include "scenario.h"


namespace OS
{


    class Adapter: public Abstraction, public Scenario
    {
    public:
        Adapter()
        {
            
        }
        
        void operation()
        {
            std::cout << "OS::IMPLEMENTATION is Calling void Adapter()" << std::endl;
            
            this->Scenario::enter();
            this->Abstraction::operation();
            this->Scenario::leave();
        }
    };

}
