

#pragma once

#include <iostream>
#include "abstraction.h"


namespace OS
{


    class Proxy: public AbstractionInterface
    {
    public:
        Proxy()
        {
            
        }
        
        void operation()
        {
            std::cout << "OS::IMPLEMENTATION is Calling void operation()" << std::endl;
        }
    };

}
