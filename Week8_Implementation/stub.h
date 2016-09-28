

#pragma once

#include <iostream>
#include "abstraction.h"

#include "traits.h"
#include "meta.h"
#include "proxy.h"
#include "adapter.h"


namespace OS
{
    // aqui temos uma heranca alternativa, dependendo do traits.h, resolvido em tempo de compilação
    class Stub: public IF< Traits < AbstractionInterface >::isRemote, Proxy, Adapter >::Result
    {
    public:
        Stub()
        {
            
        }
        
        void operation()
        {
            std::cout << "Stub OS::IMPLEMENTATION is Calling void operation()" << std::endl;
            
            // chamar a super classe
            this->SuperClass::operation();
        }
        
    private:
        typedef IF< Traits < AbstractionInterface >::isRemote, Proxy, Adapter >::Result SuperClass;
        
    };

}
