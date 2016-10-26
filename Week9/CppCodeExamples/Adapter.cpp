/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */

/* 
 * File:   Adapter.cpp
 * Author: cancian
 * 
 * Created on 27 de Setembro de 2016, 22:05
 */

#include "Traits.h"
#include "Adapter.h"

#include <iostream>

/*
template< bool condition, typename Then > struct IF
{
    Then;
};


template< typename Then, typename Else > struct IF< false, Then >
{
    
};*/

namespace OS {

    Adapter::Adapter() {
    }

    Adapter::Adapter(const Adapter& orig) {
    }

    Adapter::~Adapter() {
    }

    void Adapter::operation() {
        std::cout << "void Adapter::operation()" << std::endl;
        
        
        // define to tipo da variavel em tempo de compilação dependendo do arquivo de configuração
        // Traits. Muito melhor do que ficar fazendo #ifdef... etc.
        //IF< Traits < AbstractionInterface >::aspecto, this->Scenario::enter() >;
        
    #if defined ASPECTO_MACRO
        this->Scenario::enter();
    #endif
        
        this->Abstraction::operation();
        
    #if defined ASPECTO_MACRO
        this->Scenario::leave();
    #endif
        
        //IF< Traits < AbstractionInterface >::aspecto, this->Scenario::leave() >;
    }
}
