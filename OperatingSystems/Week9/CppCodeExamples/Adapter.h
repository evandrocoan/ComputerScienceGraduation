/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */

/* 
 * File:   Adapter.h
 * Author: cancian
 *
 * Created on 27 de Setembro de 2016, 22:05
 */

#ifndef ADAPTER_H
#define ADAPTER_H

#include "Abstraction_OS.h"
#include "Scenario.h"

namespace OS {

    class Adapter : public Abstraction
    
    #if defined ASPECTO_MACRO
    , public Scenario
    #endif
    
    {
    public:
        Adapter();
        Adapter(const Adapter& orig);
        virtual ~Adapter();
    public:
        void operation();
    private:

    };

}

#endif /* ADAPTER_H */

