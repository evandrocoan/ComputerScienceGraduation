/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */

/* 
 * File:   Scenario.cpp
 * Author: cancian
 * 
 * Created on 27 de Setembro de 2016, 22:53
 */

#include "Traits.h"
#include "Scenario.h"

#include "Aspecto1.cpp"
#include "Aspecto2.cpp"

#include <iostream>

namespace OS {
Scenario::Scenario() {
}

Scenario::Scenario(const Scenario& orig) {
}

Scenario::~Scenario() {
}

void Scenario::enter() {
    std::cout << "void Scenario::enter()" << std::endl;
    
    switch( Traits < AbstractionInterface >::aspecto )
    {
        case 1:
        {
            Aspecto1* aspecto1 = new Aspecto1();
            aspecto1->enter();
            break;
        }
        case 2:
        {
            Aspecto2* aspecto2 = new Aspecto2();
            aspecto2->enter();
            break;
        }
    }
}

void Scenario::leave() {
    std::cout << "void Scenario::leave()" << std::endl;
    
    switch( Traits < AbstractionInterface >::aspecto )
    {
        case 1:
        {
            Aspecto1* aspecto1 = new Aspecto1();
            aspecto1->leave();
            break;
        }
        case 2:
        {
            Aspecto2* aspecto2 = new Aspecto2();
            aspecto2->leave();
            break;
        }
    }
}
}
