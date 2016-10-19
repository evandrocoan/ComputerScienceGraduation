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

template< int condition, typename Then, typename Else > struct IF
{
    typedef Else Result;
};


template< typename Then, typename Else > struct IF< 1, Then, Else >
{
    typedef Then Result;
};

template< typename Then, typename Else > struct IF< 2, Then, Else >
{
    typedef Else Result;
};


namespace OS {
Scenario::Scenario() {
}

Scenario::Scenario(const Scenario& orig) {
}

Scenario::~Scenario() {
}

void Scenario::enter() {
    std::cout << "void Scenario::enter()" << std::endl;
    
    /*switch( Traits < AbstractionInterface >::aspecto )
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
    }*/
    
    IF< Traits < AbstractionInterface >::aspecto, Aspecto1, Aspecto2 >::Result* aspecto = 
        new IF< Traits < AbstractionInterface >::aspecto, Aspecto1, Aspecto2 >::Result();
    
    aspecto->enter();
}

void Scenario::leave() {
    std::cout << "void Scenario::leave()" << std::endl;
    
    /*switch( Traits < AbstractionInterface >::aspecto )
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
    }*/
    
    IF< Traits < AbstractionInterface >::aspecto, Aspecto1, Aspecto2 >::Result* aspecto = 
        new IF< Traits < AbstractionInterface >::aspecto, Aspecto1, Aspecto2 >::Result();
    
    aspecto->leave();
}
}
