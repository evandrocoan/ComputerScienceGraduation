/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */

/* 
 * File:   Abstraction.cpp
 * Author: cancian
 * 
 * Created on 27 de Setembro de 2016, 21:50
 */

#include "Abstraction_OS.h"

#include <iostream>

namespace OS {

    Abstraction::Abstraction() {
    }

    Abstraction::Abstraction(const Abstraction& orig) {
    }

    Abstraction::~Abstraction() {
    }

    void Abstraction::operation() {
        std::cout << "void Abstraction_OS::operation()" << std::endl;
    }

}