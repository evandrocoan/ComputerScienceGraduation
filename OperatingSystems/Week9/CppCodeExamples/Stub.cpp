/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */

/* 
 * File:   Stub.cpp
 * Author: cancian
 * 
 * Created on 27 de Setembro de 2016, 22:08
 */

#include "Stub.h"
#include <iostream>
namespace OS {

    Stub::Stub() {
    }

    Stub::Stub(const Stub& orig) {
    }

    Stub::~Stub() {
    }

    void Stub::operation() {
        std::cout << "void Stub::operation()" << std::endl;
        this->BaseClass::operation();
    }
}