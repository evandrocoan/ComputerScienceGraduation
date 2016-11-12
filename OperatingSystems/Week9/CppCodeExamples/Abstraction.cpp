/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */

/* 
 * File:   Abstraction.cpp
 * Author: cancian
 * 
 * Created on 27 de Setembro de 2016, 23:17
 */

#include "Abstraction.h"

#include "Handle.h"
#include "Stub.h"

#include <iostream>

Abstraction::Abstraction() {
    handle = new OS::Handle(new OS::Stub());
}

Abstraction::Abstraction(const Abstraction& orig) {
}

Abstraction::~Abstraction() {
}

void Abstraction::operation() {
    std::cout << "void Abstraction::operation()" << std::endl;
    this->handle->operation();
}
