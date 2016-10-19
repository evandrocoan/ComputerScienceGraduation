/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */

/* 
 * File:   Handle.cpp
 * Author: cancian
 * 
 * Created on 27 de Setembro de 2016, 22:45
 */

#include "Handle.h"
#include <iostream>
namespace OS {
Handle::Handle(Stub* stub) {
    this->stub = stub;
}

Handle::Handle(const Handle& orig) {
}

Handle::~Handle() {
}

void Handle::operation() {
    std::cout << "void Handle::operation()" << std::endl;
    this->stub->operation();
}

}