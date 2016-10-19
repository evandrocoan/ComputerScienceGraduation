/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */

/* 
 * File:   Proxy.cpp
 * Author: cancian
 * 
 * Created on 27 de Setembro de 2016, 22:06
 */

#include "Proxy.h"
#include <iostream>

namespace OS {
Proxy::Proxy() {
}

Proxy::Proxy(const Proxy& orig) {
}

Proxy::~Proxy() {
}

void Proxy::operation() {
    std::cout << "void Proxy::operation()" << std::endl;
}
}