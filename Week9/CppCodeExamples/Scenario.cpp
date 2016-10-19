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

#include "Scenario.h"
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
}

void Scenario::leave() {
    std::cout << "void Scenario::leave()" << std::endl;
}
}