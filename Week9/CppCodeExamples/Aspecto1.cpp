
#include "Aspecto1.h"

#include <iostream>
namespace OS {
Aspecto1::Aspecto1() {
}

Aspecto1::Aspecto1(const Aspecto1& orig) {
}

Aspecto1::~Aspecto1() {
}

void Aspecto1::enter() {
    std::cout << "void Aspecto1::enter()" << std::endl;
}

void Aspecto1::leave() {
    std::cout << "void Aspecto1::leave()" << std::endl;
}
}
