
#include "Aspecto2.h"

#include <iostream>
namespace OS {
Aspecto2::Aspecto2() {
}

Aspecto2::Aspecto2(const Aspecto2& orig) {
}

Aspecto2::~Aspecto2() {
}

void Aspecto2::enter() {
    std::cout << "void Aspecto2::enter()" << std::endl;
}

void Aspecto2::leave() {
    std::cout << "void Aspecto2::leave()" << std::endl;
}
}
