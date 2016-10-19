/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */

/* 
 * File:   Abstraction.h
 * Author: cancian
 *
 * Created on 27 de Setembro de 2016, 23:17
 */

#ifndef ABSTRACTION_H
#define ABSTRACTION_H

#include "AbstractionInterface.h"

class Abstraction : public AbstractionInterface {
public:
    Abstraction();
    Abstraction(const Abstraction& orig);
    virtual ~Abstraction();
public:
    void operation();
private:
    AbstractionInterface* handle;
};

#endif /* ABSTRACTION_H */

