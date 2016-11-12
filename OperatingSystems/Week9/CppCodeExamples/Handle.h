/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */

/* 
 * File:   Handle.h
 * Author: cancian
 *
 * Created on 27 de Setembro de 2016, 22:45
 */

#ifndef HANDLE_H
#define HANDLE_H

#include "AbstractionInterface.h"
#include "Stub.h"
namespace OS {

    class Handle : public AbstractionInterface {
    public:
        Handle(Stub* stub);
        Handle(const Handle& orig);
        virtual ~Handle();
    public:
        void operation();
    private:
        Stub* stub;
    };
}
#endif /* HANDLE_H */

