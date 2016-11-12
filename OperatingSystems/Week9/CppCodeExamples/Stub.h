/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */

/* 
 * File:   Stub.h
 * Author: cancian
 *
 * Created on 27 de Setembro de 2016, 22:08
 */

#ifndef STUB_H
#define STUB_H

#include "Adapter.h"
#include "Proxy.h"
#include "Templater.h"
#include "Traits.h"

namespace OS {

    class Stub : public IF<Traits<AbstractionInterface>::isRemote, Proxy, Adapter>::Result {
    public:
        Stub();
        Stub(const Stub& orig);
        virtual ~Stub();
    private:
        typedef IF<Traits<AbstractionInterface>::isRemote, Proxy, Adapter>::Result BaseClass;
    public:
        void operation();
    };
}
#endif /* STUB_H */

