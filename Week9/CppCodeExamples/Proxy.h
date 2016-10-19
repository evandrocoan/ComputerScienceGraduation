/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */

/* 
 * File:   Proxy.h
 * Author: cancian
 *
 * Created on 27 de Setembro de 2016, 22:06
 */

#ifndef PROXY_H
#define PROXY_H

#include "AbstractionInterface.h"

namespace OS {

    class Proxy : public AbstractionInterface {
    public:
        Proxy();
        Proxy(const Proxy& orig);
        virtual ~Proxy();
    public:
        void operation();
    private:

    };
}
#endif /* PROXY_H */

