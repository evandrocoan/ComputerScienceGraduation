/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */

/* 
 * File:   Traits.h
 * Author: cancian
 *
 * Created on 27 de Setembro de 2016, 20:57
 */

#ifndef TRAITS_H
#define TRAITS_H

#include "AbstractionInterface.h"



    template<typename T>
    struct Traits {
        static const bool enabled = true;
    };

    template<> struct Traits<AbstractionInterface> {
        static constexpr bool doublePrecision = false;
        static constexpr bool isRemote = false;
        static constexpr int aspecto = 0;
        static constexpr int abstraction = 2;
    };


#endif /* TRAITS_H */

