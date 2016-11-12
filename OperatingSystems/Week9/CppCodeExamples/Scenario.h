/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */

/* 
 * File:   Scenario.h
 * Author: cancian
 *
 * Created on 27 de Setembro de 2016, 22:53
 */

#ifndef SCENARIO_H
#define SCENARIO_H

namespace OS {

    class Scenario {
    public:
        Scenario();
        Scenario(const Scenario& orig);
        virtual ~Scenario();
    public:
        void enter();
        void leave();
    private:

    };
}
#endif /* SCENARIO_H */

