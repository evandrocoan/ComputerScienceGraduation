/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */

/* 
 * File:   Tester.h
 * Author: cancian
 *
 * Created on 27 de Setembro de 2016, 17:41
 */

#ifndef TESTER_H
#define TESTER_H

#include "FractionNumber.h"
#include "Templater.h"
#include "Traits.h"
#include "Abstraction.h"

#include <string>
#include <iostream>

class Tester {
public:
    Tester();
    Tester(const Tester& orig);
    virtual ~Tester();
private:
public:

    static void TestOperatorOverriding() {
        FractionNumber a(4, 6);
        FractionNumber b(1, 3);
        // without operator overriding
        FractionNumber c = a.add(b);
        FractionNumber d = a.sub(b).add(c);
        std::cout << "c:" << std::to_string(c.evaluate()) << "\td:" << std::to_string(d.evaluate()) << std::endl;
        if (c.isLessThan(d)) {
            std::cout << "c is less than d" << std::endl;
        }

        // with operator overriding
        FractionNumber c2 = a + b;
        FractionNumber d2 = a - b + c;
        std::cout << "c:"<< c2 << "\td:" << d2 << std::endl;
        if (c < d) {
            std::cout << "c is less than d" << std::endl;
        }
    }

    static void TestBasicClassTemplate() {
        int a1 = 2, b1 = 3;
        double a2 = 2.5, b2 = 3.5;
        FractionNumber a3(2, 5), b3(3, 5);

        std::cout << "a1+b1:" <<std::to_string(add(a1, b1)) << "\t\ta2+b2:" << std::to_string(add(a2, b2)) << "\t\ta3+b3:" << add(a3, b3) << std::endl;
    }

    static void TestSolveCompileTime() {
        // EXAMPLE 2
        std::cout << "Fat(6):" << Factorial<6>::result << std::endl;

        // EXAMPLE 3
        double result = FractionNumberEvaluate<FractionNumberAdd<FractionNumberTemplate<2, 1>, FractionNumberTemplate < 1, 3 >> ::result>::result;
        std::cout << "2/1 + 1/3:" <<  std::to_string(result) << std::endl;
    }

    static void TestMetaIfThenElse() {
        //EXAMPLE 4
        typedef IF<Traits<AbstractionInterface>::doublePrecision, double, float>::Result doubleOrFloatVar;
        std::cout << "SizeofVar: " << std::to_string(sizeof (doubleOrFloatVar)) << std::endl;
    }

    static void TestFramework() {
        // Example 6 (Adapter + framework)
        AbstractionInterface* abstraction = new Abstraction();
        abstraction->operation();
        
        /*AbstractionInterface* abstraction2 = new Abstraction();
        abstraction2->operation();*/

    }
};

#endif /* TESTER_H */

