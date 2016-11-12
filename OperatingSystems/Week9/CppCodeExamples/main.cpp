/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */

/* 
 * File:   main.cpp
 * Author: cancian
 *
 * Created on 27 de Setembro de 2016, 16:34
 */

#include <cstdlib>
#include <string>
#include <iostream>

#include "Tester.h"

/**
 * To run it do: reset;make;./dist/Debug/GNU-Linux/cppcodeexamples
 * If needed do this on the main root folder CppCodeExamples: chmod 777 *
 */
int main(int argc, char** argv) {
    int test = 4;
    switch (test) {
        case 0:
            Tester::TestOperatorOverriding();
            break;
        case 1:
            Tester::TestBasicClassTemplate();
            break;
        case 2:
            Tester::TestSolveCompileTime();
            break;
        case 3:
            Tester::TestMetaIfThenElse();
            break;
        case 4:
            Tester::TestFramework();
            break;
    }
    return 0;
}

