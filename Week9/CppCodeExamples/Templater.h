/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */

/* 
 * File:   Templater.h
 * Author: cancian
 *
 * Created on 27 de Setembro de 2016, 18:00
 */

#ifndef TEMPLATER_H
#define TEMPLATER_H

// EXAMPLE 1

template<typename T>
static T add(T x, T y) {
    return x + y;
}


// EXAMPLE 2

template <int N> struct Factorial {
    static const int result = N * Factorial<N - 1 > ::result;
};

template <> struct Factorial<0> {
    static const int result = 1;
};


// EXAMPLE 3

template <long num, long den> struct FractionNumberTemplate {
    static const long numerator = num;   
    static const long denominator = den;
};

template<typename FN> struct FractionNumberEvaluate {
    static constexpr double result = ((double)FN::numerator)/((double)FN::denominator);
}; 

template <typename FN1, typename FN2> struct FractionNumberAdd {
    typedef FractionNumberTemplate<FN1::numerator*FN2::denominator + FN2::numerator*FN1::denominator, FN1::denominator*FN2::denominator> result;
};


// EXAMPLE 4

template<bool condition, typename Then, typename Else>
struct IF
{ typedef Then Result; };

template<typename Then, typename Else>
struct IF<false, Then, Else>
{ typedef Else Result; };


// EXAMPLE 5

template<bool condition, int Then, int Else>
struct IF_INT
{ enum { Result = Then }; };

template<int Then, int Else>
struct IF_INT<false, Then, Else>
{ enum { Result = Else }; };

#endif /* TEMPLATER_H */

