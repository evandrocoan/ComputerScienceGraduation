/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */

/* 
 * File:   FractionNumber.h
 * Author: cancian
 *
 * Created on 27 de Setembro de 2016, 16:36
 */

#ifndef FRACTIONNUMBER_H
#define FRACTIONNUMBER_H

#include <cmath>
#include <iostream>

class FractionNumber {
public:

    FractionNumber(long numerator, long denominator) {
        this->numerator = numerator;
        this->denominator = denominator;
        //simplify fraction
        long gcd = Gcd(this->numerator, this->denominator);
        this->numerator /= gcd;
        this->denominator /= gcd;
    }

    FractionNumber(const FractionNumber& orig) {
        this->numerator = orig.numerator;
        this->denominator = orig.denominator;
    }

    virtual ~FractionNumber() {

    }

public:

    FractionNumber add(FractionNumber& other) {
        long mmc = this->denominator * other.denominator;
        FractionNumber resp(this->numerator * mmc / this->denominator + other.numerator * mmc / other.denominator, mmc);
        return resp;
    };

    FractionNumber sub(FractionNumber& other) {
        long mmc = this->denominator * other.denominator;
        FractionNumber resp(this->numerator * mmc / this->denominator - other.numerator * mmc / other.denominator, mmc);
        return resp;
    }

    const bool isLessThan(FractionNumber& other) {
        return this->evaluate() < other.evaluate();
    }

    double evaluate() {
        return ((double) numerator) / ((double) denominator);
    }

public: // operator override

    FractionNumber &operator+(const FractionNumber& other) {
        long mmc = this->denominator * other.denominator;
        FractionNumber resp(this->numerator * mmc / this->denominator + other.numerator * mmc / other.denominator, mmc);
        return resp;
    }

    FractionNumber &operator-(const FractionNumber& other) {
        long mmc = this->denominator * other.denominator;
        FractionNumber resp(this->numerator * mmc / this->denominator - other.numerator * mmc / other.denominator, mmc);
        return resp;
    }

    friend std::ostream& operator<<(std::ostream& out, const FractionNumber& other) {
        return out << other.numerator << "/" << other.denominator;
    }

    const bool &operator<(FractionNumber& other) {
        double a = this->evaluate();
        double b = other.evaluate();
        return a < b;
    }
private:

    long Gcd(long num, long den) {
        if (den == 0) {
            return num;
        }
        return Gcd(den, fmod(num, den));
    }

private:
    long numerator;
    long denominator;


public:

};

#endif /* FRACTIONNUMBER_H */

