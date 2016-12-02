

clc
clear
close all

more off
format long
split_long_rows(0)

#format rat
#output_precision(30)
#output_max_field_width(0)

# addpath( 'Maclaurin' )


#
# \int GM(m), GM(m) = (b-a)/2 \sum_i=1^m C(m,i) * y(i)
#
# GM GaussLegendre integra exatamente polinômios de grau ( 2*m-1 )
#
#     The same as the ChebyshevDomainLinearTransformationIn.m
#     Se m = 2 --> P0, P1, P2, P3
#
#     For x \in [a, b] and t \in [-1, 1]
#     x(i) = (b-a)/2*t(m,i) + (b+a)/2
#


a = 1
b = 6
printf( "\n" )

# Problem 1 integral
integralAtIntervalab_ = log( b+1 ) - log( a+1 );


# GaussLegendre
m = 3

gaussLegendreIntegral = problem1GaussLegendreIntegral( m, a, b );


integralAtIntervalab_
gaussLegendreIntegral

printf( "\n" )
printf( "\n" )


# GaussChebychev's Integral
#

# Sempre:
a = -1
b =  1

m = 500

# Here we will use f(x) as sin(x). The GaussChebychev integral is always on the from:
#
#     \int _-1 ^1 f(x)/( sqrt(1-x^2) ) dx
#
gaussChebyshevIntegralExacly_____ = problem1GaussChebyshevIntegral( m  , @sin );
gaussChebyshevIntegralApproximate = problem1GaussChebyshevIntegral( m*2, @sin );


erroEstimado = abs( gaussChebyshevIntegralExacly_____ - gaussChebyshevIntegralApproximate )


