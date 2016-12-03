

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



a = 1
b = 6
printf( "\n" )

# Problem 1 integral
integralAtIntervalab = log( b+1 ) - log( a+1 );


printf( "\nSimpson's degree 1\n" )
n = 2000

integralAtIntervalab
aproximateIntegral__ = problem1SimpsonNumericIntegralDegree1( n, a, b )
estimatedIntegral___ = problem1SimpsonNumericIntegralDegree1( 2*n, a, b )

printf( "\n" )

erroExato___ = abs( integralAtIntervalab - aproximateIntegral__ )
erroEstimado = abs( aproximateIntegral__ - estimatedIntegral___ )


#
# For the Simpson's degree 2, the n intervals must to be even.
# n             = even;
# h             = (b-a)/n;
# intevalsCount = numel( h );
#
# The each one of the Simpson's Area is named by its middle point. Example:
# x1, x2, x3, x4, x5, x6, x7
#     A2,     A4,     A6
#
# A2 = \integral_x1^x3 = h/3 * ( y(1) + 4*y(2) + y(3) )
# A4 = \integral_x1^x3 = h/3 * ( y(3) + 4*y(3) + y(4) )
# A6 = \integral_x1^x3 = h/3 * ( y(5) + 4*y(6) + y(7) )
#

printf( "\nSimpson's degree 2\n" )
n = 60

integralAtIntervalab
aproximateIntegral__ = problem1SimpsonNumericIntegralDegree2( n, a, b )
estimatedIntegral___ = problem1SimpsonNumericIntegralDegree2( 2*n, a, b )

printf( "\n" )

erroExato___ = abs( integralAtIntervalab - aproximateIntegral__ )
erroEstimado = abs( aproximateIntegral__ - estimatedIntegral___ )



