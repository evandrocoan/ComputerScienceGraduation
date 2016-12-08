

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

# GaussLegendre
#
# It is useful when/ALWAYS USE this Integral when its is not continue on its interval
# extremities as [0,1] for 1/x, which does not converge.
#
m = 16
printf( "\n" )

# Problem 1 integral
integralAtIntervalab_ = log( b+1 ) - log( a+1 );
gaussLegendreIntegral = problem1GaussLegendreIntegral( m, a, b );

erroExato = abs( integralAtIntervalab_ - gaussLegendreIntegral );

integralAtIntervalab_
gaussLegendreIntegral

erroExato

printf( "\n" )
# printf( "\n" )

t = [  0        , 0        , 0        ;
      -1/sqrt(3), 1/sqrt(3), 0        ;
      -sqrt(3/5), 0        , sqrt(3/5); ...
    ];

C = [ 2  , 0  , 0   ;
      1  , 1  , 0   ;
      5/9, 8/9, 5/9 ; ...
    ];

# GaussChebychev's Integral
#
# This Integral only to works within the interval [-1, 1], and its use is only to find out the
# Chebyshev b's coefficients.
#
# Sempre:
a = -1
b =  1

m = 5000

# Here we will use f(x) as sin(x). The GaussChebychev integral is always on the from:
#
#     \int _-1 ^1 f(x)/( sqrt(1-x^2) ) dx
#
# gaussChebyshevIntegralApproximate = problem1GaussChebyshevIntegral( m  , @sin );
# gaussChebyshevIntegralExacly_____ = problem1GaussChebyshevIntegral( m*2, @sin );


gaussChebyshevIntegralApproximate = problem1GaussChebyshevIntegral( m  , @fLog1 );
gaussChebyshevIntegralEstimated__ = problem1GaussChebyshevIntegral( m*2, @fLog1 );

gaussChebyshevIntegralExactly____ = -pi*log(2);


printf( "\n" )
erroEstimado = abs( gaussChebyshevIntegralEstimated__ - gaussChebyshevIntegralApproximate );
erroExato___ = abs( gaussChebyshevIntegralExactly____ - gaussChebyshevIntegralApproximate );

gaussChebyshevIntegralExactly____
gaussChebyshevIntegralEstimated__
gaussChebyshevIntegralApproximate

printf( "\n" )
erroEstimado
erroExato___

printf( "\n" )
# printf( "\n" )


# Sempre use o intervalo [-1,1] para calcular os b's Coeficientes de Chebyshev:
a = -1
b =  1

# Numero de nós de Chebyshev, i.e., grau da precisão numérica da integral.
m = 500


# Calcule o primeiro 1º coeficiente da serie de Chebyshev de f(x) = cos(x)
#
#     b(0) = (1/pi)*integral( f(x)*T(0)) / sqrt(1-x^2) ) dx
#
# b(0) multiplica T(0) `Polinômio` de Chebyshev. T(0) = 1;
#
b0 = ( 1/pi ) * problem1GaussChebyshevIntegral( m, @cos, 0 )


# Calcule o segundo 2º coeficiente da serie de Chebyshev de f(x) = cos(x)
#
#     b(1) = (2/pi)*integral( f(x)*T(1)) / sqrt(1-x^2) ) dx
#
# b(1) multiplica T(1) `Polinômio` de Chebyshev. T(1) = x;
#
# Find out how to create a function here and pass it. The @cos here is supposed to be `cos(x)*x`.
#
b1 = ( 2/pi ) * problem1GaussChebyshevIntegral( m, @cos, 1 )


# Calcule o segundo 3º coeficiente da serie de Chebyshev de f(x) = cos(x)
#
#     b(2) = (2/pi)*integral( f(x)*T(1)) / sqrt(1-x^2) ) dx
#
# b(2) multiplica T(2) `Polinômio` de Chebyshev. T(2) = x^2 - 1;
#
# Find out how to create a function here and pass it. The @cos here is supposed to be `cos(x)*x`.
#
b2 = ( 2/pi ) * problem1GaussChebyshevIntegral( m, @cos, 2 )



