

clc
clear
close all

more off
format long
split_long_rows(0)

#format rat
#output_precision(30)
#output_max_field_width(0)

#{
printf( "\nQuestão 1.a\n" )

a = 0
b = 1
# printf( "\n" )

# Problem 1 integral
integralAtIntervalab = erf(1);


printf( "\nSimpson's degree 1\n" )
n = 2700

integralAtIntervalab
aproximateIntegral__ = problem1SimpsonNumericIntegralDegree1( n  , a, b )
estimatedIntegral___ = problem1SimpsonNumericIntegralDegree1( 2*n, a, b )

printf( "\n" )

erroExato___ = abs( integralAtIntervalab - aproximateIntegral__ )
erroEstimado = abs( aproximateIntegral__ - estimatedIntegral___ )
#





#
printf( "\n\n\nQuestão 1.b\n" )

a = 0
b = 1
# printf( "\n" )

# Problem 1 integral
integralAtIntervalab = erf(1);

printf( "\nSimpson's degree 2\n" )
n = 32

integralAtIntervalab
aproximateIntegral__ = problem1SimpsonNumericIntegralDegree2( n  , a, b )
estimatedIntegral___ = problem1SimpsonNumericIntegralDegree2( 2*n, a, b )

printf( "\n" )

erroExato___ = abs( integralAtIntervalab - aproximateIntegral__ )
erroEstimado = abs( aproximateIntegral__ - estimatedIntegral___ )

#




#
printf( "\n\n\nQuestão 1.c\n" )

a = 0
b = 1
printf( "\n" )

# GaussLegendre
#
# It is useful when/ALWAYS USE this Integral when its is not continue on its interval
# extremities as [0,1] for 1/x, which does not converge.
#
m = 5
printf( "\n" )

# Problem 1 integral
integralAtIntervalab_ = erf(1);


gaussLegendreIntegralAproximate = problem1GaussLegendreIntegral( m  , a, b );
gaussLegendreIntegralEstimado__ = problem1GaussLegendreIntegral( m*2, a, b );

erroEstimado = abs( gaussLegendreIntegralAproximate - gaussLegendreIntegralEstimado__ );
erroExato___ = abs( gaussLegendreIntegralAproximate - integralAtIntervalab_ );

integralAtIntervalab_
gaussLegendreIntegralAproximate

printf( "\n" )

erroExato___
erroEstimado

# printf( "\n" )


#}



#{
printf( "\n\n\nQuestão 2.\n\n" )


Coef_6 = 4.72935285881143e-03;

# Numero de nós de Chebyshev, i.e., grau da precisão numérica da integral.
m = 10

# Calcule o segundo 3º coeficiente da serie de Chebyshev de f(x) = cos(x)
#
#     b(2) = (2/pi)*integral( f(x)*T(1)) / sqrt(1-x^2) ) dx
#
# b(2) multiplica T(2) `Polinômio` de Chebyshev. T(2) = x^2 - 1;
# Find out how to create a function here and pass it. The @cos here is supposed to be `cos(x)*x`.
#
b6____ = ( 2/pi ) * problem1GaussChebyshevIntegral( m, @erf, 5 );


erroExato = abs( b6____ - Coef_6 );

b6____
Coef_6

printf( "\n" )
erroExato


#}



printf( "\n\n\nQuestão 3.\n" )



printf( "\n" )





