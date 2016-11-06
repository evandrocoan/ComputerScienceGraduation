

clc
clear
close all

more off
format long
split_long_rows(0)

# Derivatives
#
# ( u^n )' = n*u^(n-1)*u'   <-- Chain rule, the external derivative times the internal derivative.
#
# To create the MacLaurin coefficients we need to derivate several times aways applying them on the
# 0 point, because that is the MacLaurin Series, the Taylor series applied on the zero point.
#
# 1. Always to transform the domain from [a, b] to [-1, +1] using this formula:
#    x(t) = 0.5*( b-a )*t + 0.5*( b+a )
#
# Now we got f( x(t) ) with t on [-1, 1]. This is useful/necessary to the Chebyshev Series.
# And we always to apply the derivatives on this new domain [-1, +1] at the point 0, to deduce
# the nth derivate formula at the point 0 by backtracking.
# Como vamos padronizar o domínio [a, b] da aproximado para [-1, 1], pode-se fixar o x da série em 0.
#

# Linear transformation to convert the [a, b] domain to [-1, 1] domain.
# We may call it as `t(x)`.
#
function t = MaclaurinLinearTransformationDomainIn( x, a, b )

    t = ( 2*x - (b+a) ) / ( b-a );

end

# Linear transformation to convert the [-1, 1] domain to [a, b] domain.
#
# We may call it as `x(t)`. On this way, we apply the the approximation
# methods to the `f(x(t))`, were `x` belongs to the Domain [a, b].
#
function x = MaclaurinLinearTransformationDomainOut( t, a, b )

    x = ( (b-a)*t + (b+a) ) / 2;

end

#
# Function: log( x )
# For the Domain [-1, 1]
# MaclaurinSeries( 0 ) = f( 0 ) + (f'( 0 )*z^1) / 1! + (f''( 0 )*z^2) / 2! + ... + (f^n'( 0 )*z^n) / n!
#
# For the Domain [a, b]
# MaclaurinSeries( 0 ) = f( 0 )
#                        + (f'( 0 )*z^1) / 1!
#                        + (f''( 0 )*z^2) / 2!
#                        + ...
#                        + (f^n'( 0 )*z^n) / n!
#
function coef = fMaclaurinForLog( n, a, b )

    # Here we applicate the first coefficient from the Maclaurin series on its Domain [-1, 1]
    # middle point 0. Before apply our function `fLog` we need to convert from the Domain [-1, 1]
    # to the original or correct Domain [a, b] for the function `fLog`.
    # This is the whole reason why we may apply the derivative functions on the 0 point.
    MaclaurinDomainPointZero = MaclaurinLinearTransformationDomainOut( 0, a, b )

    cache = ( b - a ) / ( b + a )
    coef( 1 ) = fLog( MaclaurinDomainPointZero )

    for i = 2 : n + 1

        coef( i ) =  (-1)^(i) * (cache^(i-1)) / (i-1);

    end

end

#{
n = 7
a = 1
b = 2
h = (b-a)/n

coefMaclaurin = fMaclaurinForLog( n, a, b )

x = a : h : b
y = fLog( x )

xInterPontos = a : h/20 : b
yInterPontos = fLog( xInterPontos )

tInterPontos = MaclaurinLinearTransformationDomainIn( xInterPontos, a, b )
yAproximado  = fPnPorBriotRunifi( n, coefMaclaurin, tInterPontos )

# Erro máximo deve ser calculado pelas formulas deve ser feito nos limites do nosso
# intervalo [-1,1], ou seja, em -1 ou em 1.
#
# O gráfico do erro mostra que o erro é 0 no ponto 0 (do intervalo [-1,1]), por que foi ali
# que fizemos a expansão da série de Maclaurin. O contrário da Sério de Chebyshev, que possui
# um erro mais distribuído ao londo do intervalo (Comparar um Gráfico de Chebyshev e Maclaurin).
erroDeMaclaurin       = abs( yAproximado .- yInterPontos )
erroMaximoDeMaclaurin = max( erroDeMaclaurin )

% plot( x, y, '*' )
% plot( x, y, '*', xInterPontos, yInterPontos, 'g', xInterPontos, yAproximado, 'b' )
#}


# Método de Chebyshev
#
# f(t) = log( .5*t + 1.5 ) in [-1, 1]
#
# i = 1 : k
# fChebyshev( i ) = b0*T0( t(i) ) + b1*T1( t(i) ) + b2*T2( t(i) ) + b3*T3( t(i) ) + ...
# 
# b0 = 1/m \sum_j=1^m f( t(j) ), m = 10
#
# t(j) = cos( ( 2*j - 1 ) * pi / 2*m ), j = 1 : m
# 
source( "ChebyshevPolynomsOfFirstKindList.m" )

# Grau da Série de Chebyshev
k = 10

# Grau de precisão da Integral Numérica, e também o número de nós de Chebyshev
m = 1e1

# Numero de pontos do Gráfico
n = 10

# Gráfico de Chebyshev
a = 1
b = 2
h = (b-a)/n

x = a : h : b
y = fLog( x )

xInterPontos = a : h/20 : b

tInterPontos = MaclaurinLinearTransformationDomainIn( xInterPontos, a, b )
coef_b       = calculateChebyshevCoefficients( k, m, a, b, @fLog )

# For log( x ) in [1, 2]
correct_b = [  3.76452812919196e-001,  3.43145750507620e-001, -2.94372515228575e-002, ...
               3.36708925555306e-003, -4.33275888539416e-004,  5.94707115514397e-005, ...
              -8.50296480504678e-006,  1.25045018720205e-006, -1.87619547238052e-007, ...
               2.79406818857846e-008,  1.71766583014649e-014, -2.79406536352273e-008 ]

# i = 1 : k
# fChebyshev( i ) = b0*T0( t(i) ) + b1*T1( t(i) ) + b2*T2( t(i) ) + b3*T3( t(i) ) + ...
#
# yAproximado = coef_b( 1)*T0( tInterPontos ) ...
#             + coef_b( 2)*T1( tInterPontos ) ...
#             + coef_b( 3)*T2( tInterPontos ) ...
#             + coef_b( 4)*T3( tInterPontos ) ...
#             + coef_b( 5)*T4( tInterPontos ) ...
#             + coef_b( 6)*T5( tInterPontos ) ...
#             + coef_b( 7)*T6( tInterPontos ) ...
#             + coef_b( 8)*T7( tInterPontos ) ...
#             + coef_b( 9)*T8( tInterPontos ) ...
#             + coef_b(10)*T9( tInterPontos )
#
yAproximado = coef_b(1)*T0( tInterPontos ) ...
            + coef_b(2)*T1( tInterPontos ) ...
            + coef_b(3)*T2( tInterPontos ) ...
            + coef_b(4)*T3( tInterPontos );

# Erro máximo deve ser calculado pelas formulas deve ser feito nos limites do nosso
# intervalo [-1,1], ou seja, em -1 ou em 1.
#
# O gráfico do erro mostra que o erro é 0 no ponto 0 (do intervalo [-1,1]), por que foi ali
# que fizemos a expansão da série de Maclaurin. O contrário da Sério de Chebyshev, que possui
# um erro mais distribuído ao londo do intervalo (Comparar um Gráfico de Chebyshev e Maclaurin).
#
yInterPontos          = fLog( xInterPontos );
erroDeChebyshev       = abs( yAproximado .- yInterPontos );
erroMaximoDeChebyshev = max( erroDeChebyshev )

% plot( x, y, '*' )
plot( x, y, '*', xInterPontos, yInterPontos, 'g', xInterPontos, yAproximado, 'b' )



