


clc
clear
close all

more off
format long
split_long_rows(0)



###################################################################################################
###################################################################################################
###################################################################################################
# Método de MacLaurin
# 
# Derivatives
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
function run_maclarin_test( n, a, b, targetFunction )

    h = (b-a)/n

    coefMaclaurin = calculateMaclaurinCoefficients( n, a, b, targetFunction )

    x = a : h : b
    y = targetFunction( x )

    xInterPontos = a : h/20 : b
    yInterPontos = targetFunction( xInterPontos )

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

end



###################################################################################################
###################################################################################################
###################################################################################################
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
function run_chebyshev_test( n, a, b, targetFunction, k, m )

    source( "ChebyshevPolynomsOfFirstKindList.m" )

    # Gráfico de Chebyshev
    h = (b-a)/n

    x = a : h : b;
    y = targetFunction( x );

    xInterPontos = a : h/20 : b;
    tInterPontos = MaclaurinLinearTransformationDomainIn( xInterPontos, a, b );
    coef_b       = calculateChebyshevCoefficients( k, m, a, b, targetFunction )

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
    #             + coef_b( 4)*T3( tInterPontos );
    #
    yAproximado = evaluateChebyshevPolynom( k, coef_b, tInterPontos );

    # Erro máximo deve ser calculado pelas formulas deve ser feito nos limites do nosso
    # intervalo [-1,1], ou seja, em -1 ou em 1.
    #
    # O gráfico do erro mostra que o erro é 0 no ponto 0 (do intervalo [-1,1]), por que foi ali
    # que fizemos a expansão da série de Maclaurin. O contrário da Sério de Chebyshev, que possui
    # um erro mais distribuído ao londo do intervalo (Comparar um Gráfico de Chebyshev e Maclaurin).
    #
    yInterPontos          = targetFunction( xInterPontos );
    erroDeChebyshev       = abs( yAproximado .- yInterPontos );
    erroMaximoDeChebyshev = max( erroDeChebyshev )

    % plot( x, y, '*' )
    plot( x, y, '*', xInterPontos, yInterPontos, 'g', xInterPontos, yAproximado, 'b' )

end





# Profiling
# 
# Command: profile on
# Command: profile off
# Command: profile resume
# Command: profile clear
# Function File: S = profile ("status")
# Function File: T = profile ("info")
# 
# https://www.gnu.org/software/octave/doc/v4.0.1/Profiling.html
profile on


# Numero de pontos do Gráfico
n = 10

a = 1
b = 2

% run_maclarin_test( n, a, b, @fLog )



# Grau da Série de Chebyshev
k = 18

# Grau de precisão da Integral Numérica, e também o número de nós de Chebyshev
m = 100

run_chebyshev_test( n, a, b, @fLog, k, m )

polynom = evaluateChebyshevPolynomOfFirstKindAt_( k )
% polyout( polynom );

# Stop profiling. The collected data can later be retrieved and examined.
profile off

# Interactively explore hierarchical profiler output.
% profexplore()

# Show the profile resume, displaying per-function profiler results.
# 
# profshow (data, n)
# If data is unspecified, profshow will use the current profile dataset.
# If n is unspecified it defaults to 20.
profshow( profile ("info"), 8 )




