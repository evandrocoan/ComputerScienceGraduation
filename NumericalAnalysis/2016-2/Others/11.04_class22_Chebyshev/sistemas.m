


###################################################################################################
###################################################################################################
###################################################################################################
# Método de Chebyshev
#
# f(t) = log( .5*t + 1.5 ) in [-1, 1]
#
# i = 1 : n
# fChebyshev( i ) = b0*T0( t(i) ) + b1*T1( t(i) ) + b2*T2( t(i) ) + b3*T3( t(i) ) + ...
#
# b0 = 1/m \sum_j=1^m f( t(j) ), m = 10
#
# t(j) = cos( ( 2*j - 1 ) * pi / 2*m ), j = 1 : m
#
function erroMaximoDeChebyshev = run_chebyshev_test( a, b, targetFunction, n, m, chebyshevPolynomType )

    source( "ChebyshevPolynomsOfFirstKindList.m" );

    # Gráfico de Chebyshev
    h = (b-a)/n;

    x = a : h : b;
    y = targetFunction( x );

    xInterPontos = a : h/20 : b;
    tInterPontos = ChebyshevDomainLinearTransformationIn( xInterPontos, a, b );
    coef_b       = calculateChebyshevCoefficients( n, m, a, b, targetFunction, chebyshevPolynomType );

    # For log( x ) in [1, 2]
    correct_b = [  3.76452812919196e-001,  3.43145750507620e-001, -2.94372515228575e-002, ...
                   3.36708925555306e-003, -4.33275888539416e-004,  5.94707115514397e-005, ...
                  -8.50296480504678e-006,  1.25045018720205e-006, -1.87619547238052e-007, ...
                   2.79406818857846e-008,  1.71766583014649e-014, -2.79406536352273e-008 ];

    #
    # printf( '\n\n\n\n\n( run_chebyshev_test ) Calling the evaluateChebyshevPolynom.\n' );

    # i = 1 : n
    # fChebyshev( i ) = b0*T0( t(i) ) + b1*T1( t(i) ) + b2*T2( t(i) ) + b3*T3( t(i) ) + ...
    #
    # yAproximado = coef_b( 1)*T0( tInterPontos ) ...
    #             + coef_b( 2)*T1( tInterPontos ) ...
    #             + coef_b( 3)*T2( tInterPontos ) ...
    #             + coef_b( 4)*T3( tInterPontos );
    #
    yAproximado = evaluateChebyshevPolynom( n, coef_b, tInterPontos, chebyshevPolynomType );

    # Erro máximo deve ser calculado pelas formulas deve ser feito nos limites do nosso
    # intervalo [-1,1], ou seja, em -1 ou em 1.
    #
    # O gráfico do erro mostra que o erro é 0 no ponto 0 (do intervalo [-1,1]), por que foi ali
    # que fizemos a expansão da série de Maclaurin. O contrário da Sério de Chebyshev, que possui
    # um erro mais distribuído ao londo do intervalo (Comparar um Gráfico de Chebyshev e Maclaurin).
    #
    yInterPontos          = targetFunction( xInterPontos );
    erroDeChebyshev       = abs( yAproximado .- yInterPontos );
    erroMaximoDeChebyshev = max( erroDeChebyshev );

    # plot( x, y, '*' )
    plot( x, y, '*', xInterPontos, yInterPontos, 'g', xInterPontos, yAproximado, 'b' );

end

clc
clear
close all

more off
format long
split_long_rows(0)

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
profile clear
profile on

# Numero de pontos do Gráfico e grau da Série de Chebyshev
n = 18

# Domínio
a = 1
b = 2

# Grau de precisão da Integral Numérica, e também o número de nós de Chebyshev
m = 100

errorByPolinom__ = run_chebyshev_test( a, b, @log, n, m, @getChebyshevCoefficientsByPolinom );
errorNumerically = run_chebyshev_test( a, b, @log, n, m, @getChebyshevCoefficientsNumerically );

chebyshevCoefficientsByPolinom_at_06__ = getChebyshevCoefficientsByPolinom  ( n, 0.6 )
chebyshevCoefficientsNumerically_at_06 = getChebyshevCoefficientsNumerically( n, 0.6 )

errorByPolinom__
errorNumerically

chebyshevCoefficientsPolinom_n = getChebyshevPolinom( n );
polyout( chebyshevCoefficientsPolinom_n, "t" );



# Stop profiling. The collected data can later be retrieved and examined.
profile off

# Interactively explore hierarchical profiler output.
# profexplore()

# Show the profile resume, displaying per-function profiler results.
#
# profshow (data, n)
# If data is unspecified, profshow will use the current profile dataset.
# If n is unspecified it defaults to 20.
profshow( profile ("info"), 8 )




