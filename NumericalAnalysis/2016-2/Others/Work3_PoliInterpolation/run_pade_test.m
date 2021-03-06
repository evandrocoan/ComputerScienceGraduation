

###################################################################################################
###################################################################################################
###################################################################################################
# 4-aproximação racional de padé
#
# yPade(x) = (a(1) + a(2)x + a(3)x^2 + a(4)x^3)/(1 + b(1)x + b(2)*x^2)
#
# R32 = (a(1) + a(2)x + a(3)x^2 + a(4)x^3)/(1 + b(1)x + b(2)*x^2)
#
# Como determinar o m e n do método de Pade?
# Não é como saber, somente experimentando valores e verificando o resultado/erro.
#
# A método de Pade é útil para funções assintóticas como 1/x.
# Ele é o contrário da Serie de Chebyshev, que não funciona para funções assintóticas.
#
# Por exemplo, a função log( x ) é assintótica entre perto de 0 até 1. Mas podemos utilizar
# Chebyshev no função log( x ) no intervalo [1, 2] ou mais, onde a função é suave/bem comportada.
#
function [ erroMaximoDePade, aPadeCoefficients, bPadeCoefficients, ...
        xInterPontos, erroDePade ] = run_pade_test( ...
        n, m, a, b, targetFunction, maclaurinCoefficientsFunction )

    h = (b-a)/n;

    x = a : h : b;
    y = targetFunction( x );

    grauDeMaclaurinParaPade = n + m;
    coefMaclaurinParaPade   = maclaurinCoefficientsFunction( grauDeMaclaurinParaPade, a, b );

    xInterPontos      = a : h/20 : b;
    yInterPontosExato = targetFunction( xInterPontos );

    # R32 = (a(1) + a(2)x + a(3)x^2 + a(4)x^3)/(1 + b(1)x + b(2)*x^2)
    #
    [ aPadeCoefficients, bPadeCoefficients ] = calculatePadeCoefficients( n, m, coefMaclaurinParaPade );
    tInterPontos = ChebyshevDomainLinearTransformationIn( xInterPontos, a, b );

    # For log( x ) in [1, 2] with n = 3 and m = 2
    ap_correct_value = [ 0.4054651   0.4955194   0.0912933   0.0012346 ];
    aPadeCoefficients;

    # For log( x ) in [1, 2] with n = 3 and m = 2

    bp_correct_value = [ 1.00000     0.40000     0.03333     0.00000   ];
    bPadeCoefficients;

    # yAproximado = fPnPorBriotRunifi( n, coefMaclaurin, tInterPontos )
    # yAproximadoPorPade = polyval( aPadeCoefficients, tInterPontos ) / polyval( bPadeCoefficients, tInterPontos );
    #
    yAproximadoPorPade = fPnPorBriotRunifi( ...
            n, aPadeCoefficients, tInterPontos ) ./ fPnPorBriotRunifi( ...
            m, bPadeCoefficients, tInterPontos );

    #
    # O erro de Pade é como o erro de Maclaurin, ele é 0 no ponto 0 entre [-1,1], como o Maclaurin.
    # O Maclaurin tem esse erro por que o Maclaurin é expandido em torno do ponto 0.
    # Mas mesmo o Pade sendo feito em base do Maclaurin, ele melhora um pouco o erro de Maclaurin com
    # seu polinômio interpolador.
    #
    erroDePade       = abs( yAproximadoPorPade .- yInterPontosExato );
    erroMaximoDePade = max( erroDePade );

    # plot( x, y, '*' )
    # plot( x, y, '*', xInterPontos, yInterPontosExato, 'g', xInterPontos, yAproximadoPorPade, 'b' )

end




