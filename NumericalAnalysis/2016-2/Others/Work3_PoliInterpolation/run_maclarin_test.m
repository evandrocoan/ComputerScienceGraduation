
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
function erroMaximoDeMaclaurin = run_maclarin_test( n, a, b, targetFunction )

    h = (b-a) / n;
    x = a : h : b;
    y = targetFunction( x );

    xInterPontos = a : h/20 : b;
    yInterPontos = targetFunction( xInterPontos );

    coefMaclaurin = calculateMaclaurinCoefficientsForSin( n, a, b );

    tInterPontos = ChebyshevDomainLinearTransformationIn( xInterPontos, a, b );
    yAproximado  = fPnPorBriotRunifi( n, coefMaclaurin, tInterPontos );

    # Erro máximo deve ser calculado pelas formulas deve ser feito nos limites do nosso
    # intervalo [-1,1], ou seja, em -1 ou em 1.
    #
    # O gráfico do erro mostra que o erro é 0 no ponto 0 (do intervalo [-1,1]), por que foi ali
    # que fizemos a expansão da série de Maclaurin. O contrário da Sério de Chebyshev, que possui
    # um erro mais distribuído ao londo do intervalo (Comparar um Gráfico de Chebyshev e Maclaurin).
    erroDeMaclaurin       = abs( yAproximado .- yInterPontos );
    erroMaximoDeMaclaurin = max( erroDeMaclaurin );

    # plot( x, y, '*' )
    # plot( x, y, '*', xInterPontos, yInterPontos, 'g', xInterPontos, yAproximado, 'b' )

end


