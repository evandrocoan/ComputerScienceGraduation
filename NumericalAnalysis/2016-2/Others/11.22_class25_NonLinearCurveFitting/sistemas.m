

#
# Curve Fitting - https://en.wikipedia.org/wiki/Curve_fitting
#
# Ajuste de curvas é minimizar o desvio de uma dada curva ajustando seus coeficientes a(1), ... a(n)
# Minimizar:
# D( a(1), a(2) ) = \sum_k=1^m ( log( a(1) + a(2)*x(k)^2 ) - y(k) )^2
#


y = [ 0.1, 0.3, 0.9, 1.2, 1.3, 1.2, 1 ]
m = numel( y )

x = [ 0 : 1 : (m-1) ]


aInicial = [ 0.5, 0.5 ]

f1Problema1aInicial = f1Problema1( aInicial )
f2Problema1aInicial = f2Problema1( aInicial )

aSolucao = fNewtonSistemasNaoLineares( aInicial, @f1Problema1, @f2Problema1 )

# Os pontos podem não estar ordenados, portanto calculamos os intervalos a partir dos pontos
# mínimo e máximo de `x`.
xInterpontos           = min( x ) : 0.01 : max( x );
yInterPontosAproximado = aSolucao(1) * sin( xInterpontos*aSolucao(2) );

plot( x, y, '*', xInterpontos, yInterPontosAproximado );








