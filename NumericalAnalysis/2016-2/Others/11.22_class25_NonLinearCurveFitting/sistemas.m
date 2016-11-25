

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

functions(1).vector = @f1Problema1;
functions(2).vector = @f2Problema1;

aSolucao = fNewtonSistemasNaoLineares( aInicial, functions )

# Podemos perturbar a os coeficientes aSolucao, 10% para cima ou -10% para baixo.
# Caso ele esteja em um ponto de minimo, descolar um pouco para esquerda (*0.9) ou para direita (*1.1)
# o valor do desvio vai subir.
aSolucao = aSolucao.*0.9

# Os pontos podem não estar ordenados, portanto calculamos os intervalos a partir dos pontos
# mínimo e máximo de `x`.
xInterpontos           = min( x ) : 0.01 : max( x );
yInterPontosAproximado = aSolucao(1) * sin( xInterpontos*aSolucao(2) );

plot( x, y, '*', xInterpontos, yInterPontosAproximado );


# Desvio                  = sum( abs(d) )

DesvioLocal =

# A se a média dos desvios for zero, então temos que o desvio quadrático está correto, pois
# partimos do principio:
#
#     Usando a(1) e a(2) de modo que D seja mínimo:
#     desvioLocal = a(1)*x(k) + a(2)*cos(x(k)) - y(k)
#     D( a(1), a(2) ) = \sum_k=1^m ( desvioLocal )^2
#
# Desvio quadrático médio = sum(d.^2) / m



