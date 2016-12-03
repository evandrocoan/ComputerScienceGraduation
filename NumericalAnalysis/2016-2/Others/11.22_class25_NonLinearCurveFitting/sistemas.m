

#
# Curve Fitting - https://en.wikipedia.org/wiki/Curve_fitting
#
# Ajuste de curvas é minimizar o desvio de uma dada curva ajustando seus coeficientes a(1), ... a(n)
# Minimizar:
# D( a(1), a(2) ) = \sum_k=1^m ( log( a(1) + a(2)*x(k)^2 ) - y(k) )^2
#

clc
clear
close all

more off
format long
split_long_rows(0)

#format rat
#output_precision(30)
#output_max_field_width(0)


y = [ 0.1, 0.3, 0.9, 1.2, 1.3, 1.2, 1 ];
m = numel( y );

x = [ 0 : 1 : (m-1) ];


aInicial = [ 1.32 0.38 ];

printf( "\n" )
f1Problema1aInicial = f1Problema1( aInicial )
f2Problema1aInicial = f2Problema1( aInicial )

printf( "\n" )
functions(1).vector = @f1Problema1;
functions(2).vector = @f2Problema1;

aSolucao = fNewtonSistemasNaoLineares( aInicial, functions );

# Podemos perturbar a os coeficientes aSolucao, 10% para cima ou -10% para baixo.
# Caso ele esteja em um ponto de minimo, descolar um pouco para esquerda (*0.9) ou para direita (*1.1)
# o valor do desvio vai subir.
aSolucao = aSolucao.*1.0;

# Os pontos podem não estar ordenados, portanto calculamos os intervalos a partir dos pontos
# mínimo e máximo de `x`.
xInterpontos           = min( x ) - 0.1 : 0.01 : max( x ) + 0.1;
yInterPontosAproximado = aSolucao(1) * sin( xInterpontos*aSolucao(2) );

plot( x, y, '*', xInterpontos, yInterPontosAproximado );
legend('location','north');
grid on;

# A se a média dos desvios for zero, então temos que o desvio quadrático está correto, pois
# partimos do principio:
#
#     Usando a(1) e a(2) de modo que D seja mínimo:
#     desvioLocal = a(1)*x(k) + a(2)*cos(x(k)) - y(k)
#     D( a(1), a(2) ) = \sum_k=1^m ( desvioLocal )^2
#
# Desvio quadrático médio = sum(desvioLocal.^2) / m
#

desvioLocal      = aSolucao(1) .* sin( x.*aSolucao(2) ) .- y
desvioLocalMedio = sum( desvioLocal ) / m

desvioLocalQuadratico      = desvioLocal.^2
desvioLocalQuadraticoMedio = sum( desvioLocalQuadratico ) / m

printf( "\n" )
f1Problema1 = f1Problema1( aSolucao )
f2Problema1 = f2Problema1( aSolucao )


