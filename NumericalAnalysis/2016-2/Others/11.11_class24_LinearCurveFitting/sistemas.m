

#
# Curve Fitting - https://en.wikipedia.org/wiki/Curve_fitting
#
# m = 5
# x = [ 0   , 0.39, 0.78, 1.18 ]
# y = [ 0.99, 0.92, 0.71, 0.28 ]
#
# Esta formula de formações foi encontrada inicialmente para um conjunto de pontos, portanto para
# outros pontos precisamos re-calcular os coeficientes a(1) e a(2) dos pontos.
# y = g(x) = a(1)*x + a(2)*cos(x)
#
# Desvio Local
# \for_all k = 1 : m
# desvio = a(1)*x(k) + a(2)*cos(x(k)) - y(k)
#
# Usando a(1) e a(2) de modo que D seja mínimo.
# D( a(1), a(2) ) = \sum_k=1^m ( a(1)*x(k) + a(2)*cos(x(k)) - y(k) )^2
#
# Primeira derivado igual a zero significa ponto de máximo, de mínimo ou de cela.
#
# Segunda derivada positiva significa Ponto de Mínimo. Como o x calculado eixo de uma parabola.
# Segunda derivada negativa significa Ponto de Máximo. Como o x calculado eixo de uma parabola invertida.
# Mas, segunda derivada igual a zero significa que nada podemos saber sobre o ponto em questão.
#
# Primeiro passo, encontrar os pontos críticos fazemos a primeira derivada igual a zero,
# para encontrar os pontos de mínimos e máximos.
#
#
# Primeira derivada de D( a(1), a(2) ) em relação à variável a(1)
# ( u^2 )' = 2*u*u'
# D( a(1), a(2) ) / dx a(1) = \sum_k=1^m 2*( a(1)*x(k) + a(2)*cos(x(k)) - y(k) )*( x(k) ) = 0
#
# Agora dividimos por 2, e eliminamos o 2, já que fizemos a derivado igual a zero:
# D( a(1), a(2) ) / dx a(1) = \sum_k=1^m 2*( a(1)*x(k) + a(2)*cos(x(k)) - y(k) )*( x(k) ) = 0 (/2)
# D( a(1), a(2) ) / dx a(1) = \sum_k=1^m   ( a(1)*x(k) + a(2)*cos(x(k)) - y(k) )*( x(k) ) = 0
#
# Aplicamos o somatório a cada uma das parcelas de sua soma interna:
# D( a(1), a(2) ) / dx a(1) =   \sum_k=1^m ( a(1)*x(k)^2         )
#                             + \sum_k=1^m ( a(2)*x(k)*cos(x(k)) )
#                             - \sum_k=1^m ( y(k)*x(k)           ) = 0
#
# Agora retiramos de dentro do somatório os termos comuns que nunca mudam. Estes são os
# independentes do índice `k` do somatório:
# D( a(1), a(1) ) / dx a(1) =   a(1)* \sum_k=1^m ( x(k)^2         )
#                             + a(2)* \sum_k=1^m ( x(k)*cos(x(k)) )
#                             -       \sum_k=1^m ( y(k)*x(k)      ) = 0
#
#
# Primeira derivada de D( a(1), a(2) ) em relação à variável a(2)
# ( u^2 )' = 2*u*u'
# D( a(1), a(2) ) / dx a(2) = \sum_k=1^m 2*( a(1)*x(k) + a(2)*cos(x(k)) - y(k) )*( cos(x(k)) ) = 0
#
# Agora dividimos por 2, e eliminamos o 2, já que fizemos a derivado igual a zero:
# D( a(1), a(2) ) / dx a(2) = \sum_k=1^m 2*( a(1)*x(k) + a(2)*cos(x(k)) - y(k) )*( cos(x(k)) ) = 0 (/2)
# D( a(1), a(2) ) / dx a(2) = \sum_k=1^m   ( a(1)*x(k) + a(2)*cos(x(k)) - y(k) )*( cos(x(k)) ) = 0
#
# Aplicamos o somatório a cada uma das parcelas de sua soma interna:
# D( a(1), a(2) ) / dx a(2) =   \sum_k=1^m ( a(1)*x(k)*cos(x(k)) )
#                             + \sum_k=1^m ( a(2)*cos(x(k))^2    )
#                             - \sum_k=1^m ( y(k)*cos(x(k))      ) = 0
#
# Agora retiramos de dentro do somatório os termos comuns que nunca mudam. Estes são os
# independentes do índice `k` do somatório:
# D( a(1), a(2) ) / dx a(2) =   a(1)* \sum_k=1^m ( x(k)*cos(x(k)) )
#                             + a(2)* \sum_k=1^m ( cos(x(k))^2    )
#                             -       \sum_k=1^m ( y(k)*cos(x(k)) ) = 0
#
#
# Por fim terminamos de montar o sistema de equações utilizando as derivadas parciais de
# `D( a(1), a(2) )`.
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


x = [ 0   , 0.39, 0.78, 1.18 ]
y = [ 0.99, 0.92, 0.71, 0.28 ]
m = numel( x )


coeficientes = ajusteDeCurvasLinearesParaCos( m, x, y )

# Os pontos podem não estar ordenados, portanto calculamos os intervalos a partir dos pontos
# mínimo e máximo de `x`.
xInterpontos = min( x ) - 0.0 : 0.01 : max( x ) + 0.0;
yInterpontos = coeficientes(1).*xInterpontos .+ coeficientes(2).*cos(xInterpontos);

plot( x, y, '*', xInterpontos, yInterpontos );








