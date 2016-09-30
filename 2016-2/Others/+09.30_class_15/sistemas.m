
clear
clc
format long


# Dominio da tangante { x \in \R | x != pi / 2 + k * pi, k \in \Z } 
#function x = f( x )
#    
#    x = tan( x ) - 1
#    
#end

# Encontre as 5 raízes positivas
#
# x * tan( x ) - 1 = 0
# tan( x ) 
#
# tan x = cateto oposto / cateto adjacente
# tan \theta = sen \theta / cos \theta
# 
# tan( 180º ) = 0, -> tan( 0 ) = 0
# tan( 3 pi / 4 = 35º ) = 1 
#
# tan( 91º ) = perto de - \inf 
# tan( 89º ) = perto de + \inf 


# Ex:
# P3( x ) = x^3 - 3*x^2 + 3x - 1 = 0;
# -> (x - 1)^3 = 0
# \alpha = 1 :: M = 3 ( multiplicidade, há três raízes )
# 
# Raízes podem ser simples ou repetidas (múltiplas)
#
# Domínio = { x \in \C }
# a + bi = a + 0i, \R contido \C
#   \C       \R
#
# 1º passo
# xi = ?
# abs( xi ) <= r (que é a cota limitante, i.e., raio)
# 
# Pn( x ) = a(1)*x^n + a(2)*x^(n-1) + ... + a(n)*x + a(n+1) = 0
# 
# a(1)   != 0, para grau n
# a(n+1) != 0, para excluir raízes nulas
#
# r = 1 + { max( { abs( a(2) ), abs( a(3) ), ..., abs( a(n) ), abs( a(n+1) ) } ) } / abs( a(1)








# Divisão sintética de Brio-Rufini
#
# Somente funciona para polinômios onde o divisor é da forma x - 1.
# Assim dividiremos sempre por:
# 
# x - xi
# 
# P3( x ) = x^3 - 3*x^2 + 3*x - 1 == 0
# 
#   xi |  x^3 |  x^2 | x^1  |         <-- coeficientes do polinômio a.k.a. a
# ---------------------------------
#    2 |    1 |   -3 |  +3  |   -1
#      |      |  2*1 | 2*-1 |  2*1
#    1 |   -1 |    1 |  +1  |   +1    <-- b( i )
#
#
#   xi | a(1) | a(2) | ... | a(n) | a(n+1)
# ---------------------------------
#      | b(1) | b(2) | ... | b(n) | b(n+1)
#
# b(1)   = a(1), 
# b(2)   = a(2) + xi*b(1), 
# ...
# b(i)   = a(n) + xi*b(n-1), 
# b(n+1) = a(n+1) + xi*b(n)
# 
#
# Pn^n( xi ) = k! * R( k + 1 ), x = 0, 1, 2, ...
# R( k + 1 ) é o resto da divisão k de Pn( x ) por ( x - xi )
#
# Se xi = 2, P3( xi == 2 ) = 2^3 - 3*2^2 + 3*2 - 1 == 0
# P3^0( xi == 2 ) = 0! * R( 0 + 1 ) = R( 1 ) = 1
# 
a = [ 1, -3, 3, -1 ]
my_roots( a )













