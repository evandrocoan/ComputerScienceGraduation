clear
clc
format long

% x   = [ 0:0.01: 5 * pi ];
% y_1 = tan( x );
% y_2 = 1 ./ x;

# encontrar os pontos onde: tan( x ) == 1 / x
#
#plot( x, y_1, 'b', x, y_2, 'r', 'linewidth', 5 )
#
# localizamos as 5 raizes iniciais

xi = [ pi / 4, pi, 2 * pi, 3 * pi, 4 * pi ];

for i = 1 : 5
    i;
    x( i ) = fMetodoDeNewton( xi( i ), 1e-15 );
end

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
# a(n+1) != 0, para exclir raízes nulas
#
# r = 1 + { max( { abs( a(2) ), abs( a(3) ), ..., abs( a(n) ), abs( a(n+1) ) } ) } / abs( a(1)



function x = cute_roots( a )
    
    n = size( a, 2 ) - 1
    
    k = 1
    
    xi( k ) = fLocaliza( n, a )
    
end

function xi = fLocaliza( n, a )
    
    # Encontrar o raio inicial
    raio_inicial = 1 + max( abs( a(2:n+1) ) ) / abs( a(1) )
    
    # Encontrar o raio mínimo das raízes. Abaixo dele não existe raízes.
    raio_minimo = 1 / ( 1 + max( abs( a(1:n) ) ) / abs( a(n+1) ) )
    
    xi = ( raio_inicial + raio_minimo ) / 2
    
    
end


a = [ 1, -3, 3, -1 ]
cute_roots( a )

















