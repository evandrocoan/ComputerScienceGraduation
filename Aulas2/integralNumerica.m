clear
clc
format long
a = 1
b = 6

%método dos trapézios
n = 512

Tn = fTrapezio( n, a, b )

_I = log( 1 + b ) - log( 1 + a ) % Integral exata

T2 = fTrapezio( 2 * n, a, b ) % simulação de um valor exato (com mais casas decimais)
erroExato = abs( Tn - _I ) 
erroExatoEstimado = abs( Tn - T2 ) %erro estimado usando a simulação de um valor exato

%método de Simpson
n = 32 %n SEMPRE PAR

Sn = fSimpson( n, a, b )

S2 = fSimpson( 2 * n, a, b ) % simulação de um valor exato (com mais casas decimais)
erroExato = abs( Sn - _I ) 
erroExatoEstimado = abs( Sn - S2 ) %erro estimado

%método de gauss legendre
m = 3 %número de pontos
Gm = fGaussLegendre( m, a, b)
G2 = fGaussLegendre( m+1, a, b)

erroExatoEstimado = abs( Gm - G2 ) %erro estimado
