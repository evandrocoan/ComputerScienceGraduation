%Método Iterativo de Jacob
clear
clc
format long

xi = [ 0 0 0 ]
x = [ 0 0 0 ]

ErroMax = 1e-8

%1º passo
passo = 0
Erro = 1

while ( ( Erro > ErroMax ) & ( passo < 100 ) )
	passo = passo + 1	
	x(1) = ( 1 + xi(2) + xi(3) ) / 3;
	x(2) = ( 5 - xi(1) - xi(3) ) / 3;
	x(3) = ( 4 - 2 * xi(1) + 2 * xi(2) ) / 4;
	x
	% adiciona-se ponto antes do operador para torná-lo um operador vetorial
	Erro = max( abs( x .- xi ) )
	xi = x;
end
