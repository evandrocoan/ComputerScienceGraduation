%Método Iterativo de Gauss-Seidel com fator de subrelaxação
clear
clc
format long

xi = [ 0 0 0 ]
x = xi;

ErroMax = 1e-8

%1º passo
passo = 0
Erro = 1
fat = 0.5

while ( ( Erro > ErroMax ) & ( passo < 100 ) )
	passo = passo + 1	
	x(1) = ( 1 - fat ) * xi(1) + fat * ( 1 + x(2) + x(3) ) / 3;
	x(2) = ( 1 - fat ) * xi(2) + fat * ( 5 - x(1) - x(3) ) / 3;
	x(3) = ( 1 - fat ) * xi(3) + fat * ( 4 - 2 * x(1) + 2 * x(2) ) / 4;
	x
	% adiciona-se ponto antes do operador para torná-lo um operador vetorial
	Erro = max( abs( x .- xi ) )
	xi = x;
	vetorX(passo) = passo;
	vetorY1(passo) = x(1);
	vetorY2(passo) = x(2);
	vetorY3(passo) = x(3);	
end
plot( vetorX, vetorY1, "r; x(1);", vetorX, vetorY2, "g; x(2);",vetorX, vetorY3, "b; x(3);"  )
