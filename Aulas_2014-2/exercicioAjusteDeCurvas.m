clc
clear

a = 1;
b = 2;

pontos = 10;
h = abs( a - b ) / pontos;
x = a : h : b;
y = log( x );

n = 2; %grau do polinomio interpolador
D = 1;
passos = 0;

while ( D > 1.e-6 * sqrt(10) ) && ( passos < 100 )
	passos = passos + 1
	n = n + 1;

	numEq = n + 1; %número de equações

	for i = 1 : numEq
		for j = 1 : numEq
			somatorio = 0;
			for k = 1 : pontos %pontos = número de testes experimentais
				somatorio = somatorio + x(k) ^ (i + j + 2);
			end
			A(i, j) = somatorio;
		end
		somatorio = 0;
		for k = 1 : pontos %numero de pontos experimentais
			somatorio = somatorio + x(k) ^ ( i - 1 ) * y(k);
		end
		A(i, numEq + 1) = somatorio;
	end

	coefs = fGauss( numEq, A );

	%calculo do erro
	for i = 1 : pontos
		yAjustado(i) = fPnBrio( n, coefs, x(i) );
	end

	D = fdesvio( pontos, y, yAjustado )
end