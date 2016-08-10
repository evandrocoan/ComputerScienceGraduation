clc
clear

a = 1;
b = 2;

pontos = 10; %número de pontos a ser usa como experimentais
h = abs( a - b ) / pontos;
x = a : h : b;
y = cos(x);

n = 10; %grau do polinomio aproximador
numEq = n + 1;

for i = 1 : numEq
	for j = 1 : numEq
		somatorio = 0;
		for k = 1 : pontos
			somatorio = somatorio + x(k) ^ ( i + j - 2);
		end
		A(i,j) = somatorio;
	end
	somatorio = 0;
	for k = 1 : pontos
		somatorio = somatorio + x(k) ^ ( i - 1 ) * y(k);
	end
	A(i, numEq + 1) = somatorio;
end

coefs = fGauss( numEq, A );

for i = 1 : pontos
	yAproximado(i) = fPnBrio( n, coefs, x(i) );
end

D = fdesvio( pontos, y, yAproximado )










