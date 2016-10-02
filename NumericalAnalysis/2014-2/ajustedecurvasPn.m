clc
clear

%número de pontos, do teste experimental
m = 8
x = [ 1, 2,   3,   5,  10, 15, 16, 19  ]
y = [ 1, 1.5, 4.5, 15, 39, 62, 81, 101 ]

%genérico para n = 1 (a0 + a1 * x), ajustar uma reta (se n=2, é uma parabola, etc...)
n = 2
numEq = n + 1; %número de equações e o número de incognitas

for i = 1 : numEq
	for j = 1 : numEq 
		somatorio = 0;
		for k = 1 : m % m = número de testes experimentais
			somatorio = somatorio + x(k) ^ ( i + j - 2 );
		end
		A(i, j) = somatorio;
	end
	somatorio = 0;
	for k = 1 : m
		somatorio = somatorio + x(k)^(i-1) * y(k);
	end
	A(i, numEq + 1) = somatorio;
end

A

coefs = fGauss( numEq, A )
coefs = coefs * 1.01;
%plotagem dos pontos 
nPlotagem = 200; %3 intervalos dividos em 20 vezes
hPlotagem = ( x(m) - x(1) ) / nPlotagem;
xP = x(1): hPlotagem : x(m); % 31 pontos para n = 3

for i = 1 : nPlotagem + 1
	yP(i) = fPnBrio( n, coefs, xP(i) );
end

%calculo do erro
%um polinomio de grau igual ao numero de pontos fornecidos, passa sobre todos os pontos
%por exemplo, um polinomio de grau 7 possui 8 coeficientes e passa portanto sobre 8 pontos
for i = 1 : m
	yAjustado(i)  = fPnBrio( n, coefs, x(i) );
end
D=fdesvio(m, y, yAjustado )

plot( x, y, "*;pontos testes;", xP, yP, "b;função ajustadora de grau n;" );
