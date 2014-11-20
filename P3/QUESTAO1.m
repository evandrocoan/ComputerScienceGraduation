%Interpolação polinomial por Gregory-Newton
clc
clear
a = 1;
b = 2;
n = 0; %numero de subdivisões do intervalo [a,b] (grau do polinomio)
passos = 0;
ErroMax = 0;

while ( ErroMax < ( 3.16 * 1e-4 ) && ( passos < 1000 ) )
	passos = passos + 1;
	n = n + 1;
	h = ( b - a ) / n; % espaçamento entre as subdivisões
	x = a : h : b; %vetor de pontos para plotar o gráfico
	y = log(x);

	%matriz que resulta de uma função diferenças divididas
	difDiv = fDifDiv( n, x, y )
	difDiv(n+1,n) = 0;
	[x' y' difDiv ]

	%plotagem dos pontos 
	nPlotagem = 50 * n; %3 intervalos dividos em 20 vezes
	aPlot = a;
	bPlot = b;
	hPlotagem = ( bPlot - aPlot ) / nPlotagem;
	xP = aPlot: hPlotagem : bPlot; % 31 pontos para n = 3

	yE = log(xP);

	for i = 1 : nPlotagem + 1
		%yP(i) = fPnLagrange( n, x, y, xP(i) );
		yP(i) = fPnGregNew( n, x, y, difDiv, xP(i) );
	end
	erro = abs( yE .- yP )

	ErroMax = max(erro)
end

%plot( x, y, "*;f(x) = ln;", xP, yE, "r;f(x)=ln(x);", xP, yP, "b;g(x) = Pn(x) de Lagrange;" );
%plot( xP, erro, "k;Erro(x) = |f(x) - Pn(x)|;" );
