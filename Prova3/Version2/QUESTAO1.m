%Interpolação polinomial por Gregory-Newton
clc
clear
format long
a = -1;
b = 1;
n = 0; %numero de subdivisões do intervalo [a,b] (grau do polinomio)
passos = 0;
erroMax1 = 1;
erroPermitido = sqrt(10) * 1e-4;

while ( (erroMax1 > erroPermitido) && ( passos < 100 ) ) 
	passos = passos + 1;
	n = n + 1;
	
	h = ( b - a ) / n; 
	x = a : h : b;
	y = cos(x);

	difDiv = fDifDiv( n, x, y );
	difDiv(n+1,n) = 0;
	[x' y' difDiv ];

	%plotagem dos pontos
	nPlotagem = 50 * n; 
	aPlot = a;
	bPlot = b;
	hPlotagem = ( bPlot - aPlot ) / nPlotagem;
	xP = aPlot: hPlotagem : bPlot; 
	yE = f(xP);

	for i = 1 : nPlotagem + 1
		yP(i) = fPnGregNew( n, x, y, difDiv, xP(i) );
	end
	
	%cálculo do erro por 
	erro1 = abs( yE .- yP );
	erroMax1 = max(erro1);
end

erroMax1
n

%plot( xP, yE, "r;f(x) = cos(x);", xP, yP, "b;g(x) = Pn(x) de Gregory-Newton;" );
%plot( xP, erro1, "k;Erro(x) = |f(x) - Pn(x)|;" );
