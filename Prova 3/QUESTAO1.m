%Interpolação polinomial por Gregory-Newton
clc
clear
format long
a = -1;
b = 1;
n = 0; %numero de subdivisões do intervalo [a,b] (grau do polinomio)
passos = 0;
erroMax = 1;
erroPermitido = sqrt(10) * 1e-4;

while ( (erroMax > erroPermitido) && ( passos < 100 ) ) 
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
	erroMax1 = max(erro);
end

erroMax
n

%plot( xP, yE, "r;f(x) = cos(x);", xP, yP, "b;g(x) = Pn(x) de Gregory-Newton;" );
plot( xP, erro, "k;Erro(x) = |f(x) - Pn(x)|;" );



%Aproximação por Séries Maclaurin
clc
clear
format long
a = -1;
b = 1;
n = 0; %numero de subdivisões do intervalo [a,b] (grau do polinomio)
passos = 0;
erroMax = 1;
erroPermitido = sqrt(10) * 1e-6;

while ( ( erroMax > erroPermitido )  && ( passos < 100 ) )
	n += 1;
	passos+=1;
	
	h = ( b - a ) / n; 
	x = a : h : b;
	y = cos(x);
	
	cMac = fCoefMaclaurin(n);
	
	
	nPlot = 50 * n; 
	aPlot = a;
	bPlot = b;
	hPlot = ( bPlot - aPlot ) / nPlot;
	xP = aPlot: hPlot : bPlot; 
	yE = f(xP);
	
	for i = 1 : nPlot + 1
		yMac(i) = fPnBrio(n, cMac, xP(i));
	end
	
	erroMac = abs( yE .- yMac );
	erroMax =  max( erroMac );
end

n
erroMax


% plot( xP, yE, "b;f(x) = cos(x);", xP, yMac, "k;Série de Maclaurin;" );
plot( xP, erroMac, "k;Erro(x) = |f(x) - Pn(x)|;" );