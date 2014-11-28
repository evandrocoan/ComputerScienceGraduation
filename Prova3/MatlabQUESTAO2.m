%Aproximação por Séries Maclaurin
clc
clear
format long
a = -1;
b = 1;
n = 0; %numero de subdivisões do intervalo [a,b] (grau do polinomio)
passos = 0;
erroMax = 1;
erroPermitido = sqrt(10) * 1e-4;

while ( ( erroMax > erroPermitido )  && ( passos < 100 ) )
	n = n + 1;
	passos = passos + 1;
	
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
	
	erroMac = abs( yE - yMac );
	erroMax =  max( erroMac );
end

n
erroMax


% plot( xP, yE, "b;f(x) = cos(x);", xP, yMac, "k;Série de Maclaurin;" );
%plot( xP, yE, xP, yMac ); %"k;Erro(x) = |f(x) - Pn(x)|;"
%title('Fun��o Original vs. Aproxima��o por S�rie de Maclaurin')
%xlabel('xP')
%ylabel('yP')
%hleg1 = legend('f(x) = cos(x)','S�rie de Maclaurin');

plot( xP, erroMac ); %"k;Erro(x) = |f(x) - Pn(x)|;"
title('Gr�fico do Erro da S�rie de Maclaurin')
xlabel('xP')
ylabel('yP')
hleg1 = legend('Erro(x) = |f(x) - Maclaurin(x)|');