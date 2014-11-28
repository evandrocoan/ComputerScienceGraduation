%InterpolaÃ§Ã£o polinomial por Gregory-Newton
clc
clear
format long
a = -1;
b = 1;
n = 0; %numero de subdivisÃµes do intervalo [a,b] (grau do polinomio)
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
	
	%cÃ¡lculo do erro por 
	erro = abs( yE - yP );
	erroMax = max(erro);
end

erroMax
n

plot( xP, yE, xP, yP );
title('Função Original vs. Função Interpolada')
xlabel('xP')
ylabel('yP')
hleg1 = legend('f(x) = cos(x)','g(x) = Pn(x) de Gregory-Newton');
%plot( xP, erro ); %"k;Erro(x) = |f(x) - Pn(x)|;"
%title('Gráfico do Erro da interpolação')
%xlabel('xP')
%ylabel('yP')
%hleg1 = legend('Erro(x) = |f(x) - Pn(x)|');