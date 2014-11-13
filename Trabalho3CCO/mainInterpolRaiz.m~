clc
clear
format long

% Intervalo definido no enunciado do trabalho
a = 1;
b = 2;

n = 3;			%numero de subdivisoes do intervalo [a,b] (grau do polinomio)
h = ( b - a ) / n;	%espacaento entre as subdivisao x = a : h : b;
x = a : h : b;		%vetor x de pontos para plotar o gráfico		
y = sqrt(x);		%vetor y de pontos para plotar o grafico


% 1. Interpolacao por Gregory-Newton
printf("1. Interpolação pelo método de Gregory-Newton\n");
n
%matriz que resulta de uma funcao de diferencas divididas
difDiv = fDifDiv( n, x, y );
difDiv(n+1,n) = 0;
[x' y' difDiv ];

%plotagem dos pontos
nPlotagem = 50 * n; %n intervalos dividos em 50 vezes
aPlot = a;
bPlot = b;
hPlotagem = ( bPlot - aPlot ) / nPlotagem;
xPlot = aPlot: hPlotagem : bPlot; % 31 pontos para n = 3
yE = sqrt(xPlot);

for i = 1 : nPlotagem + 1
	yGregNew(i) = fPnGregNew( n, x, y, difDiv, xPlot(i) );
end
erroGregNew = abs( yE .- yGregNew );
erroMaxGregNew = max(erroGregNew)
printf("\n-------------------------\n")

% 2. Aproximação por series de Maclaurin
printf("2. Aproximação por Séries de Maclaurin\n");
nMac = 5
cMac = fCoefMaclaurin( nMac, a, b ); %coeficientes da serie de Maclaurin em t=0

%plotagem dos pontos da serie de Maclaurin
for i = 1 : nPlotagem + 1
	tP(i) = (2*xPlot(i)-(b+a)) / (b-a);
	yMac(i) = fPnBrio(nMac, cMac, tP(i));
end
erroMac = abs( yE .- yMac );
erroMaxMac =  max( erroMac )
printf("\n-------------------------\n")

% 3. Aproximação por Chebyshev
printf("3. Aproximação por Séries de Chebyshev\n");
% cMac:
% 1.22474487139159e+00   2.04124145231932e-01  -1.70103454359943e-02   2.83505757266572e-03  -5.90636994305357e-04   1.37815298671250e-04
% cMac aplicado em t:
%1.22474487139159*t^0+0.204124145231932*t^1-0.0170103454359943*t^2+0.00283505757266572*t^3-0.000590636994305357*t^4+0.000137815298671250*t^5
% cMac aplicado em T:
% 1.21601820980073*T0+0.206336572973101*T1-0.0088004912151498*T2+0.00075183167400120*T3-0.000073829624288170*T4+8.6134561669531e-6*T5
% Retornando ao t:
% 1.2247448713916+0.20412414523193*t-0.017010345435994*t^2+0.0028350575726657*t^3-0.0005906369943054*t^4+0.00013781529867125*t^5
% Foi desprezado o último coeficiente pois o erro permanece ainda aceitável.

nCheb = 4
cCheb = [1.2247448713916 0.20412414523193 -0.017010345435994 0.0028350575726657 -0.0005906369943054];

%plotagem dos pontos da série de Chebychev
for i = 1 : nPlotagem + 1
	tP(i) = (2*xPlot(i)-(b+a)) / (b-a);
	yCheb(i) = fPnBrio(nCheb, cCheb, tP(i));
end
erroCheb = abs(yE .- yCheb);
erroMaxCheb =  max(erroCheb)
printf("\n-------------------------\n")

% 4. Aproximação racional de Padé
printf("4. Aproximação Racional de Padé\n");
c = cMac;
M = nMac;
nPad = 3 % R32 (Padé)
mPad = 2 %

%Primeiro determinamos os coeficiente de bj
%A = [ c(n-m+1), c(n-m+2), -c(n+1);
%		c(M-m),   c(M-1),   -c(M)  ; ]
A = [ c(nPad-mPad+1+1), c(nPad-mPad+2+1), -c(nPad+1+1);
	c(M-mPad+1),   c(M-1+1),   -c(M+1)  ; ];
baux = fGauss(mPad, A);
bp = flipud(baux');

bp(3) = 0; %porque o polinomio de denominador é igual a 2 (m)
% ap = aPade
ap(0+1) = c(0+1);
ap(1+1) = bp(1) * c(0+1) + c(1+1);
ap(2+1) = bp(2) * c(0+1) + bp(1) * c(1+1) + c(2+1);
ap(3+1) = bp(3) * c(0+1) + bp(2) * c(1+1) + bp(1) * c(2+1) + c(3+1);

bp = [ 1; bp]';

%plotagem dos pontos da série de Padé
for i = 1 : nPlotagem + 1
	%é necessario determiniar os tP correspondentes aos xP anteriores
	%tP = tê de Plotagem correspondente ao xîs
	tPlot(i) = ( 2 * xPlot(i) - ( b+a ) ) / (b - a);
	%polinomio de maclaurin -> fMac(t) = c(1) + c(2) * t^1 + c(3) * t^2 + ...
	%calcula a serie racional de Padé no ponto tP
	yPad(i) = fPnBrio( nPad, ap, tP(i) ) / fPnBrio( mPad, bp, tP(i) ); 
end

erroPad = abs( yE .- yPad );
erroMaxPad =  max( erroPad )
printf("\n-------------------------\n")

% 5 Interpolação Polinomial usando Gauß
printf("5. Interpolação Polinomial usando método de Gauß\n");
nGauss = 3
cGauss = fInterCoef( nGauss, x, y );

%plotagem dos pontos
for i = 1 : nPlotagem + 1
	yGauss(i) = fPnBrio(nGauss, cGauss, xPlot(i));
end
erroGauss = abs( yE .- yGauss );
erroMaxGauss =  max( erroGauss )
printf("\n-------------------------\n")

% 6 Interpolação Polinomial usando Newton-Raphson
printf("6. Interpolação Polinomial usando método de Newton-Raphson\n");
%plotagem dos pontos
erroPropNR = 1.e-2;
for i = 1 : nPlotagem + 1
	yNR(i) = fSqrtNR(xPlot(i), erroPropNR);
end
erroNR = abs( yE .- yNR );
erroMaxNR =  max( erroNR )
printf("\n-------------------------\n")

printf("Plotando os Gráficos");

% Plot das Interpolações
plot(xPlot, yCheb, "y;Chebyshev;", xPlot, yMac, "r;MacLaurin;", xPlot, yGregNew, "g;Gregory-Newton;", xPlot, yPad, "m;Padé;", xPlot, yGauss, "c;Gauß;", xPlot, yNR, "k;Newton-Raphson;");

% plot(x, y, "b;f(x) = sqrt(x);", xPlot, yGregNew, "g;g(x) = Pn(x) de Gregory-Newton;")	 		- verde
% plot(x, y, "b;f(x) = sqrt(x);", xPlot, yMac, "r;g(x) = Pn(x) de serie de Maclaurin;")	 		- vermelho
% plot(x, y, "b;f(x) = sqrt(x);", xPlot, yCheb, "y;g(x) = Pn(x) de Chebyshev;") 		 		- amarelo
% plot(x, y, "b;f(x) = sqrt(x);", xPlot, yPad, "m;g(x) = Pn(x) de Padé;")			 		- rosa
% plot(x, y, "b;f(x) = sqrt(x);", xPlot, yGauss, "c;g(x) = Interpolador Polinomial (Gauß);") 		- azul claro
% plot(x, y, "b;f(x) = sqrt(x);", xPlot, yNR, "k;g(x) = Interpolador Polinomial (Newton-Raphson);") 	- preto

% Plot de erros
plot(xPlot, erroCheb, "y;Erro Chebyshev;", xPlot, erroMac, "r;Erro MacLaurin;", xPlot, erroGregNew, "g;Erro Gregory Newton;", xPlot, erroPad, "m;Erro Padé;", xPlot, erroGauss, "c;g(x) = Erro Gauß;", xPlot, erroNR, "k;g(x) = Erro Newton-Raphson;");
