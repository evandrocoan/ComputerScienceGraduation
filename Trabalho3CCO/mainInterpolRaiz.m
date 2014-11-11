clc
clear
format long

% Intervalo definido no enunciado do trabalho
a = 1;
b = 2;

n = 6				%numero de subdivisoes do intervalo [a,b] (grau do polinomio)
h = ( b - a ) / n;	%espacaento entre as subdivisao x = a : h : b;
x = a : h : b;		%vetor x de pontos para plotar o gráfico		
y = sqrt(x);		%vetor y de pontos para plotar o grafico


% 1. Interpolacao por Gregory-Newton
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

% 2. Interpolação por series de Maclaurin
nMac = 6
cMac = fCoefMaclaurin( nMac, a, b ) %coeficientes da serie de Maclaurin em t=0

%plotagem dos pontos da serie de Maclaurin
for i = 1 : nPlotagem + 1
	tP(i) = (2*xPlot(i)-(b+a)) / (b-a);
	yMac(i) = fPnBrio(nMac, cMac, tP(i));
end
erroMac = abs( yE .- yMac );
erroMaximoMac =  max( erroMac )

% 3. Interpolação por Chebyshev
% cMac:
% 1.22474487139159e+00 2.04124145231932e-01 -1.70103454359943e-02 3.54382196583214e-03 -8.26891792027500e-04 2.06722948006875e-04
% cMac aplicado em t:
%1.22474487139159e+00*(t^0)+2.04124145231932e-01*(t^1)-1.70103454359943e-02*(t^2)+3.54382196583214e-03*(t^3)-8.26891792027500e-04*(t^4)+2.06722948006875e-04*(t^5)
%1.22474487139159+0.204124145231932*t -0.0170103454359943*t^2+0.00354382196583214*t^3-0.000826891792027500*t^40.000206722948006875*t^5
%Transformando t (minusculo) em T (maiusculo)
%1.22474487139159*T0+0.204124145231932*T1 -0.0170103454359943*((T2+T0)/2)+0.00354382196583214*((T3+3*T1)/4)-0.000826891792027500*((T4+4*T2+3*T0)/8)+0.000206722948006875((T5+5*T3+10*T1)/16)
%Simplificação
%0.000206722948006875*t^5-0.000826891792027500*t^4+0.00354382196583214*t^3-0.0170103454359943*t^2+0.204124145231932*t+1.22474487139159
% Transformando de T para t:
%1.2409283250356+0.19452629407450*x-0.027405556535769*x^2+0.010040828903148*x^3-0.006615134336220*x^4+0.0033075671681100*x^5

nCheb = 5
cCheb = [1.2409283250356 +0.19452629407450 -0.027405556535769 +0.010040828903148 -0.006615134336220 +0.0033075671681100];

%plotagem dos pontos da série de Chebychev
for i = 1 : nPlotagem + 1
	tP(i) = (2*xPlot(i)-(b+a)) / (b-a);
	yCheb(i) = fPnBrio(nCheb, cCheb, tP(i));
end
erroCheb = abs(yE .- yCheb);
erroMaxCheb =  max(erroCheb)


 plot(x, y, "b;f(x) = sqrt(x);", xPlot, yGregNew, "g;g(x) = Pn(x) de Gregory-Newton;", xPlot, yMac, "r;g(x) = Pn(x) de serie de Maclaurin;", xPlot, yCheb, "y;g(x) = Pn(x) de Chebyshev;");

% plot(xPlot, erroMac, "y;Erro Chebyshev;", xPlot, erroMac, "r;Erro MacLaurin;", xPlot, erroGregNew, "g;Erro Gregory Newton;");		% Erro Chebyshev
