clc
clear
format long

% Intervalo definido no enunciado do trabalho
a = 1;
b = 2;

n = 3				%numero de subdivisoes do intervalo [a,b] (grau do polinomio)
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
nMac = 5
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
% 1.22474487139159e+00   2.04124145231932e-01  -1.70103454359943e-02   2.83505757266572e-03  -5.90636994305357e-04   1.37815298671250e-04
% cMac aplicado em t:
%1.22474487139159*t^0+0.204124145231932*t^1-0.0170103454359943*t^2+0.00283505757266572*t^3-0.000590636994305357*t^4+0.000137815298671250*t^5
% cMac aplicado em T:
% 1.21601820980073*T0+0.206336572973101*T1-0.0088004912151498*T2+0.00075183167400120*T3-0.000073829624288170*T4+8.6134561669531e-6*T5
% Retornando ao t:
% 1.2247448713916+0.20412414523193*t-0.017010345435994*t^2+0.0028350575726657*t^3-0.0005906369943054*t^4+0.00013781529867125*t^5

nCheb = 5
cCheb = [1.2247448713916 0.20412414523193 -0.017010345435994 0.0028350575726657 -0.0005906369943054 +0.00013781529867125];

%plotagem dos pontos da série de Chebychev
for i = 1 : nPlotagem + 1
	tP(i) = (2*xPlot(i)-(b+a)) / (b-a);
	yCheb(i) = fPnBrio(nCheb, cCheb, tP(i));
end
erroCheb = abs(yE .- yCheb);
erroMaxCheb =  max(erroCheb)


plot(x, y, "b;f(x) = sqrt(x);", xPlot, yGregNew, "g;g(x) = Pn(x) de Gregory-Newton;", xPlot, yMac, "r;g(x) = Pn(x) de serie de Maclaurin;", xPlot, yCheb, "y;g(x) = Pn(x) de Chebyshev;");

% plot(xPlot, erroMac, "y;Erro Chebyshev;", xPlot, erroMac, "r;Erro MacLaurin;", xPlot, erroGregNew, "g;Erro Gregory Newton;");		% Erro Chebyshev
