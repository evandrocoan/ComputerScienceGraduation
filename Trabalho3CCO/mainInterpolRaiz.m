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
cMac = fCoefMaclaurin( nMac, a, b ); %coeficientes da serie de Maclaurin em t=0

%plotagem dos pontos da serie de Maclaurin
for i = 1 : nPlotagem + 1
	tP(i) = (2*xPlot(i)-(b+a)) / (b-a);
	yMac(i) = fPnBrio(nMac, cMac, tP(i));
end
erroMac = abs( yE .- yMac );
erroMaximoMac =  max( erroMac )

% 3. Interpolação por Chebyshev
%PnMaclaurin(t) = 1.22474487139159*t^0+0.204124145231932*t^1-0.0170103454359943*t^2+0.00354382196583214*t^3-8.26891792027500e-04*t^4
% → 1.22474487139159 + 0.204124145231932*t - 0.0170103454359943*t^2 + 0.00354382196583214*t^3 - 0.000826891792027500*t^4
% 1.22474487139159*T0+0.204124145231932*T1-0.0170103454359943*(T2+T0)+0.00354382196583214*((T3+3*T1)/4)-0.000826891792027500*((T4+4*T2+3*T0)/8)
% → 1.20742444153359*T0+0.206782011706306*T1-0.0174237913320081*T2+0.000885955491458035*T3-0.000103361474003438*T4

nCheb = 4
cCheb = [1.20742444153359 0.206782011706306 -0.0174237913320081 0.000885955491458035 -0.000103361474003438];

%plotagem dos pontos da série de Chebychev
for i = 1 : nPlotagem + 1
	tP(i) = (2*xPlot(i)-(b+a)) / (b-a);
	yCheb(i) = fPnBrio(nCheb, cCheb, tP(i));
end
erroCheb = abs(yE .- yCheb);
erroMaxCheb =  max(erroCheb)


% plot(x, y, "b;f(x) = sqrt(x);", xPlot, yGregNew, "g;g(x) = Pn(x) de Gregory-Newton;", xPlot, yMac, "r;g(x) = Pn(x) de serie de Maclaurin;", xPlot, yCheb, "y;g(x) = Pn(x) de Chebyshev;");

 plot(xPlot, erroMac, "y;Erro Chebyshev;", xPlot, erroMac, "r;Erro MacLaurin;", xPlot, erroGregNew, "g;Erro Gregory Newton;");		% Erro Chebyshev
