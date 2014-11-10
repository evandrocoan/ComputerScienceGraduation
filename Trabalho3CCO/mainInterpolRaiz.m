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
	tP(i) = ( 2 * xPlot(i) - ( b+a ) ) / (b - a);
	yMac(i) = fPnBrio( nMac, cMac, tP(i) );
end
tP;
xPlot;
yMac;
erroMac = abs( yE .- yMac );
erroMaximoMac =  max( erroMac )

 plot(x, y, "b;f(x) = sqrt(x);", xPlot, yGregNew, "g;g(x) = Pn(x) de Gregory-Newton;", xPlot, yMac, "r;g(x) = Pn(x) de serie de Maclaurin;");

% plot(xPlot, erroGregNew, "b;Erro Gregory Newton;")	% Erro Gregory-Newton
%plot(xPlot, erroMac, "r;Erro MacLaurin;");		% Erro Maclaurin
