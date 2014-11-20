%Interpolação polinomial por Gregory-Newton
clc
clear
a = 1;
b = 2;
n = 7; %numero de subdivisões do intervalo [a,b] (grau do polinomio)
h = ( b - a ) / n; % espaçamento entre as subdivisões
x = a : h : b; %vetor de pontos para plotar o gráfico

%Aproximação em séries de Maclaurin com n = 14
nMaclaurin = 2
cMaclaurin = fCoefMaclaurin( nMaclaurin, a, b ); %coeficientes da série de Maclaurin em t
%fMac(t) = c(1) + c(2) * t^1 + c(3) * t^2 + ...

%plotagem dos pontos da série de Maclaurin
for i = 1 : nPlotagem + 1
	tP(i) = ( 2 * xP(i) - ( b+a ) ) / (b - a);
	yMac(i) = fPnBrio( nMaclaurin, cMaclaurin, tP(i) );
end
erroMaclaurin = abs( yE .- yMac );
erroMaximoMaclaurin =  max( erroMaclaurin )

%plot( x, y, "*;f(x) = ln;", xP, yE, "r;f(x)=ln(x);", xP, yP, "b;g(x) = Pn(x) de Lagrange;", xP, yMac, "k;Série de Maclaurin;" );
%plot( xP, erro, "k;Erro(x) = |f(x) - Pn(x)|;" );
