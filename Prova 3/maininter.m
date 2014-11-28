%Interpolação polinomial usando a base canonica dos polinomios
clc
clear
a = 2;
b = 2.15;
n = 3; %numero de subdivisões do intervalo [a,b]
h = ( b - a ) / n; % espaçamento entre as subdivisões
x = a : h : b;
y = log(x);

coef = fInterCoef( n, x, y )

%plotagem dos pontos 
nPlotagem = 20 * n; %3 intervalos dividos em 20 vezes
hPlotagem = ( b - a ) / nPlotagem;
xP = a: hPlotagem : b; % 31 pontos para n = 3

yE = log(xP);

for i = 1 : nPlotagem + 1
	yP(i) = fPnBrio( n, coef, xP(i) );
end
erro = abs( yE .- yP )

ErroMax = max(erro)
%plot( x, y, "*;f(x) = ln;", xP, yP, "b;g(x) = Pn(x);" );
plot( xP, erro, "k;Erro(x) = |f(x) - Pn(x)|;" );
