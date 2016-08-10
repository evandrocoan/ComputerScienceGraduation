%Interpolação polinomial por polinomios de lagrange
clc
clear
a = 1;
b = 2;
n = 7; %numero de subdivisões do intervalo [a,b] (grau do polinomio)
h = ( b - a ) / n; % espaçamento entre as subdivisões
x = a : h : b; %vetor de pontos para plotar o gráfico
y = log(x);

%plotagem dos pontos 
nPlotagem = 20 * n; %3 intervalos dividos em 20 vezes
hPlotagem = ( b - a ) / nPlotagem;
xP = a : hPlotagem : b; % 31 pontos para n = 3

yE = log(xP);

for i = 1 : nPlotagem + 1
	yP(i) = fPnLagrange( n, x, y, xP(i) );
end
erro = abs( yE .- yP )

ErroMax = max(erro)
plot( x, y, "*;f(x) = ln;", xP, yP, "b;g(x) = Pn(x) de Lagrange;" );
%plot( xP, erro, "k;Erro(x) = |f(x) - Pn(x)|;" );
