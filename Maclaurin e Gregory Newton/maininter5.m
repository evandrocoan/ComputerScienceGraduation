%Interpolação polinomial por Gregory-Newton
clc
clear
a = 1;
b = 2;
n = 7; %numero de subdivisões do intervalo [a,b] (grau do polinomio)
h = ( b - a ) / n; % espaçamento entre as subdivisões
x = a : h : b; %vetor de pontos para plotar o gráfico
y = log(x);

%teste
%n = 3
%x = [ 1; 3; 4; 7; ];
%y = [ 2; 0; 1; 3; ];


%matriz que resulta de uma função diferenças divididas
difDiv = fDifDiv( n, x, y )
difDiv(n+1,n) = 0;
[x' y' difDiv ]

%plotagem dos pontos 
nPlotagem = 50 * n; %3 intervalos dividos em 20 vezes
aPlot = a;
bPlot = b;
hPlotagem = ( bPlot - aPlot ) / nPlotagem;
xP = aPlot: hPlotagem : bPlot; % 31 pontos para n = 3

yE = log(xP);

for i = 1 : nPlotagem + 1
	%yP(i) = fPnLagrange( n, x, y, xP(i) );
	yP(i) = fPnGregNew( n, x, y, difDiv, xP(i) );
end
erro = abs( yE .- yP )

ErroMax = max(erro)
plot( x, y, "*;f(x) = ln;", xP, yE, "r;f(x)=ln(x);", xP, yP, "b;g(x) = Pn(x) de Lagrange;" );
%plot( xP, erro, "k;Erro(x) = |f(x) - Pn(x)|;" );
