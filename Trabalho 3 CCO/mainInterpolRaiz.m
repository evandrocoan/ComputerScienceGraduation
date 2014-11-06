% Determine funções aproximadoras de f(x)=sqrt(x), com x entre 1 e 2, através de Interpolador Polinomial, de séries de MacLaurin, Chebyschev e Padé (encontre a melhor combinação 'n' e 'm' de Padé), com erro máximo EXATO da ordem de 1e-6.
% Lembre-se de primeiramente de fazer mudança de variáveis de x entre [1;2] para t entre[-1;+1], fazendo todos as aproximações em t, e somente no final, retorne as variáveis de t para x, apresentando a forma final das séries em função de x.
% Imprima o erro máximo em cada aproximação.
% Plote gráficos comparativos das funções aproximadoras e dos erros em relação à função exata.

clc
clear
format long

% Intervalo definido no enunciado do trabalho
a = 1;
b = 2;

n = 4;			%numero de subdivisões do intervalo [a,b] (grau do polinomio)
h = ( b - a ) / n;	% espaçamento entre as subdivisões
x = a : h : b;		%vetor x de pontos para plotar o gráfico
y = sqrt(x);		%vetor y de pontos para plotar o gráfico

%matriz que resulta de uma função diferenças divididas
difDiv = fDifDiv( n, x, y )
difDiv(n+1,n) = 0;
[x' y' difDiv ]

%plotagem dos pontos 
nPlotagem = 50 * n; %n intervalos dividos em 20 vezes
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

ErroMaxInter = max(erro)

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

plot( x, y, "*;f(x) = ln;", xP, yE, "r;f(x)=ln(x);", xP, yP, "b;g(x) = Pn(x) de Lagrange;", xP, yMac, "k;Série de Maclaurin;" );
%plot( xP, erro, "k;Erro(x) = |f(x) - Pn(x)|;" );
