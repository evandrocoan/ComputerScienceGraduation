% Determine fun��es aproximadoras de f(x)=sqrt(x), com x entre 1 e 2, atrav�s de Interpolador Polinomial, de s�ries de MacLaurin, Chebyschev e Pad� (encontre a melhor combina��o 'n' e 'm' de Pad�), com erro m�ximo EXATO da ordem de 1e-6.
% Lembre-se de primeiramente de fazer mudan�a de vari�veis de x entre [1;2] para t entre[-1;+1], fazendo todos as aproxima��es em t, e somente no final, retorne as vari�veis de t para x, apresentando a forma final das s�ries em fun��o de x.
% Imprima o erro m�ximo em cada aproxima��o.
% Plote gr�ficos comparativos das fun��es aproximadoras e dos erros em rela��o � fun��o exata.

clc
clear
format long

% Intervalo definido no enunciado do trabalho
a = 1;
b = 2;

n = 4;			%numero de subdivis�es do intervalo [a,b] (grau do polinomio)
h = ( b - a ) / n;	% espa�amento entre as subdivis�es
x = a : h : b;		%vetor x de pontos para plotar o gr�fico
y = sqrt(x);		%vetor y de pontos para plotar o gr�fico

%matriz que resulta de uma fun��o diferen�as divididas
difDiv = fDifDiv( n, x, y );
difDiv(n+1,n) = 0;
[x' y' difDiv ];

%plotagem dos pontos 
nPlotagem = 20 * n; %n intervalos dividos em 20 vezes
aPlot = a;
bPlot = b;
hPlotagem = ( bPlot - aPlot ) / nPlotagem;
xP = aPlot: hPlotagem : bPlot; % 31 pontos para n = 3

yE = sqrt(xP);

for i = 1 : nPlotagem + 1
	yP(i) = fPnGregNew( n, x, y, difDiv, xP(i) );
end
erro = abs( yE .- yP );

ErroMaxInter = max(erro)

%Aproxima��o em s�ries de Maclaurin com n = 14
nMaclaurin = 4
cMaclaurin = fCoefMaclaurin( nMaclaurin, a, b ); %coeficientes da s�rie de Maclaurin em t
%fMac(t) = c(1) + c(2) * t^1 + c(3) * t^2 + ...

%plotagem dos pontos da s�rie de Maclaurin
for i = 1 : nPlotagem + 1
	tP(i) = ( 2 * xP(i) - ( b+a ) ) / (b - a);
	yMac(i) = fPnBrio( nMaclaurin, cMaclaurin, tP(i) );
end
erroMaclaurin = abs( yE .- yMac );
erroMaximoMaclaurin =  max( erroMaclaurin )

plot( x, y, "*;f(x) = ln;", xP, yMac, "k;Serie de Maclaurin;" );
%plot( xP, erro, "k;Erro(x) = |f(x) - Pn(x)|;" );
