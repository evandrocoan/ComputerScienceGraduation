% Determine fun?s aproximadoras de f(x)=sqrt(x), com x entre 1 e 2, atrav?de Interpolador Polinomial, de s?es de MacLaurin, Chebyschev e Pad?encontre a melhor combina? 'n' e 'm' de Pad? com erro m?mo EXATO da ordem de 1e-6.
% Lembre-se de primeiramente de fazer mudan?de vari?is de x entre [1;2] para t entre[-1;+1], fazendo todos as aproxima?s em t, e somente no final, retorne as vari?is de t para x, apresentando a forma final das s?es em fun? de x.
% Imprima o erro m?mo em cada aproxima?.
% Plote gr?cos comparativos das fun?s aproximadoras e dos erros em rela? ?un? exata.

clc
clear
format long

% Intervalo definido no enunciado do trabalho
a = 1;
b = 2;

n = 3				%numero de subdivis??do intervalo [a,b] (grau do polinomio)
h = ( b - a ) / n;	% espa?ento entre as subdivis??x = a : h : b;		%vetor x de pontos para plotar o gr?co
y = sqrt(x);		%vetor y de pontos para plotar o gr?co

%matriz que resulta de uma fun? diferen? divididas
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
	yPlot(i) = fPnGregNew( n, x, y, difDiv, xPlot(i) );
end
erroGregNew = abs( yE .- yPlot );

ErroMaxInter = max(erroGregNew)

%Aproxima? em s?es de Maclaurin com n = 14
nMaclaurin = 5
cMaclaurin = fCoefMaclaurin( nMaclaurin, a, b ); %coeficientes da s?e de Maclaurin em t
%fMac(t) = c(1) + c(2) * t^1 + c(3) * t^2 + ...

%plotagem dos pontos da s?e de Maclaurin
for i = 1 : nPlotagem + 1
	tP(i) = ( 2 * xPlot(i) - ( b+a ) ) / (b - a);
	yMac(i) = fPnBrio( nMaclaurin, cMaclaurin, tP(i) );
end
erroMac = abs( yE .- yMac );
erroMaximoMac =  max( erroMac )

plot(x, y, "b;f(x) = sqrt(x);", xPlot, yPlot, "g;g(x) = Pn(x) de Gregory-Newton;", xPlot, yMac, "r;g(x) = Pn(x) de serie de Maclaurin;");
%plot( x, y, "*;f(x) = ln;", xP, yE, "r;f(x)=ln(x);", xP, yP, "b;g(x) = Pn(x) de Lagrange;", xP, yMac, "k;S?e de Maclaurin;" );

%plot( xPlot, erro, "k;Erro(x) = |f(x) - Pn(x)|;" );
