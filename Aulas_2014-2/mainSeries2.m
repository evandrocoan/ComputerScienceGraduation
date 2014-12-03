%Interpolação polinomial por Gregory-Newton
clc
clear
a = 1;
b = 2;
n = 3 %numero de subdivisões do intervalo [a,b] (grau do polinomio)
h = ( b - a ) / n; % espaçamento entre as subdivisões
x = a : h : b; %vetor de pontos para plotar o gráfico
y = log(x);

%matriz que resulta de uma função diferenças divididas
difDiv = fDifDiv( n, x, y );
difDiv(n+1,n) = 0;
[x' y' difDiv ];

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
erroInter = abs( yE .- yP );

ErroMaxInter = max(erroInter)

%Aproximação em séries de Maclaurin com n = 14
nMaclaurin = 5
cMaclaurin = fCoefMaclaurin( nMaclaurin, a, b ); %coeficientes da série de Maclaurin em t
%fMac(t) = c(1) + c(2) * t^1 + c(3) * t^2 + ...

%plotagem dos pontos da série de Maclaurin
for i = 1 : nPlotagem + 1
	%é necessario determiniar os tP correspondentes aos xP anteriores
	tP(i) = ( 2 * xP(i) - ( b+a ) ) / (b - a);
	%polinomio de maclaurin -> fMac(t) = c(1) + c(2) * t^1 + c(3) * t^2 + ...
	yMac(i) = fPnBrio( nMaclaurin, cMaclaurin, tP(i) );
end
erroMaclaurin = abs( yE .- yMac );
erroMaximoMaclaurin =  max( erroMaclaurin )

%Aproximação por Chebychev
%PnMaclarin(t) = 4.05465108108164e-01*t^0+3.33333333333333e-01*t^1-5.55555555555556e-02*t^2+1.23456790123457e-02*t^3-3.08641975308642e-03*t^4+8.23045267489712e-04t^5
%PnMaclaurin(t) = 4.05465108108164e-01*T0+3.33333333333333e-01*T1-5.55555555555556e-02*(T2+T0 )/2+1.23456790123457e-02*(T3+3*T1)/4-3.08641975308642e-03*(T4+4*T2+3*T0)/8+8.23045267489712e-04*(T5+5*T3+10*T1)/16 
%PnMaclaurin em termos do T (maiúsculo)
%0.376529922922979*T0+0.343106995884773*T1-0.0293209876543210*T2+0.00334362139917696*T3-0.000385802469135802*T4+0.000051440329218107*T5
%Vamos tentar desprezar o último termo da serie de maclaurin escrita em função de T (maiusculo):0.000051440329218107*T5, acrescentando um erro na ordem de O(10e-4)
%PnChebychev(T) = 0.376529922922979*T0+0.343106995884773*T1-0.0293209876543210*T2+0.00334362139917696*T3-0.000385802469135802*T4
%PnChebychev(t) = -0.00308641975308642*t^4+0.0133744855967078*t^3-0.055555555555556*t^2+0.333076131687242*t+0.40546510810816
nChebychev = 4
cChebychev = [ 0.405465108108164 0.333076131687242 -0.055555555555556 0.0133744855967078 -0.00308641975308642 ]

%plotagem dos pontos da série de Chebychev
for i = 1 : nPlotagem + 1
	%é necessario determiniar os tP correspondentes aos xP anteriores
	tP(i) = ( 2 * xP(i) - ( b+a ) ) / (b - a);
	%polinomio de maclaurin -> fMac(t) = c(1) + c(2) * t^1 + c(3) * t^2 + ...
	yChebychev(i) = fPnBrio( nChebychev, cChebychev, tP(i) ); %calcula a serie de cheb... no ponto tP
end
erroChebychev = abs( yE .- yChebychev );
erroMaximoChebychev =  max( erroChebychev )

%aproximação racional de Padé
c = cMaclaurin
M = nMaclaurin %M = 5 
n = 3 % R32 (Padé)
m = 2 %

%Primeiro determinamos os coeficiente de bj
%A = [ c(n-m+1), c(n-m+2), -c(n+1);
%		c(M-m),   c(M-1),   -c(M)  ; ]
A = [ c(n-m+1+1), c(n-m+2+1), -c(n+1+1);
		c(M-m+1),   c(M-1+1),   -c(M+1)  ; ]
baux = fGauss(m, A)
bp = flipud(baux')

bp(3) = 0; %porque o polinomio de denominador é igual a 2 (m)
% ap = aPade
ap(0+1) = c(0+1);
ap(1+1) = bp(1) * c(0+1) + c(1+1);
ap(2+1) = bp(2) * c(0+1) + bp(1) * c(1+1) + c(2+1);
ap(3+1) = bp(3) * c(0+1) + bp(2) * c(1+1) + bp(1) * c(2+1) + c(3+1);

ap
bp = [ 1; bp]'

%plotagem dos pontos da série de Padé
for i = 1 : nPlotagem + 1
	%é necessario determiniar os tP correspondentes aos xP anteriores
	%tP = tê de Plotagem correspondente ao xîs
	tP(i) = ( 2 * xP(i) - ( b+a ) ) / (b - a);
	%polinomio de maclaurin -> fMac(t) = c(1) + c(2) * t^1 + c(3) * t^2 + ...
	%calcula a serie racional de Padé no ponto tP
	yPade(i) = fPnBrio( n, ap, tP(i) ) / fPnBrio( m, bp, tP(i) ); 
end

erroPade = abs( yE .- yPade );
erroMaximoPade =  max( erroPade )

%plot( x, y, "*;f(x) = ln;", xP, yE, "r;f(x)=ln(x);", xP, yP, "b;g(x) = Pn(x) de Lagrange;", xP, yMac, "k;Série de Maclaurin;", xP, yChebychev, "g;P(x) = PnChebychev(x);", xP, yPade, "y;yPade(x);" );
plot( xP, erroInter, "k;Erro(x) = |f(x) - Pn(x)|;", xP, erroMaclaurin, "r;Erro(x) = |f(x) - PnMaclaurin(x)|;", xP, erroChebychev, "b;Erro(x) = |f(x) - PnChebychev(x)|;", xP, erroPade, "k;Erro(x) = |f(x) - PnPade(x)|;" );
