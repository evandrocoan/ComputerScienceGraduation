%Aproximação Racional de Padé
clc
clear
format long
a = -1;
b = 1;
M = 4;
n = M;

h = ( b - a ) / n; 
x = a : h : b;
y = cos(x);
nPad = 2;
mPad = 2;
c = fCoefMaclaurin(M);


A = [ c(nPad-mPad+1+1),      c(nPad-mPad+2+1),     -c(nPad+1+1);
	     c(M-mPad+1),               c(M-1+1),                    -c(M+1)         ; 
		];
baux = fGauss(mPad, A);
bp = flipud(baux');

% ap = aPade
ap(0+1) = c(0+1);
ap(1+1) = bp(1) * c(0+1) + c(1+1);
ap(2+1) = bp(2) * c(0+1) + bp(1) * c(1+1) + c(2+1);

%acrescenta o valor do primeiro coeficiente já conhecido '1', 
%para obtermos uma única solução
bp = [ 1; bp]';

nPlot = 50 * n; 
aPlot = a;
bPlot = b;
hPlot = ( bPlot - aPlot ) / nPlot;
xP = aPlot: hPlot : bPlot; 
yE = f(xP);
	
for i = 1 : nPlot + 1
	yPade(i) = fPnBrio( nPad, ap, xP(i) ) / fPnBrio( mPad, bp, xP(i) ); 
end

erroPade = abs( yE .- yPade );
erroMax =  max( erroPade )

ap
bp

% plot( xP, yE, "b;f(x) = cos(x);", xP, yPade, "k;Série de Pade;" );
plot( xP, erroPade, "k;Erro(x) = |f(x) - Pn(x)|;" );
