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
c = fMaclaurin(M);


A = [ c(nPad-mPad+1+1), c(nPad-mPad+2+1), -c(nPad+1+1);
	c(M-mPad+1),       c(M-1+1),         -c(M+1)  ; ];
baux = fGauss(mPad, A);
bp = flipud(baux');

% ap = aPade
ap(0+1) = c(0+1);
ap(1+1) = bp(1) * c(0+1) + c(1+1);
ap(2+1) = bp(2) * c(0+1) + bp(1) * c(1+1) + c(2+1);

bp = [ 1; bp]';

for i = 1 : n + 1
	yPade(i) = fPnBrio( nPad, ap, x(i) ) / fPnBrio( mPad, bp, x(i) ); 
end


erroPade = abs( y .- yPade );
erroMax =  max( erroPade )




 plot( x, y, "b;f(x) = cos(x);", x, yPade, "k;Série de Pade;" );
%plot( x, erroPade, "k;Erro(x) = |f(x) - Pn(x)|;" );
