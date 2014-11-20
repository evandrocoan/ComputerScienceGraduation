%Aproximação por Séries Maclaurin
clc
clear
format long
a = -1;
b = 1;
n = 0; %numero de subdivisões do intervalo [a,b] (grau do polinomio)
passos = 0;
erroMax = 1;
erroPermitido = sqrt(10) * 1e-4;

while ( ( erroMax > erroPermitido )  && ( passos < 1000 ) )
	n += 1;
	passos+=1;
	
	h = ( b - a ) / n; 
	x = a : h : b;
	y = cos(x);
	
	cMac = fMaclaurin(n);
	
	for i = 1 : n + 1
		yMac(i) = fPnBrio(n, cMac, x(i));
	end
	
	erroMac = abs( y .- yMac );
	erroMax =  max( erroMac );
end

n
erroMax
cMac


 plot( x, y, "b;f(x) = cos(x);", x, yMac, "k;Série de Maclaurin;" );
%plot( x, erroMac, "k;Erro(x) = |f(x) - Pn(x)|;" );
