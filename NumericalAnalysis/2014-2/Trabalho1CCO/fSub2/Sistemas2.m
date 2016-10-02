% método de escalonamento de Gauss
clear
clc
A = [	0 2 3; 
	1 -2 1; 
	1 0 -2; ]

%Resolvendo 4 sistemas Ax = b, com mesma matriz A
b = [ -1  1  1 -1;
	 -2 -2  2  2;
	  5  5 -5  5;
	]

n = rows( A );
Aaux = [ A b ];
[A b] = fLUCrout(n, A, b)
%Resolve o sistema
x = fSub2( n, A, b )
resMax = fResiduo2(n, Aaux, x)


