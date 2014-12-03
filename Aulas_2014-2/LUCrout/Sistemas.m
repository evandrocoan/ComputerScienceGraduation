% método de escalonamento de Gauss

A = [	0 2 3; 
		1 -2 1; 
		1 0 -2; ]

%Resolvendo 4 sistemas Ax = b, com mesma matriz A
b = [ 1; 3;	 5;  4;
		2; 5; -3; -2;
		5; 0;  1;  3;
	]



%n = rows( B )
%Aaux = B;
%x = fGauss (n, B)
%resMax = fResiduo(n, Aaux, x)
n = rows( A );
Aaux = [ A b ];
[A b] = fLUCrout(n, A, b)
%Resolve o sistema
x = fSub1( n, A, b )
resMax = fResiduo(n, Aaux, x)


