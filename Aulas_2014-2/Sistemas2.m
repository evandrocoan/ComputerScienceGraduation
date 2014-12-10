% método de escalonamento de Gauss
format long
A = [	1 2 3; 
		1 -2 1; 
		1 0 -2; ]

%Resolvendo 4 sistemas Ax = b, com mesma matriz A
b = [ -1 1  1  1;
		-2 2  2  2;
		5 5  5  5;
	]

n = size( A, 1 )
Aaux = [ A b ];
A= fLUCrout(n, A)
%Resolve o sistema
x = fSub2( n, A, b )
resMax = fResiduo2(n, Aaux, x)


