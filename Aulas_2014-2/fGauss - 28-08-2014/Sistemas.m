% método de escalonamento de Gauss

A = [	10 2 3; 
		1 -2 1; 
		1 0 -2; ]

b = [1; 2; 5;]

D = [	1 2 1 12; 
		1 -3 5 1; 
		2 -1 3 10; ]

B = [	0 2 3 4; 
		5 6.2 7 8; 
		0 4 6 50; ]


C = [	0 2 3 4; 
		5 6.2 7 8; 
		9 0.5 10 2; ]

%n = rows( B )
%Aaux = B;
%x = fGauss (n, B)
%resMax = fResiduo(n, Aaux, x)
n = rows( A );
Aaux = [ A b ];
A = fLUCrout(n, A)
%Resolve o sistema
x = fSub1( n, A, b )
resMax = fResiduo(n, Aaux, x)













