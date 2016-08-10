% método de escalonamento de Gauss

A = [	1 2 1 12; 
		1 -3 5 1; 
		2 -1 3 10; ]

B = [	0 2 3 4; 
		5 6.2 7 8; 
		0 4 6 50; ]


C = [	0 2 3 4; 
		5 6.2 7 8; 
		9 0.5 10 2; ]

n = rows( A )
Aaux = A;

x = fGauss (n, A)

resMax = fResiduo(n, Aaux, x)
