clear
format short

N = 3;
A = [1  -1 -1;
	 1   1  1;
	 4   1 -1;]

b = [6;
	 1;
	 2;]
A_original=[A b];

x = fcrout(N, A, b);
r = residuos(N,A_original, x)