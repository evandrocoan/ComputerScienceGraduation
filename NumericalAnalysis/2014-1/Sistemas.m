clear
format short

N = 4;
A = [1.5  -1 -1 6  -4;
	 1.7   1  1 1   5;
	 4.21  1 -1 2   2;
	 7.8   3 -2 1.7 1;]
A_original=A;
	 
x = fgauss(N, A, A_original);

r = residuos(N,A_original, x)