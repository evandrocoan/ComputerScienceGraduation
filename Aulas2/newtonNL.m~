n = 2;
xi = [1 -1];
erromax = eps; lim = 20;
erro = 1; passo = 0;
while (erro > erromax && passo < lim)
	passo = passo+1;
	%inicio do núcleo do método de newton
   A = [exp(xi(1)) 1       -f1(xi);
		  2*xi(1)    2*xi(2) -f2(xi);];

	dx = fGauss (n, A);

	x = xi.+dx;
	xi = x;
	%fim do núcleo do método de newton
	erro = max(abs(dx));
end
x
erro
passo
