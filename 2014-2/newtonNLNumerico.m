%método de newton com derivada numérica
clc 
clear
format long
n = 2;
xi = [1 -1];
erromax = 1.e-6; lim = 20;
erro = 1; passo = 0;
while (erro > erromax && passo < lim)
	passo = passo+1;
	dx = [1.e-6 1.e-6 ];
	%inicio do núcleo do método de newton
A=[(f1([xi(1)+dx(1),xi(2)])-f1([xi(1),xi(2)]))/dx(1), (f1([xi(1),xi(2)+dx(2)])-f1([xi(1),xi(2)]))/dx(2), -f1(xi);
	(f2([xi(1)+dx(1),xi(2)])-f2([xi(1),xi(2)]))/dx(1), (f2([xi(1),xi(2)+dx(2)])-f2([xi(1),xi(2)]))/dx(2), -f2(xi);
  ];

	dx = fGauss (n, A);

	x = xi.+dx;
	xi = x;
	%fim do núcleo do método de newton
	erro = max(abs(dx));
end
x
erro
passo
