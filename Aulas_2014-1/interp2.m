clc
clear all
close all
N = 2;
a = 1;
b = 2;

h = (b - a)/N; # passo
x = a:h:b;
y = log(x);

#met1 det pelo sistema, pol na base canonica
for i= 1:N+1
	A(i,1) = 1;
	for j=2:N+1
		A(i,j) = A(i,j-1)*x(i);
	end #for
	A(i, N+2) = y(i);
end #for
neq = N + 1; # numero de equacoes
c = fgauss(neq, A)
#plot func original
np = N*10;
hp = (b - a)/np;
xp = a:hp:b;
yp = log(xp);
#plot pol da base canonica
xi = xp;
yi = resto(N, c, xi);
#met2 pol escrito na base dos polinomios de lagrange
xi = xp;
yiL = flgrang(N, x, y, xi);

%metodo3 pol escrito na forma de diferencas divididas de gregory newton
#[x; y;]'
difdiv = fdifdiv(x,y,N);
xi = xp;
yiGN = fGnewton(N, x, y, difdiv, xi);


#erro = abs(yp .- yi);
#plot(xp, erro);
plot(x, y, '*',xp, yp, "b;f(x)=ln(x);", xi, yi, 
"k;Pn(x)basecanonica;", xi, yiL, 
"y;Pn(x)base Lagrange;" , xi, yiGN, "g;Pn(x)gregory newton;");

