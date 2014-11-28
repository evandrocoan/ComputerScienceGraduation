clear all
close all
N = 8;
a = 1;
b = 2;

h = (b - a)/N; # passo
x = a:h:b;
y = log(x);

for i= 1:N+1
	A(i,1) = 1;
	for j=2:N+1
		A(i,j) = A(i,j-1)*x(i);
	end #for
	A(i, N+2) = y(i);
end #for

neq = N + 1; # numero de equacoes
c = fgauss(neq, A)

np = N*10;
hp = (b - a)/np;
xp = a:hp:b;
yp = log(xp);

xi = xp;
yi = resto(N, c, xi);

erro = abs(yp .- yi);
plot(xp, erro);
#plot(x, y, '*',xp, yp, "b;ln(x);", xi, yi, "k;Pn(x);");
