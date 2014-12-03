clc
clear all
close all
N = 2;
a = -1;
b = 1;

h = (b - a)/N; # passo
x = a:h:b;
y = exp(x);

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
np = N*20;
hp = (b - a)/np;
xp = a:hp:b;
yp = exp(xp);
#plot pol da base canonica
xi = xp;
yi = resto(N, c, xi);

erroInter = abs(yp .- yi);

#Serie de Maclaurin
xM = xp;
for j=1:np+1
	yM(j) = 0;
	for i=N:-1:1
		yM(j) += (xM(j)^i)/factorial(i);
	end #for
	yM(j)+=1;
end #for

erroMac = abs(yp .- yM);

#plot(xp, erroInter, "b;erro(x)=|f(x) - Pn(x)|;", xM, erroMac, "g;erro(x)=|f(x) - Mac(x)|;");
plot(x, y, '*',xp, yp, "b;f(x)=exp(x);", xi, yi,"k;Pn(x)basecanonica;",
 xM, yM, "m;exp(x) Maclaurin;");
grid();
