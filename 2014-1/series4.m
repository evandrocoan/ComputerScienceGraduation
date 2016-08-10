clc
clear
%f(x)=sen(x);                        com x entre [a=-1;b=+1]

N = 7
a = -1
b = +1

h = (b - a)/N; % passo
t = a:h:b;
y = sin(t);

%met1 det pelo sistema, pol na base canonica
for i= 1:N+1
	A(i,1) = 1;
	for j=2:N+1
		A(i,j) = A(i,j-1)*t(i);
	end %for
	A(i, N+2) = y(i);
end %for
neq = N + 1; % numero de equacoes

c = fgauss(neq, A);
'coeficientes de Pn(x):'
[c]

%plot func original
np = N*20;
hp = (b - a)/np;
tp = a:hp:b;
yp = sin(tp);

%plot pol da base canonica
ti = tp;
yi = resto(N, c, ti);

erroInter = abs(yp .- yi);
erroInterMax = max(erroInter)

%Serie de Maclaurin
'coeficientes da serie de MacLaurim:'
temp=[1 0 -1/factorial(3) 0 1/factorial(5) 0 -1/factorial(7)]   

tM = tp;
for j=1:np+1
	yM(j) = 0;
	for i=N:-2:1
	   yM(j) += temp(i)*(tM(j))^i;
      	end %for
end %for

erroMac = abs(yp .- yM);
erroMacMax = max(erroMac)

#series de Chebyschev
'coeficientes da serie de Chebyschev:'
temp=[46079./46080  0 -959./5760. 0 +23./2880.]   

tC = tM;
yC=(23.*tC.^5)./2880.-(959.*tC.^3)./5760.+(46079.*tC)./46080;

erroCheb = abs(yp .- yC);
erroChebMax = max(erroCheb)

plot(tp, erroInter, "k;erro(x)=|f(x) - Pn(x)|;", tM, erroMac, "m;erro(x)=|f(x) - Mac(x)|;", tC, erroCheb, "g;erro(x)=|f(x) - Cheb(x)|;" );
%plot(t, y, '*',tp, yp, "b;f(x)=sen(x);", ti, yi,"k;Pn(x)basecanonica;",
% tM, yM, "m;sen(x) Maclaurin;", tC, yC, "g;sen(x)Cheb;");
grid();
