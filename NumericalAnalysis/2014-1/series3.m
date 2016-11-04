clc
clear
%f(x)=sqrt(x);                        com x entre [a=1;b=2]

%%%%%%%%%%TRANSFORMAÇÃO 'MANUAL' PARA INTERVALO [-1;+1]
%x(t)=0.5*(b-a)*t+0.5*(b+a);
%f(x(t))=sqrt(0.5*(b-a)*t+0.5*(b+a)); com t entre [a=-1;b=+1]
%f(x(t))=sqrt(0.5*t+1.5);             com t entre [a=-1;b=+1]

N = 3;
a = -1;
b = 1;

h = (b - a)/N; % passo
t = a:h:b;
y = sqrt(0.5*t+1.5);

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
yp = sqrt(0.5*tp+1.5);

%plot pol da base canonica
ti = tp;
yi = resto(N, c, ti);

erroInter = abs(yp .- yi);
erroInterMax = max(erroInter)

%Serie de Maclaurin
for i=N:-1:1
    produtorio =1;
	for k=1:i
		produtorio = produtorio*(3-2*k);
	end %for
	temp(i)= (produtorio/(2^i))*(1.5)^((1-2*i)/2)*0.5^i/factorial(i);
end %for
o_temp=(1.5)^(0.5);

'coeficientes da serie de MacLaurim:'
[o_temp temp]   

%%%%%%%PODEMOS VOLTAR E TRANSFORMAR PARA x entre [1;2] FAZENDO:
%  t=(x-0.5*(b+a))/(0.5*(b-a));

tM = tp;
for j=1:np+1
	yM(j) = 0;
	for i=N:-1:1
	   yM(j) += temp(i)*(tM(j))^i;
	end %for
	yM(j)+=o_temp;
end %for

erroMac = abs(yp .- yM);
erroMacMax = max(erroMac)

#series de Chebyshev
tC = tM;
yC = -0.0170103 .*tC.^2+0.20625 .*tC + 1.22475;

erroCheb = abs(yp .- yC);
erroChebMax = max(erroCheb)

plot(tp, erroInter, "k;erro(x)=|f(x) - Pn(x)|;", tM, erroMac, "m;erro(x)=|f(x) - Mac(x)|;", tC, erroCheb, "g;erro(x)=|f(x) - Cheb(x)|;" );
%plot(t, y, '*',tp, yp, "b;f(x)=sqrt(x);", ti, yi,"k;Pn(x)basecanonica;",
% tM, yM, "m;sqrt(x) Maclaurin;", tC, yC, "g;sqrt(x)Cheb;");
grid();
