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

#series de Chebyshev
'coeficientes da serie de Chebyshev:'
temp=[46079./46080  0 -959./5760. 0 +23./2880.]   

tC = tM;
yC=(23.*tC.^5)./2880.-(959.*tC.^3)./5760.+(46079.*tC)./46080;

erroCheb = abs(yp .- yC);
erroChebMax = max(erroCheb)

# aproximação racional de Padé
% f(x) = sen(x) aproximado com série de MacLaurin de grau M = 7
C = [0 1 0 -1/factorial(3) 0 1/factorial(5) 0 -1/factorial(7)];
M = 7;
npade = 5;% a(0+1) até a(5+1)
mpade = 2;% b(1) até b(2)
%M = npade + mpade;
%calcular os Bs
A = [
[c(5) c(6) -c(7)];%o c(0) foi trocado por c(1) e etc pq o octave começa do 1
[c(6) c(7) -c(8)];
];
fgauss(mpade, A)
b = fliplr(fgauss(mpade, A))
 %B começa de 1, igual a formula do pade
% fliplr serve para reordenar a solução de acordo com o formulário [b3 b2 b1]
%calcular os As
a(1) = c(1); %o a(0) foi trocado por a(1) e etc =D
a(2) = b(1)*c(1) + c(2);
a(3) = b(2)*c(1) + b(1)*c(2) + c(3);

b(3)=0;b(4)=0;b(5)=0;
a(4) = b(3)*c(1) + b(2)*c(2) + b(1)*c(3) + c(4);
a(5) = b(4)*c(0+1) + b(3)*c(1+1) + b(2)*c(2+1)+ b(1)*c(3+1) + c(4+1);
a(6) = b(5)*c(0+1) + b(4)*c(1+1)+ b(3)*c(2+1) + b(2)*c(3+1)+ b(1)*c(4+1) + c(5+1);

a
%R32 = (a0 + a1x + a2x^2 + a3x^3 + a4x^4+ a5x^5)/(1 + b1x + b2*x^2)
tP = tp;
ypade=(a(0+1) + a(1+1) .*tP + a(2+1) .*tP.^2 + a(3+1) .*tP.^3 + a(4+1) .*tP.^4+ a(5+1) .*tP.^5) ./(1 + b(1) .*tP + b(2) .*tP.^2 );
erroPade=abs(yp-ypade);
erroPadeMax=max(erroPade)

%%%%%%%%%%%%%%
%Como a serie MacLaurin de sen(x) tem termos nulos para graus pares, fazendo n=5 IMPAR no numerador, temos erros melhores, mas analogos a Chebyshev.  
%%%%%%%%%%%%%%



plot(tp, erroInter, "k;erro(x)=|f(x) - Pn(x)|;", tM, erroMac, "m;erro(x)=|f(x) - Mac(x)|;", tC, erroCheb, "g;erro(x)=|f(x) - Cheb(x)|;", tP, erroPade, "r;erro(x)=|f(x) - R52(x)|;");
%plot(t, y, '*',tp, yp, "b;f(x)=sen(x);", ti, yi,"k;Pn(x)basecanonica;",
% tM, yM, "m;sen(x) Maclaurin;", tC, yC, "g;sen(x)Cheb;");
grid();
