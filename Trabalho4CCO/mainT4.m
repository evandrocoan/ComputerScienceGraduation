clc
clear
format long

%Definições
m = 3
n = 2*m
C = [ 0.5 0.5 0.5];	%Correto  [5/9	8/9	5/9]
X = [-0.5 0   0.5 ];	%Correto: [-sqrt(3/5)	0	sqrt(3/5)]
xInicial = X
cInicial = C

%f1 → C(1)		C(2)		C(3)			=	2
%f2 → C(1)*X(1)	C(2)*X(2)	C(3)*X(3)		=	0
%f3 → C(1)*X(1)^2	C(2)*X(2)^2	C(3)*X(3)^2	=	2/3
%f4 → C(1)*X(1)^3	C(2)*X(2)^3	C(3)*X(3)^3	=	0
%f5 → C(1)*X(1)^4	C(2)*X(2)^4	C(3)*X(3)^4	=	2/5
%f6 → C(1)*X(1)^5	C(2)*X(2)^5	C(3)*X(3)^5	=	0


%df1(x)/dX1		df1(x)/dX2		...		df1(x)/dx6
%df2(x)/dX1		df2(x)/dX2		...
%...			...			...
%df6(x)/dX1		...			...		df6/dx6

erro = 1;
cont = 0;
while (erro>1e-16 && cont<20)
	cont+=1;
	
	Df1_dX1 = 0;
	Df2_dX1 = C(1);
	Df3_dX1 = 2*C(1)*X(1);
	Df4_dX1 = 3*C(1)*X(1)^2;
	Df5_dX1 = 4*C(1)*X(1)^3;
	Df6_dX1 = 5*C(1)*X(1)^4;

	Df1_dX2 = 0;
	Df2_dX2 = C(2);
	Df3_dX2 = 2*C(2)*X(2);
	Df4_dX2 = 3*C(2)*X(2)^2;
	Df5_dX2 = 4*C(2)*X(2)^3;
	Df6_dX2 = 5*C(2)*X(2)^4;

	Df1_dX3 = 0;
	Df2_dX3 = C(3);
	Df3_dX3 = 2*C(3)*X(3);
	Df4_dX3 = 3*C(3)*X(3)^2;
	Df5_dX3 = 4*C(3)*X(3)^3;
	Df6_dX3 = 5*C(3)*X(3)^4;

	Df1_dC1 = 1;
	Df2_dC1 = X(1);
	Df3_dC1 = X(1)^2;
	Df4_dC1 = X(1)^3;
	Df5_dC1 = X(1)^4;
	Df6_dC1 = X(1)^5;

	Df1_dC2 = 1;
	Df2_dC2 = X(2);
	Df3_dC2 = X(2)^2;
	Df4_dC2 = X(2)^3;
	Df5_dC2 = X(2)^4;
	Df6_dC2 = X(2)^5;

	Df1_dC3 = 1;
	Df2_dC3 = X(3);
	Df3_dC3 = X(3)^2;
	Df4_dC3 = X(3)^3;
	Df5_dC3 = X(3)^4;
	Df6_dC3 = X(3)^5;
	
	A(1,1) = Df1_dX1;
	A(1,2) = Df1_dX2;
	A(1,3) = Df1_dX3;
	A(1,4) = Df1_dC1;
	A(1,5) = Df1_dC2;
	A(1,6) = Df1_dC3;

	A(2,1) = Df2_dX1;
	A(2,2) = Df2_dX2;
	A(2,3) = Df2_dX3;
	A(2,4) = Df2_dC1;
	A(2,5) = Df2_dC2;
	A(2,6) = Df2_dC3;

	A(3,1) = Df3_dX1;
	A(3,2) = Df3_dX2;
	A(3,3) = Df3_dX3;
	A(3,4) = Df3_dC1;
	A(3,5) = Df3_dC2;
	A(3,6) = Df3_dC3;

	A(4,1) = Df4_dX1;
	A(4,2) = Df4_dX2;
	A(4,3) = Df4_dX3;
	A(4,4) = Df4_dC1;
	A(4,5) = Df4_dC2;
	A(4,6) = Df4_dC3;

	A(5,1) = Df5_dX1;
	A(5,2) = Df5_dX2;
	A(5,3) = Df5_dX3;
	A(5,4) = Df5_dC1;
	A(5,5) = Df5_dC2;
	A(5,6) = Df5_dC3;

	A(6,1) = Df6_dX1;
	A(6,2) = Df6_dX2;
	A(6,3) = Df6_dX3;
	A(6,4) = Df6_dC1;
	A(6,5) = Df6_dC2;
	A(6,6) = Df6_dC3;

	A(1,7) = -(f1(C, X));
	A(2,7) = -(f2(C, X));
	A(3,7) = -(f3(C, X));
	A(4,7) = -(f4(C, X));
	A(5,7) = -(f5(C, X));
	A(6,7) = -(f6(C, X));
	
	dx = fgauss(n,A);
	
	X(1) = xInicial(1) + dx(1);
	X(2) = xInicial(2) + dx(2);
	X(3) = xInicial(3) + dx(3);
	xInicial = X;

	C(1) = cInicial(1) + dx(4);
	C(2) = cInicial(2) + dx(5);
	C(3) = cInicial(3) + dx(6);	
	cInicial=C;
	
	erro=max(abs(dx));
end

X

C

a = 0
b = 1

soma1 = 0;
soma2 = 0;
for k=1:m
	%Converte o x para -1 a 1
	x(k) = 0.5*(b-a)*X(k)+0.5*(b+a);
	
	%Calcula a aproximação da integral por Gauß-Legendre do Polinomio de Grau 5
	y1(k) = fT(x(k));
	soma1 = soma1+C(k)*y1(k);

	%Calcula a aproximação da integral por Gauß-Legendre do Polinomio de Grau 6
	y2(k) = fT2(x(k));
	soma2 = soma2+C(k)*y2(k);
end
Gm1 = 0.5*(b-a)*(soma1)
Exato1 = integralT(a, b)
erro1 = abs(Gm1 - Exato1)

Gm2 = 0.5*(b-a)*(soma2)
Exato2 = integralT2(a, b)
erro2 = abs(Gm2 - Exato2)

