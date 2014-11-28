clear
clc

%ajuste polinomial de grau n. n =1.

m=5; %numero de pontos experimentais
X = [1 3 4 6 8]
Y = [0 1 2 4 5]
n=1;%grau do polinomio ajustador
n_eq=n+1;

for i=1:n_eq
	for j=1:n_eq
		A(i,j)=0;
		for k=1:m
			A(i,j)=A(i,j)+X(k)^(i+j-2);
		endfor
	endfor
	A(i,n_eq+1)=0;
	for k=1:m
		A(i,n_eq+1)= A(i,n_eq+1)+X(k)^(i-1)*Y(k);
	endfor
endfor
A
coef=fgauss(n_eq,A)
plot(X,Y,'*') 




