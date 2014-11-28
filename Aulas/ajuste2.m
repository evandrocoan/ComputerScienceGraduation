clear
clc

m=5; %numero de pontos experimentais
T = [1 1.25 1.50 1.75 2.00]
V = [0 0.24 0.48 0.68 0.85]

%ajuste não polinomial transformado para polinomial de grau 1.
% V(T) = sen(a + b*T)
% 	arcsen(Vk) 	= a  + b*Tk
% 			yk	= a0 + a1*xk
#------------------------#
n=1;%grau do polinomio ajustador
X = T;
Y = asin(V);
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
coef=fgauss(n_eq,A);
#------------------------#
a = coef(1)
b = coef(2)
a = -1, b = 1
np = 10*m;
Tp = T(1):(T(m)-T(1))/np:T(m);
Vp = sin(a .+ b.*Tp);
D = 0;
for k = 1:m
	D = D + (sin(a + b*T(k)) - V(k))^2;
end
D = D/m
plot(T,V,'*', Tp, Vp, "b;Vp = sin(a + b*Tp);") 




