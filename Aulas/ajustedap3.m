clear
clc

m=5; %numero de pontos experimentais
T=[ 0.20 0.40 0.60 0.80 1.00]; 
P=[ 5.21 2.89 2.26 2.04 2.00];

% ajuste não polinomial.
% P(T) = a*T + b/T;
#------------------------#
n_eq=2;

A(1,1) = 0;
for k = 1:m
	A(1,1) = A(1,1) + T(k)^2;
end
A(1,2) = m;
A(1,3) = 0;
for k = 1:m
	A(1,3) = A(1,3) + T(k)*P(k);
end
A(2,1) = m;
A(2,2) = 0;
for k = 1:m
	A(2,2) = A(2,2) + 1/T(k)^2;
end
A(2,3) = 0;
for k = 1:m
	A(2,3) = A(2,3) + P(k)/T(k);
end
coef=fgauss(n_eq,A);
a = coef(1)
b = coef(2)
#------------------------#
np = 10*m;
Tp = T(1):(T(m)-T(1))/np:T(m);
Pp = a.*Tp .+ b./Tp;
D = 0;
for k = 1:m
	D = D + (a*T(k) + b/T(k) - P(k))^2;
end
D = D/m
plot(T,P,'*', Tp, Pp, "b;Pp = a*Tp + b/Tp;") 
