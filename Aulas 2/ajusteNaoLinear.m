clc
clear
format long
m = 16;
N = [ 30,    30,    30,    30,    35,    35,    35,    35,    45,    45,    45,    45,    50,   50,   50,  50 ];
C = [ 3,     3.5,   4.5,   5,     3,     3.5,   4.5,   5,     3,     3.5,   4.5,   5,     3,    3.5,  4.5, 5  ];
T = [ 225.3, 200.2, 163.8, 150.2, 306.5, 272.4, 222.9, 204.3, 506.5, 450.2, 368.4, 337.7, 625.3 555.8 454.7 416.8];

n = 3;
xi = [ 1.1 1.1 1.1];
erromax = 1.e-6; lim = 100;
erro = 1; passo = 0;
while (erro > erromax && passo < lim)
	passo = passo+1
	dx = [ 1.e-6, 1.e-6, 1.e-6 ];
	%inicio do núcleo do método de newton
A=[(h1([xi(1)+dx(1),xi(2),xi(3)],m,N,C,T)-h1(xi,m,N,C,T))/dx(1), (h1([xi(1),xi(2)+dx(2),xi(3)],m,N,C,T)-h1(xi,m,N,C,T))/dx(2), (h1([xi(1),xi(2),xi(3)+dx(3)],m,N,C,T)-h1(xi,m,N,C,T))/dx(3), -h1(xi,m,N,C,T);
(h2([xi(1)+dx(1),xi(2),xi(3)],m,N,C,T)-h2(xi,m,N,C,T))/dx(1), (h2([xi(1),xi(2)+dx(2),xi(3)],m,N,C,T)-h2(xi,m,N,C,T))/dx(2), (h2([xi(1),xi(2),xi(3)+dx(3)],m,N,C,T)-h2(xi,m,N,C,T))/dx(3), -h2(xi,m,N,C,T);
(h3([xi(1)+dx(1),xi(2),xi(3)],m,N,C,T)-h3(xi,m,N,C,T))/dx(1), (h3([xi(1),xi(2)+dx(2),xi(3)],m,N,C,T)-h3(xi,m,N,C,T))/dx(2), (h3([xi(1),xi(2),xi(3)+dx(3)],m,N,C,T)-h3(xi,m,N,C,T))/dx(3), -h3(xi,m,N,C,T);
  ];

	dx = fGauss (n, A);

	x = xi.+dx;
	xi = x;
	%fim do núcleo do método de newton
	erro = max(abs(dx))
end

x

% Construindo grafico da superficie T(N,C):
% Calculando passos:
hN=(N(m)-N(1))/100;
hC=(C(m)-C(1))/100;
% Definindo pontos:
Np=N(1):hN:N(m);
Cp=C(1):hC:C(m);
for i=1:101
for j=1:101
Tp(i,j)=funcaoTempo(x, Np(i),Cp(j) );
end
end
% Plotando:
surf(Np,Cp,Tp)
xlabel('N')
ylabel('C')
zlabel('T')
grid on
axis square
hold on
plot3(N,C,T,'*')
hold off

