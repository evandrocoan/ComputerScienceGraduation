clear
clc
format long
a=0;
b=1;

I=-cos(b)-(-cos(a))
%Método dos Trapézios
n=128
Tn = fTn(a,b,n)
Tn2 = fTn(a,b,2*n)
erroEstimadoTn = abs(Tn2-Tn)
erroExatoTn = abs(I-Tn)
%Método de Simpson
n=6
Sn = fSn(a,b,n)
Sn2 = fSn(a,b,2*n)
erroEstimadoSn = abs(Sn2-Sn)
erroExatoSn = abs(I-Sn)
%Método de Gauss-Legendre
m=3
Gm = fGm(a,b,m)
Gm2 = fGm(a,b,m+1)
erroEstimadoGm = abs(Gm2-Gm)
erroExatoGm = abs(I-Gm)
%ErroGm=((b-a)^(2*m+1)*(factorial(m))^4*sin(1))/((2*m+1)*factorial(2*m)^3)