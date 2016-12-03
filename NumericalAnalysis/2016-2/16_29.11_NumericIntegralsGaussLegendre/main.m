clear
clc
format long

%Método dos trapezios

a = 1
b = 6
I_ = log(b+1) - log(a+1);

erroTnMax = 1e-6

%n = 1614; calculado pelo erro de truncamento da série de taylor
n = 500
Tn = fTn(n, a, b)


erroTnExato___ = abs(Tn - I_)
erroTnEstimado = abs(Tn - fTn(n*2,a ,b))

%metodo simpson
n = 30

Sn = fSn(n, a, b)
erroSnExato___ = abs(Sn - I_)
erroSnEstimado = abs(Sn - fSn(n*2,a ,b))

%metod de gauss legendre
m = 6 % numero de pontos

Gm = fGm(m, a, b)
erroGmExato___ = abs(Gm - I_)
erroGmEstimado = abs(Gm - fGm(m+1,a ,b))



I_
Tn
Sn
Gm