clear
clc
format long
a=1
b=6
I=log(1+b)-log(1+a) % f(x)=(1+x)^(-1);

% Método de Integração por Trapézios (Usando polinômios de 1o. grau)
n=1024   %numero de sub-divisoes para calcular um valor de Tn
Tn=fTn(n,a,b)
n2=2*n   %Dobro de sub-divisoes para calacular um valor de Tn mais proximo do exato
Tn2=fTn(n2,a,b)  % Valor exato estimado 
ErroexatoTn  = abs(Tn-I  )
ErroestimadoTn = abs(Tn-Tn2)

% Método de Integração por Simpson (Usando polinômios de 2o. grau)
n=32 %numero de sub-divisoes para calcular um valor de Sn
Sn=fSn(n,a,b)
n2=2*n   %Dobro de sub-divisoes para calacular um valor de Sn mais proximo do exato
Sn2=fSn(n2,a,b)  % Valor exato estimado 
ErroexatoSn    = abs(Sn-I  )
ErroestimadoSn = abs(Sn-Sn2)

% Método de Gauss-Legendre (Usando m pontos integrando polinomios de grau (m-1)
m = 6
Gm = fGm(m,a,b)
m2=(m+1)   %Dobro de sub-divisoes para calacular um valor de Gm mais proximo do exato
Gm2=fGm(m2,a,b)  % Valor exato estimado 
ErroexatoGm   = abs(Gm-I  )
ErroestimadoGm = abs(Gm-Gm2)
