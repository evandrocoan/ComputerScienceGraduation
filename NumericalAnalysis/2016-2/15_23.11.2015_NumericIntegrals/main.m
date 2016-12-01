clear
clc
format long

%limites da integral
a=1
b=6
I=log(1+b)-log(1+a) %Integral exata, p validação do algoritmo


%metodo dos trapezios
n=390
Tn=fTn(n,a,b)
T2n=fTn(2*n,a,b)    %Valor exato estimado c 2*n+1 pontos é suficiente
Erroexato=abs(Tn-I) %VA-VE
ErroexatoestimadoTn=abs(Tn-T2n)

%metodo de simpson
n=26
Sn=fSn(n,a,b)
S2n=fSn(2*n,a,b)      %Valor exato estimado c 2*n+1 pontos é suficiente
ErroexatoSn=abs(Sn-I) %VA-VE
ErroexatoestimadoSn=abs(Sn-S2n)

%integral de gauss legendre
m=6
Gm=fGm(a,b,m)
ErroexatoGm=abs(Gm-I) %VA-VE
G2m=fGm(a,b,m+1)      %Valor exato estimado c m+1 pontos é suficiente
ErroexatoGmEstimado=abs(Gm-G2m)

' Conclusão: O método e Gauss-Legendre é o mais eficiente, pois consegue calcular a integral com m=6 pontos apenas, enquanto Simpson precisa de 26+1 e Trapezios 390+1 pontos.  Todos com erro exato estimado na ordem de 1e-6. '