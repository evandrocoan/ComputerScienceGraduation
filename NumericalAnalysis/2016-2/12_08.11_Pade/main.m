% Aproximação ln(x) [1,2] com erro 1e-6

% 1- Interpolação polinomial - Método de Interpolação de Lagrange
xa = 1;
xb = 2;
n=7
h= (xb-xa)/n;
x= xa : h : xb; %pontos  de anconragem do interpolador
y = log(x);

xp = xa : h/20 : xb; %Pontos para o gráfico e para o erro
yp = fPnLagrange(n, x , y , xp);
ye = log(xp); % y exato para comparacao
%Graficos e erros
tp = (2.*xp .- (xb.+xa)) ./ (xb.-xa);

erroPn=abs(yp.-ye);
erroPnMax=max(erroPn)

% 2- Serie de Maclaurim
% Transformação de variável
% x(t) = 0,5*(b-a)*t + 0,5*(b+a)
% f(x(t))= ln(0,5*t + 1,5)
n = 9
coefMac = fmac1(n);

% Yp estimado
yMac = fPnBriot(n, coefMac, tp);
% Calculo de erros
erroMac = abs(yMac - ye);
erroMacMax = max(erroMac)


%3 Serie de Tchebychev
%Testar k para que Erro máximo O(1e-6)
k=6 %Grau da serie de Tchebychev

b = fTchebychev(k); % Devolve 11 b's coeficientes de Tchebychev
t = (2.*xp .- (xb .+ xa) ./ (xb .- xa));%x de plotagem
%ye=log(0.5.*t.+1.5);
%Serie de Tchebychev em função dos polinomios de Tchebychev Ti(x) p/ grau k=6:
YTC=b(1).*1+b(2).*t.+b(3).*(2.*t.^2.-1).+b(4).*(4.*t.^3.-3.*t).+b(5).*(8.*t.^4.-8.*t.^2.+1).+b(6).*(16.*t.^5-20.*t.^3.+5.*t).+b(7).*(32.*t.^6.-48.*t.^4.+18.*t.^2.-1);
%termos adicionais de grau 7 a 11
%.+b(8).*(64.*t.^7.-112.*t.^5.+56.*t.^3.-7.*t).+b(9).*(128.*t.^8.-256.*t.^6.+160.*t.^4.-32.*t.^2.+1)+b(10).*(512.*t.^10.-1280.*t.^8.+1120.*t.^6.-400.*t.^4.+50.*t.^2.-1)+b(11).*(512.*t.^10.-1280.*t.^8.+1120.*t.^6.-400.*t.^4.+50.*t.^2.-1)+b(12).*(1024.*t.^11.-2816.*t.^9.+2816.*t.^7.-1232.*t.^5.+220.*t.^3.-11.*t);
erroTC=abs(YTC.-ye);
erroTCMax=max(abs(erroTC))

%Pade
n= 3
m= 2

coefMac = fmac1(n+m);
[ap bp]=fPade(n, m, coefMac);
yPade= fPnBriot(n, ap, tp) ./ fPnBriot(n, bp, tp)

ErroPade= abs(yPade .- ye);
ErroPadeMAX= max(ErroPade)




%plot(xp,ye,'.j',xp,YTC,'-b')

% Plot do erro
%plot(xp, erroPn, "--b;Erro de Pn(x);",'linewidth', 2,xp, erroMac, "--k;Erro de Maclaurin(x);",'linewidth', 2,xp, erroTC,"-r;Erro de Tchebychev(x);",'linewidth', 2, 2.2, 0)
% legend('location', 'north')
