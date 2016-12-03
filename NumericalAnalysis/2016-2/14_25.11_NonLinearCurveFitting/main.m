m = 7;
x = [0:6];                        %nem sempre estão ordenados
y = [0.1 0.3 0.9 1.2 1.3 1.2 1];  % medidos

%Gráfico de teste
%{
    a = [1 1];    %valores chutados
    xp = min(x) : 0.05 : max(x);
    yp = a(1) .* sin( a(1) .* xp );
    %grafico
%}
ai = [1.32 0.38];  %chute inicial influencia muito na obtenção do resultado final
a = fNewton(ai)
f1(a)
f2(a)
xp = min(x) : 0.05 : max(x);
yp = a(1) .* sin( a(2) .* xp );
plot(x, y, '*', xp, yp, 'm', 7, 0)
