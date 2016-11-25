m = 4;
x = [0 0.39 0.78 1.18];
y = [0.99 0.92 0.71 0.38];

a = fAjusteCos(m, x, y)

xp = min(x) : 0.01 : max(x);
yp = a(1).*xp .+ a(2).*cos(xp);

d=a(1).*x .+ a(2).*cos(x)-y % desvios locais
D=sum(abs(d))/m             % desvio total médio em módulo
plot(x, y, '*', xp, yp, '-r')