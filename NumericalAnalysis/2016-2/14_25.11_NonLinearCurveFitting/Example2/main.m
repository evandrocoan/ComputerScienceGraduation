m = 10;
x = [1 2.6 3.5 4.1 5.5 6.5 7.1 8.0 3.4 1.2]
y = [0.1 0.2 0.25 0.3 0.4 0.5 0.41 0.35 0.24 0.15]

ai = [0 1 0 0.5]
a = fNewtonNumerico(4, ai, @F)
a = a.*0.9  % Teste de minimo com 1.1 o desvio quadratico aumenta e maximo com 0.9 desvio quadratico aumenta
xp = min(x) : 0.5 : max(x);
yp = a(1) + a(2)*sin(a(3) + a(4)*xp);

% Desvios

DesvioLocal = a(1) .+ a(2).*sin(a(3) .+ a(4).*x) .- y
MediaDesvios = sum(DesvioLocal) / m
DesvioQuadraticoTotal = sum(DesvioLocal.^2)

% Gráfico
plot(x, y,'*', xp, yp, '-r', 9.0, 0)
