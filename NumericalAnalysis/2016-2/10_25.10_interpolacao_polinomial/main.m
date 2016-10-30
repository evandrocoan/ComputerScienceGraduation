format long
a = 1
b = 2

% Método da Interpolação Geral

n = 1
tolerancia = sqrt(10)*1e-6;
ErroPnMax = 1;
while ErroPnMax > tolerancia
    n = n + 1
    h = (b-a)/n;
    x = a : h : b;
    y = log(x);
    % coef = fliplr(polyfit(x, y, n));

    coeficientes = fInterpolacaoPolinomial(x, y, n);
    xp = a : h/20 : b;

    % Yp estimado
    % yp = fPnHorner(n, coeficientes, xp);
    yp = fPnBriot(n, coeficientes, xp);

    % Y exato
    ye = log(xp);

    % Calculo de erros
    ErroPn = abs(yp - ye);
    ErroPnMax = max(ErroPn)
end

clear

% Método de Interpolação de Lagrange
a = 1
b = 2
n = 2
h = (b-a)/n;
x = a : h : b;
y = log(x);
xp = a : h/20 : b;    % Pontos para o gráfico e para o erro
yp = fPnLagrange(n, x, y, xp);
% Y exato
ye = log(xp);

plot(x, y, '*', 2.2, 0, xp ,yp, 'r', xp, ye, 'b')
% Plot do erro
%plot(xp, ErroPn, 'm', 'linewidth', 2, 2.2, 0) 
% legend('Erro de Pn(x)', "location", 'north')