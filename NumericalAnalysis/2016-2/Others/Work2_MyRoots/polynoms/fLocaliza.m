% Encontrar apenas um valor inicial
function xi = fLocaliza(n, a)
    % Cota máxima
    r_max = 1 + max(abs(a(2 : n+1))) / abs(a(1));
    % Cota Mínima
    r_min = 1 / (1 + max(abs(a(1 : n))) / abs(a(n+1)));
    r_medio = (r_max + r_min)*0.5;
    xi=complex(r_medio*cos(pi/4),r_medio*sin(pi/4));
end