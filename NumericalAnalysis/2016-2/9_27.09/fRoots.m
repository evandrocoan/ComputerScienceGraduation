function x = fRoots(a)
    n = length(a) - 1
    for i = 2 : n+1
        a(i) = a(i)/a(1);
    end
    a(1) = 1;
    
    k = 1  % Para a 1ª raiz
    % 1º Passo: localizar a raiz
    xi(k) = fLocaliza(n, a);
    xi(k) = 2 %eliminar
    % 2º Passo: aproximação
    [x(k), M(k)] = fNPolinomios(n, a, xi(k))
    % Redução de grau pela raiz x, M vezes
    [n, a] = fReducaoGrau(n, a, x(k), M(k))
    
end