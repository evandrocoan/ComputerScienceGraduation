function [n, a] = reduzirGrauDoPolinomio(n, a, x, M)
    % Usar fRestos controladamente
    grau = n;
    for ndiv = 1 : M %divide M vezes
        b(1) = a(1);
        for i = 2 : n+1
            b(i) = a(i) + x*b(i-1);
        end
        R(ndiv) = b(n+1);
        a = b(1:n);
        n = n-1;
    end
end
