function [ x, M, iteracoes ] = fNewtonPolinomios(n, a, xi, toleranciaMinima)
    x = xi;
    iteracoes = 0;
    criterio = 1;
    while(criterio > toleranciaMinima && iteracoes < 120)
        iteracoes = iteracoes + 1;
        R = calcularRestoDaDivisao(n, a, xi);
        M = calcularNumeroDeMultiplicides(R);
        x = xi + (-R(M) / (M*R(M+1)));
        
        # Soma-se o Resto 1, por que o primeiro resto é o valor do polinômio no ponto xi.
        # Assim, como estamos buscando as raízes, esse valor será 0 quando a raiz convergir.
        # O Resto 2 é o valor da primeira derivada no ponto xi. E o último resto é sempre 1. 
        # No caso de x^4, o valor da 4ª derivada, que é o Resto 5, será 1, o coeficiente a(1).
        criterio = abs(x-xi) + abs(R(1));
        xi = x;
    end
end
        %P3 = (xi^3 - 3*xi^2 + 3*xi - 1)
        %dP3 = (3*xi^2 - 6*xi + 3)
        % x = xi - (xi^3 - 3*xi^2 + 3*xi - 1) / (3*xi^2 - 6*xi + 3)
