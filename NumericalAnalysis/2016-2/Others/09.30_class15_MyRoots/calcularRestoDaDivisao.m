function R = calcularRestoDaDivisao(n, a, xi)
    grau = n;
    for ndiv = 1 : grau
        b(1) = a(1);
        for i = 2 : n+1
            b(i) = a(i) + xi*b(i-1);
        end
        R(ndiv) = b(n+1);
        a = b(1:n);
        n = n-1;
    end
    R(grau+1) = a(1);
end
