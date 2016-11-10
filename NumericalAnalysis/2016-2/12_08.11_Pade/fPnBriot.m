function yp =fPnBriot(n, coeficientes, xp)
    a = fliplr(coeficientes);
    b(1) = a(1);
    for k = 1 : length(xp)
        for i = 2 : n+1
            b(i) = a(i) + xp(k)*b(i-1);
        end
        yp(k) = b(n+1);
    end
end