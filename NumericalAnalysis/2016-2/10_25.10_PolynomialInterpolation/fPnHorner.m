function yp =fPnHorner(n, a, xp)
    for k = 1 : length(xp)
        aux = a(n+1);
        for i = n : -1 : 1
            aux = a(i) + xp(k)*aux;
        end
        yp(k) = aux;
    end
end