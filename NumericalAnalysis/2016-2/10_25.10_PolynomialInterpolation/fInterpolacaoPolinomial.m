function coeficientes = fInterpolacaoPolinomial(x, y, n)
    for i = 1 : n+1
        A(i,1) = 1;
        A(i,2) = x(i);
        for j = 3 : n+1
            A(i,j) = A(i,j-1)*x(i);
        end
        A(i,n+2) = y(i);
    end
    A;
    coeficientes = fGaussPivot(n+1, A);
end