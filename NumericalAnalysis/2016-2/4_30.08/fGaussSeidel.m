function x = fGaussSeidel(n, A)
    lambda = 0.1; lamb_aux = 1-lambda;
    for i=1 : n xi(i) = 0; end
    x = xi;
    k = 0;
    dif = 1
    while (dif > 1e-6 & k < 10000)
        k = k + 1
        i = 1;
        x(i) = (lamb_aux)*xi(i) + lambda*( A(i, n+1) - A(i, i+1)*x(i+1) ) / A(i, i);
        
        for i=2 : n-1
            x(i) = (lamb_aux)*xi(i) + lambda*( A(i, n+1) - A(i, i-1)*x(i-1) - A(i, i+1)*x(i+1) ) / A(i, i);
        end
        
        i = n;
        x(i) = (lamb_aux)*xi(i) + lambda*( A(i, n+1) - A(i, i-1)*x(i-1) ) / A(i, i);
        
        dif = max(abs(x-xi))
        xi = x;
    
    end
    
end