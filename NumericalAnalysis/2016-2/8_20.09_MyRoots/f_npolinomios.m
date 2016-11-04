function x = f_npolinomios(n, a , xi)

    criterio=1; k=0;
    x=0;
    
    while criterio > 1e-15 && k < 1
        k++
        R = f_restos(n, a , xi)
        P3 =  (xi^3 - 3*xi^2 + 3*xi -1) 
        dP3= (3*xi^2 - 6*xi + 3)
      % x = xi - (xi^3 - 3*xi^2 + 3*xi -1) / (3*xi^2 - 6*xi + 3)
        x = xi - R(1)/ R(2) 
        criterio = abs(x-xi)
        xi=x;
    end
end