function x =f_roots(a)

    n = length(a) -1;
    
    k=1   %primeira raiz
    
    %passo 1
        xi(k)= f_find(n, a)
        xi(k)=2
    %passo 2 
        xf = f_npolinomios(n, a, xi(k))

end