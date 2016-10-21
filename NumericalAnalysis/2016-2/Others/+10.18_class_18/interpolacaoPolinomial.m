function coeficientes = interpolacaoPolinomial( x, y, grau_do_polinomio )
    
    numero_de_coeficientes = grau_do_polinomio + 1;
    
    for i = 1 : numero_de_coeficientes
        
        A( i, 1 ) = 1;
        A( i, 2 ) = x( i );
        
        for j = 3 : numero_de_coeficientes
            
            A( i, j ) = A( i, j - 1 )*x( i );
            
        end
        
        A( i, numero_de_coeficientes + 1 ) = y( i );
        
    end
    
    A;
    coeficientes = fgauss( numero_de_coeficientes, A );
    
end
