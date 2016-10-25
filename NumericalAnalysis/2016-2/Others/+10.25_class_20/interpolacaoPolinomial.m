#
# Matrix form:
#
# a( 1 )*x(1)^0 + a( 2 )*x(1)^1 + a( 3 )*x(1)^2 = f( x(1) )
# a( 1 )*x(2)^0 + a( 2 )*x(2)^1 + a( 3 )*x(2)^2 = f( x(2) )
# a( 1 )*x(3)^0 + a( 2 )*x(3)^1 + a( 3 )*x(3)^2 = f( x(3) )
#
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
