
#{

Base de Lagrange:

Pn( x ) = \sum_i=1(y i )^n+1 * \prod_j=1( (x - x( j )) / (x( i ) - x( j )) )^n+1

Simplificando: 
Pn( x ) = \sum_i=1(y i )^n+1 * auxilar
onde auxilar = \prod_j=1( (x - x( j )) / (x( i ) - x( j )) )^n+1

#}

function yp = fPnCalculoPorLangrangeSemOsCoeficientes( n, x, y, xp )
    
    # Pn( x ) = \sum_i=1(y i )^n+1 * auxilar
    for k = 1 : length( xp )
        
        # Is a \sum
        yp( k ) = 0;
        
        for i = 1 : n + 1
            
            # Limpamos o resultado para novo calculo.
            # O elemento neutro do Produtório é 1.
            # O elemento neutro do Somatório  é 0.
            auxilar = 1;
            
            # auxilar = \prod_j=1&j!=i( (x - x( j )) / (x( i ) - x( j )) )^n+1
            for j = 1 : n + 1
                
                if( i != j )
                    
                    auxilar = auxilar*( xp( k ) - x( j )) / ( x( i ) - x( j ));
                    
                end
                
            end
            
            yp( k ) = yp( k ) + y( i )*auxilar;
            
        end
    
    end
    
end
