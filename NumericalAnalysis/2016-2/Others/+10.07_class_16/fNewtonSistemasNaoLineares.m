#
# fNewtonSistemasNaoLineares com derivada numérica.
#
# 1) Chutar um valor inicial real, não converge caso as raízes da solução sejam complexas.
# 2) Mas chutar um valor initial complexo, converge caso as raízes sejam reais ou complexas.
#
function x = fNewtonSistemasNaoLineares( xi )

    % São três passos:
    % Calcular delta x (dx)
    # Calcular x
    # Calcular xi
    
    criterio   = 1
    tolerancia = 1e-15
    contador   = 0
    dx         = [ 1e-4, 1e-4 ]
    
    while criterio > tolerancia && contador < 10
        
        contador = contador + 1
        
        A( 1, 1 ) = ( f1( [ xi(1) + dx( 1 ), xi(2)         ] ) - f1( xi ) ) / dx(1);
        A( 1, 2 ) = ( f1( [ xi(1)          , xi(2) + dx(1) ] ) - f1( xi ) ) / dx(2);
        A( 1, 3 ) = - f1( xi );
        
        A( 2, 1 ) = ( f2( [ xi(1) + dx( 1 ), xi(2)         ] ) - f2( xi ) ) / dx(1);
        A( 2, 2 ) = ( f2( [ xi(1)          , xi(2) + dx(1) ] ) - f2( xi ) ) / dx(2);
        A( 2, 3 ) = - f2( xi );
        
        A
        dx       = fgauss( 2, A )
        criterio = max( abs( dx) )
        
        x  = xi .+ dx
        xi = x
        
    end
end
