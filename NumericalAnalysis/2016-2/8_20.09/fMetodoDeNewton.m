function x = fMetodoDeNewton( xi, tolerancia )

    x         = xi;
    passos    = 0;
    diferenca = 1;

    fator = 0.65;
    aux   = 1 - fator;

    while( diferenca > tolerancia && passos < 10 )

        passos = passos + 1;
        
        # o metodo de newton é somente essa linha, 
        # o que faz o processo evoluir é enquanto
        x = x - f( x ) / f_devidada1( x );
        
        diferenca = abs( x - xi );
        xi = x;
    end

    passos;
    diferenca;

end


# Dom = x \in ( 0, +inf )
function x = f( x )
    
    x = x .* tan( x ) .- 1;
    
end

function x = f_devidada1( x )
    
    x = tan( x ) + x * ( 1 / cos( x ) ) ^ 2; % 1 / cos( x ) = sec( x )
    
end





