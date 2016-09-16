function x = fMetodoDeNewton( xi, tolerancia )

    x         = xi;
    passos    = 0;
    diferenca = 1;

    fator = 0.65;
    aux   = 1 - fator;

    while( diferenca > tolerancia && passos < 10 )

        passos = passos + 1
        
        x = x - f( x ) / f_devidada1( x )
        
        diferenca = abs( x - xi )
        xi = x;
    end

    passos
    diferenca

end



