

function [ x, passos, diferenca ] = fMetodoDeNewtonOrdem2( xi, tolerancia )

    x         = xi;
    passos    = 0;
    diferenca = 1;

    fator = 0.65;
    aux   = 1 - fator;

    while( diferenca > tolerancia && passos < 120 )

        passos = passos + 1;

        f0 = f( x );
        f1 = f_devidada1( x );
        f2 = f_devidada2( x );
        
        # o metodo de newton é somente essa linha,
        # o que faz o processo evoluir é enquanto
        x = x - 2*f0 / ( f1 + sign( f1 )*( f1^2 - 2*( f2 )*f0 )^( 1/2 ) );

        diferenca = abs( x - xi );
        xi = x;

    end

    passos;
    diferenca;

end


# Dom = x \in ( 0, +inf )
function x = f( x )

    x = x^10 - 2;

end

function x = f_devidada1( x )

    x = 10*x^9;

end

function x = f_devidada2( x )

    x = 90*x^8;

end


