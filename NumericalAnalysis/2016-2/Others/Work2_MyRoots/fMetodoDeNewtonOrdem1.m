

function [ x, passos, diferenca ] = fMetodoDeNewtonOrdem1( xi, tolerancia )

    x         = xi;
    passos    = 0;
    diferenca = 1;

    fator = 0.65;
    aux   = 1 - fator;

    while( diferenca > tolerancia && passos < 120 )

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

    x = x^10 - 2;

end

function x = f_devidada1( x )

    x = 10*x^9;

end


