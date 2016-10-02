function x = fMetodoDaIteracaoLinear( xi, tolerancia )

    x         = xi;
    passos    = 0;
    diferenca = 1;

    fator = 0.65;
    aux   = 1 - fator;

    while( diferenca > tolerancia && passos < 100 )

        passos = passos + 1;

        % Esse primeiro somente converge com fator de sub-relaxação menor do que 0.6
        %x = aux * x + fator * 1 / log( x )  % com fator = 0.35 convergiu em 4 passos

        % Esse segundo converge bem sem fator de sub-relaxação
         x = aux * x + fator * exp( 1 / x ); % com fator = 0.65 convergiu em 4 passos

        diferenca = abs( x - xi );
        xi = x;
    end

    passos 
    diferenca 

end
