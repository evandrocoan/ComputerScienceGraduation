#
# fNewtonSistemasNaoLineares com derivada exata.
#
# 1) Chutar um valor inicial real, não converge caso as raízes da solução sejam complexas.
# 2) Mas chutar um valor initial complexo, converge caso as raízes sejam reais ou complexas.
#
function [ x, passos, residuo_fgauss, residuo_sistema ] = fNewtonSistemasNaoLineares_a( xi, tolerancia )

    % São três passos:
    % 1) Calcular delta x (dx)
    # 2) Calcular x
    # 3) Calcular xi

    criterio = 1;
    contador = 0;
    dx       = [ 1e-4, 1e-4 ];

    while criterio > tolerancia && contador < 120

        contador = contador + 1;

        A( 1, 1 ) =   cos( xi(1) );
        A( 1, 2 ) = - sin( xi(2) );
        A( 1, 3 ) = - f1( xi(1), xi(2) );

        A( 2, 1 ) = 2*xi(1);
        A( 2, 2 ) = 2*xi(2);
        A( 2, 3 ) = - f2( xi(1), xi(2) );

        A;
        dx = fgauss( 2, A );

        residuo_fgauss = rmax( A, 2, dx );
        criterio       = max( abs( dx) );

        x  = xi .+ dx;
        xi = x;

    end

    passos          = contador;
    residuo_sistema = max( abs( f1( x(1), x(2) ) ), abs( f2( x(1), x(2) ) ) );

end



function x = f1( x1, x2 )

    x = sin( x1 ) + cos( x2 ) - 1;

end

function x = f2( x1, x2 )

    x = x1^2 + x2^2 - 3;

end


