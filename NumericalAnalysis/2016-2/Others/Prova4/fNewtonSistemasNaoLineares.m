#
# fNewtonSistemasNaoLineares com derivada numérica.
#
# 1) Chutar um valor inicial real, não converge caso as raízes da solução sejam complexas.
# 2) Mas chutar um valor initial complexo, converge caso as raízes sejam reais ou complexas.
#
function x = fNewtonSistemasNaoLineares( xi, functions )

    % São três passos:
    % Calcular delta x (dx)
    # Calcular x
    # Calcular xi

    criterio   = 1;
    tolerancia = 1e-14;
    contador   = 0;
    dx         = [ 1e-4, 1e-4 ];

    while criterio > tolerancia && contador < 100

        contador = contador + 1;

        A( 1, 1 ) = ( functions(1).vector( [ xi(1) + dx( 1 ), xi(2)         ] ) - functions(1).vector( xi ) ) / dx(1);
        A( 1, 2 ) = ( functions(1).vector( [ xi(1)          , xi(2) + dx(2) ] ) - functions(1).vector( xi ) ) / dx(2);
        A( 1, 3 ) = - functions(1).vector( xi );

        A( 2, 1 ) = ( functions(2).vector( [ xi(1) + dx( 1 ), xi(2)         ] ) - functions(2).vector( xi ) ) / dx(1);
        A( 2, 2 ) = ( functions(2).vector( [ xi(1)          , xi(2) + dx(2) ] ) - functions(2).vector( xi ) ) / dx(2);
        A( 2, 3 ) = - functions(2).vector( xi );

        A;
        dx = fgauss( 2, A );
        printf( "dx(1): %30.20f, dx(2): %30.20f\n", dx(1), dx(2) );

        residuo_max = rmax(A,2,dx);
        criterio    = max( abs( dx) );

        x  = xi .+ dx;
        xi = x;

    end
end
