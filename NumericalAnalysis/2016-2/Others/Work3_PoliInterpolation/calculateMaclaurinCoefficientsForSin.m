

#
# Maclaurin coefficients for the function: log( x ), on the converted Domain [-1, 1]
#
# MaclaurinSeries( 0 ) = f( 0 ) + (f'( 0 )*z^1) / 1! + (f''( 0 )*z^2) / 2! + ... + (f^n'( 0 )*z^n) / n!
#
# For the Domain [a, b]
# MaclaurinSeries( 0 ) = f( 0 )
#                        + (f'( 0 )*z^1) / 1!
#                        + (f''( 0 )*z^2) / 2!
#                        + ...
#                        + (f^n'( 0 )*z^n) / n!
#
# For other series as sin( x ), the domain conversion is not necessary as they already may be
# evaluated on the domain [-1, 1].
#
function coef = calculateMaclaurinCoefficientsForSin( n, a, b )

    # Here we applicate the first coefficient from the Maclaurin series on its Domain [-1, 1]
    # middle point 0. Before apply our function `targetFunction` we need to convert from the
    # Domain [-1, 1] to the original or correct Domain [a, b] for the function `targetFunction`.
    # This is the whole reason why we may apply the derivative functions on the 0 point.
    MaclaurinDomainPointZero = ChebyshevDomainLinearTransformationOut( 0, a, b );

    coef( 1 ) = sin( MaclaurinDomainPointZero );

    # Precisamos dos zeros em séries como sen(x) que já são entre [-1, 1]
    # Assim um for antes de calcular a sério serve para completar os zeros serve para corrigir/ajudar.
    #
    # for i = 0 : 2 : n
    #
    #     c( i+1 ) = 0;
    #
    # end
    for i = 0 : 2 : n

        c( i+1 ) = 0;

    end

    function_sign = 1;

    for i = 1 : 2 : n + 1

        coef( i+1 ) = function_sign * (1) / factorial(i);
        function_sign = function_sign * (-1);

    end

end








