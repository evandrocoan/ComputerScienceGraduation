

#
# Maclaurin coefficients for the function: sin( x ), on the converted Domain [-1, 1]
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
function coef = calculateMaclaurinCoefficientsForEulerInteger( grau )

    # Precisamos dos zeros em séries como sen(x) que já são entre [-1, 1]
    # Assim um for antes de calcular a sério serve para completar os zeros serve para corrigir/ajudar.
    #
    # for i = 0 : 2 : n
    #
    #     c( i+1 ) = 0;
    #
    # end

    # coef 0 = 0
    # coef 1 = 1
    # coef 2 = 0
    # coef 3 =
    # coef 4 = 0

    for i = 1 : grau+1

        coef( i ) = 0;

    end

    for i = 0 : fix( ( grau - 1 ) / 2 )

        coef( 2*i+1+1 ) = (-1)^i / ( factorial(i)*(2*i + 1) );

    end

end








