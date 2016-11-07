

#
# Function: log( x )
# For the Domain [-1, 1]
# MaclaurinSeries( 0 ) = f( 0 ) + (f'( 0 )*z^1) / 1! + (f''( 0 )*z^2) / 2! + ... + (f^n'( 0 )*z^n) / n!
#
# For the Domain [a, b]
# MaclaurinSeries( 0 ) = f( 0 )
#                        + (f'( 0 )*z^1) / 1!
#                        + (f''( 0 )*z^2) / 2!
#                        + ...
#                        + (f^n'( 0 )*z^n) / n!
#
function coef = Maclaurin( n, a, b, targetFunction )

    # Here we applicate the first coefficient from the Maclaurin series on its Domain [-1, 1]
    # middle point 0. Before apply our function `targetFunction` we need to convert from the 
    # Domain [-1, 1] to the original or correct Domain [a, b] for the function `targetFunction`.
    # This is the whole reason why we may apply the derivative functions on the 0 point.
    MaclaurinDomainPointZero = MaclaurinLinearTransformationDomainOut( 0, a, b )

    cache = ( b - a ) / ( b + a )
    coef( 1 ) = targetFunction( MaclaurinDomainPointZero )

    for i = 2 : n + 1

        coef( i ) =  (-1)^(i) * (cache^(i-1)) / (i-1);

    end

end








