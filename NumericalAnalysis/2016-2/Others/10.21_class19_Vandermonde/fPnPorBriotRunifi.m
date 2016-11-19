
#
# DO NOT USE THIS `SHIT` NEVER EVER! Use `fPnPorHorner` instead of this SHIT.
#
function yp = fPnPorBriotRunifi( n, a, xp )

    # The input polynom must to be on the form: 0 + 5*x^1 + 0*x^2 - 20*x^3 + 0*x^4 +16*x^5
    # Without any leading zeros after the last `x^n` coefficient.
    #
    a      = fliplr( a );
    b( 1 ) = a( 1 );

    pointsToProcess = length( xp );

    for k = 1 : pointsToProcess

        for i = 2 : n + 1

            b( i ) = a( i ) + xp( k )*b( i - 1 );

        end

        yp( k ) = b( n + 1 );

    end

end



