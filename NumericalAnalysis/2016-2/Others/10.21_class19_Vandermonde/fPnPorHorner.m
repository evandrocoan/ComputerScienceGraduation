
#
# Calculates a given polynom as a coefficients vector on the given point(s).
#
# n , the polynom degree
# a , the polynom coefficients on the form: 0 + 5*x^1 + 0*x^2 - 20*x^3 + 0*x^4 +16*x^5
# xp, the vector or a point to evaluate the polynom on, as `0.5`: 0 + 5*(0.5)^1 + 0*(0.5)^2 - 20*(0.5)^3 + 0*(0.5)^4 +16*(0.5)^5
#
# @return the polynom's value on the given point. If a point's vector is given, a polynom's value
#         vector within the points calculated is returned.
#
function yp = fPnPorHorner( n, a, xp )

    pointsToProcess = length( xp );

    for k = 1 : pointsToProcess

        # Precisamos limpar auxiliar a cada iteração
        aux = a( n + 1 );

        for i = n : -1 : 1

            aux = a( i ) + xp( k )*aux;

        end

        yp( k ) = aux;

    end

end



