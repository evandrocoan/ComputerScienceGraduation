
#
# Evaluate the t variable at the k'th Chebyshev Polynom
#
# i = 1 : n
# fChebyshev( i ) = b0*T0( t(i) ) + b1*T1( t(i) ) + b2*T2( t(i) ) + b3*T3( t(i) ) + ...
#
# @param k, the k'th Chebyshev Polynom
# @param t, the value to evaluate at the k'th Chebyshev Polynom
#

function value = evaluateChebyshevPolynomOfFirstKindAt_( k, t )

    switch( k )

        case 0
            value = T0_( t );

        case 1
            value = T1_( t );

        case 2
            value = T2_( t );

        case 3
            value = T3_( t );

        case 4
            value = T4_( t );

        case 5
            value = T5_( t );

        case 6
            value = T6_( t );

        case 7
            value = T7_( t );

        case 8
            value = T8_( t );

        case 9
            value = T9_( t );

         otherwise
            value = getnthChebyshevPolynomOfFirstKind_( k, t );

    end

end

# Include the T0, T1, ... Chebyshev's Polynoms of First Kind
source( "ChebyshevPolynomsOfFirstKindList.m" )



#
# Efficient Computation of Chebyshev Polynomials in Computer Algebra
# http://www.mathematik.uni-kassel.de/koepf/cheby.pdf
#
# Mathematics 4330/5344–#5 Approximation of Functions
# http://texas.math.ttu.edu/~gilliam/ttu/m4330/m4330_5.pdf
#
# Are there functions that remember values they have found on Octave?
# http://stackoverflow.com/questions/40445316/are-there-functions-that-remember-values-they-have-found-on-octave
#
# @param k       , the k'th Chebyshev Polynom
# @param t       , the value to evaluate at the k'th Chebyshev Polynom
# @param sequence, 0 or 1 to indicate the current `t`'s Chebyshev Polynom sequence.
#                  For example, to calculate the `t` = 0.6's Chebyshev Polynoms, use the
#                  `sequence` as 0.
#                  And later when calculating the Chebyshev Polynom for `t` = 0.4, set this
#                  value to 1, to clear the last Chebyshev Polynom cached values for `t` = 0.6.
#
function result = getnthChebyshevPolynomOfFirstKind_( k, t )
    
    polynom = evaluateChebyshevPolynomOfFirstKindAt( k );
    result  = polyval( polynom, t );
    
end

