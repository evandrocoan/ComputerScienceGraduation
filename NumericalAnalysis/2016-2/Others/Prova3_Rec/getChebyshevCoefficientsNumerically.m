
#
# Evaluate the t variable at the k'th Chebyshev Polynom numerically calculated.
#
# i = 1 : n
# fChebyshev( i ) = b0*T0( t(i) ) + b1*T1( t(i) ) + b2*T2( t(i) ) + b3*T3( t(i) ) + ...
#
# @param k                           , the k'th Chebyshev Polynom.
# @param t                           , the value to evaluate at the k'th Chebyshev Polynom.
# @param isToDiscartTheSavedRecursion, true to indicate the current `t`'s Chebyshev Polynom sequence
#                                      must to be discarded. You must to always discard the old
#                                      sequence when a new `t` value is provided. Use false to
#                                      preserve the `t` cached values and use the recursion remember
#                                      feature.
#
function value = getChebyshevCoefficientsNumerically( k, t, isToDiscartTheSavedRecursion = true )

    persistent chebyshevPolynomCoefficients;
    chebyshevPolynomCoefficients;

    t_size   = numel( t );
    computed = numel( chebyshevPolynomCoefficients );

    # printf( 'Calling Chebyshev Polynom Coefficients with computed = %d, k = %d ', computed, k );
    # printf( 'and t_size: %d \n', t_size );

    # When the function is called for the first time, initialize the first element and also reset
    # the old data, when a new variable `t` is calculated.
    if computed == 0 || isToDiscartTheSavedRecursion

        # printf( '\n\n\n\n\n( getChebyshevCoefficientsNumerically ) Cleaning ' );
        # printf(  'chebyshevPolynomCoefficients! computed: %d, k: %d\n', computed, k );

        for i = 1 : t_size

            chebyshevPolynomCoefficient( i ).vector = 1;

        end

        computed                     = 1;
        chebyshevPolynomCoefficients = chebyshevPolynomCoefficient;

    end

    t_size;
    t;
    k;
    chebyshevPolynomCoefficients.vector;

    for i = 1 : t_size

        # printf( '( getChebyshevCoefficientsNumerically ) Calculating the %dth t''s vector point.\n', i );
        [ value( i ), chebyshevPolynomCoefficients( i ).vector ] = getnthChebyshevCoefficientsNumerically( ...
                k, t( i ), chebyshevPolynomCoefficients( i ).vector );

    end

end

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
# @param k                           , the k'th Chebyshev Polynom.
# @param t                           , the value to evaluate at the k'th Chebyshev Polynom.
# @param chebyshevPolynomCoefficients, a vector within the cached Chebyshev Polynom sequences.
#
function [ result, chebyshevPolynomCoefficients ] = ...
        getnthChebyshevCoefficientsNumerically( k, t, chebyshevPolynomCoefficients )

    t;
    computed = numel( chebyshevPolynomCoefficients );

    # printf( '( getnthChebyshevCoefficientsNumerically ) Calling with computed = ' );
    # printf( '%d and k = %d\n', computed, k );

    # Compute in uncomputed `chebyshevPolynomCoefficients`. The indexes are `k + 1` shifted because
    # the b's Chebyshev Polynom Coefficients starts on 0, but octave only allow indexes starting
    # at 1. This starts calculating all the missing b's Chebyshev Polynom from the index `computed`
    # until the requested coefficient `k`.
    if k + 1 > computed

        for i = computed : k

            # printf( '( getnthChebyshevCoefficientsNumerically ) Starting computing the %d ', i );
            # printf( 'coefficient of %d (k) coefficients.\n', k );

            if i == 0

                chebyshevPolynomCoefficients( i + 1, : ) = 1;

            elseif i == 1

                chebyshevPolynomCoefficients( i + 1, : ) = t;

            elseif mod( i, 2 ) == 0

                chebyshevPolynomCoefficients( i + 1, : ) = 2.*getnthChebyshevCoefficientsNumerically( ...
                        i/2, t, chebyshevPolynomCoefficients ).^2 .- 1;

            else

                chebyshevPolynomCoefficients( i + 1, : ) = 2.*getnthChebyshevCoefficientsNumerically( ...
                        (i-1) / 2, t, chebyshevPolynomCoefficients ).*getnthChebyshevCoefficientsNumerically( ...
                        (i+1) / 2, t, chebyshevPolynomCoefficients ) - t;

            end

        end

    end

    k;
    chebyshevPolynomCoefficients;

    result = chebyshevPolynomCoefficients( k + 1 );

end







# Include the T0, T1, ... Chebyshev's Polynoms of First Kind
source( "ChebyshevPolynomsOfFirstKindList.m" )

format long;
split_long_rows(0)

values( 1 ).vector = 0.6;
values( 2 ).vector = 0.4;
values( 3 ).vector = [ 0.4, 0.6 ];
values( 4 ).vector = [ 0.4, 0.6, 0.8 ];
values( 5 ).vector = [ 0.2, 0.4, 0.6, 0.8 ];



# T9_correct = T9( values( 1 ).vector )
# T9_calcula = getChebyshevCoefficientsNumerically( 9, values( 1 ).vector, false )
# printf( '\n' )




sizeof = numel( values )

for i = 1 : sizeof

    T0_correct = T0( values( i ).vector )
    T0_calcula = getChebyshevCoefficientsNumerically( 0, values( i ).vector )
    printf( '\n' )

    T1_correct = T1( values( i ).vector )
    T1_calcula = getChebyshevCoefficientsNumerically( 1, values( i ).vector, false )
    printf( '\n' )

    T2_correct = T2( values( i ).vector )
    T2_calcula = getChebyshevCoefficientsNumerically( 2, values( i ).vector, false )
    printf( '\n' )

    T3_correct = T3( values( i ).vector )
    T3_calcula = getChebyshevCoefficientsNumerically( 3, values( i ).vector, false )
    printf( '\n' )

    T4_correct = T4( values( i ).vector )
    T4_calcula = getChebyshevCoefficientsNumerically( 4, values( i ).vector, false )
    printf( '\n' )

    T5_correct = T5( values( i ).vector )
    T5_calcula = getChebyshevCoefficientsNumerically( 5, values( i ).vector, false )
    printf( '\n' )

    T6_correct = T6( values( i ).vector )
    T6_calcula = getChebyshevCoefficientsNumerically( 6, values( i ).vector, false )
    printf( '\n' )

    T7_correct = T7( values( i ).vector )
    T7_calcula = getChebyshevCoefficientsNumerically( 7, values( i ).vector, false )
    printf( '\n' )

    T8_correct = T8( values( i ).vector )
    T8_calcula = getChebyshevCoefficientsNumerically( 8, values( i ).vector, false )
    printf( '\n' )

    # value = 0.6;
    # T9_correct = -0.472103423999999
    # chebyshevPolynomCoefficients =
    #
    #    1.0000000000000000   0.6000000000000000  -0.2800000000000000  -0.9359999999999999  ...
    #   -0.8431999999999999  -0.0758399999999999   0.7521919999999997   0.9784703999999999  ...
    #    0.4219724799999998  -0.4721034240000002
    #
    # T9_calcula = -0.472103424000000
    #
    T9_correct = T9( values( i ).vector )
    T9_calcula = getChebyshevCoefficientsNumerically( 9, values( i ).vector, false )
    printf( '\n' )

end



