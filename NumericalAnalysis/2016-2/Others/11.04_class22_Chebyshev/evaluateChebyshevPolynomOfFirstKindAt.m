
#
# Evaluate the t variable at the k'th Chebyshev Polynom
#
# i = 1 : n
# fChebyshev( i ) = b0*T0( t(i) ) + b1*T1( t(i) ) + b2*T2( t(i) ) + b3*T3( t(i) ) + ...
#
# @param k, the k'th Chebyshev Polynom
# @param t, the value to evaluate at the k'th Chebyshev Polynom
#

function value = evaluateChebyshevPolynomOfFirstKindAt( k, t )

    persistent currentSequence = 0;

    switch( k )

        case 0
            value = T0( t );

        case 1
            value = T1( t );

        case 2
            value = T2( t );

        case 3
            value = T3( t );

        case 4
            value = T4( t );

        case 5
            value = T5( t );

        case 6
            value = T6( t );

        case 7
            value = T7( t );

        case 8
            value = T8( t );

        case 9
            value = T9( t );

         otherwise
            currentSequence = mod( currentSequence + 1, 2 );
            value           = getnthChebyshevPolynomOfFirstKind( k, t, currentSequence );

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
function result = getnthChebyshevPolynomOfFirstKind( k, t, sequence )

    persistent originalValue;
    persistent chebyshevPolynomCoefficients;

    computed = numel( chebyshevPolynomCoefficients );
    % printf( 'Calling Chebyshev Polynom Coefficients with computed = %d and k = %d\n', computed, k );

    % When the function is called for the first time, initialize the first element and also reset
    % the old data, when a new variable `t` is calculated.
    if computed == 0 || originalValue ~= sequence

        % printf( 'Cleaning chebyshevPolynomCoefficients! computed: %d, k: %d\n\n\n\n\n', computed, k );

        computed      = 1;
        originalValue = sequence;

        chebyshevPolynomCoefficient( 1 ) = 1;
        chebyshevPolynomCoefficients     = chebyshevPolynomCoefficient;

    end

    % Compute in uncomputed `chebyshevPolynomCoefficients`. The indexes are `k + 1` shifted because
    % the b's Chebyshev Polynom Coefficients starts on 0, but octave only allow indexes starting
    % at 1. This starts calculating all the missing b's Chebyshev Polynom from the index `computed`
    % until the requested coefficient `k`.

    if k + 1 > computed

        for i = computed : k + 1

            % printf( 'Starting computing the %d coefficient of %d (k) coefficients.\n', i, k );

            if i == 0

                chebyshevPolynomCoefficients( i + 1 ) = 1;

            elseif i == 1

                chebyshevPolynomCoefficients( i + 1 ) = t;

            elseif mod( i, 2 ) == 0

                chebyshevPolynomCoefficients( i + 1 ) = 2.*getnthChebyshevPolynomOfFirstKind( i/2, t, sequence ).^2 .- 1;

            else

                chebyshevPolynomCoefficients( i + 1 ) = 2.*getnthChebyshevPolynomOfFirstKind( (i-1) / 2, t, sequence ) ...
                                                         .*getnthChebyshevPolynomOfFirstKind( (i+1) / 2, t, sequence ) - t;

            end

        end

    end

    result = chebyshevPolynomCoefficients( k + 1 );

end



format long;
split_long_rows(0)

value = 0.6;

T0_correct = T0( value )
T0_calcula = getnthChebyshevPolynomOfFirstKind( 0, value, 0 )
printf( '\n' )

T1_correct = T1( value )
T1_calcula = getnthChebyshevPolynomOfFirstKind( 1, value, 0 )
printf( '\n' )

T2_correct = T2( value )
T2_calcula = getnthChebyshevPolynomOfFirstKind( 2, value, 0 )
printf( '\n' )

T3_correct = T3( value )
T3_calcula = getnthChebyshevPolynomOfFirstKind( 3, value, 0 )
printf( '\n' )

T4_correct = T4( value )
T4_calcula = getnthChebyshevPolynomOfFirstKind( 4, value, 0 )
printf( '\n' )

T5_correct = T5( value )
T5_calcula = getnthChebyshevPolynomOfFirstKind( 5, value, 0 )
printf( '\n' )

T6_correct = T6( value )
T6_calcula = getnthChebyshevPolynomOfFirstKind( 6, value, 0 )
printf( '\n' )

T7_correct = T7( value )
T7_calcula = getnthChebyshevPolynomOfFirstKind( 7, value, 0 )
printf( '\n' )

T8_correct = T8( value )
T8_calcula = getnthChebyshevPolynomOfFirstKind( 8, value, 0 )
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
T9_correct = T9( value )
T9_calcula = getnthChebyshevPolynomOfFirstKind( 9, value, 0 )
printf( '\n' )



value = 0.4

T6_correct = T6( value )
T6_calcula = getnthChebyshevPolynomOfFirstKind( 6, value, 1 )
printf( '\n' )

T7_correct = T7( value )
T7_calcula = getnthChebyshevPolynomOfFirstKind( 7, value, 1 )
printf( '\n' )

T8_correct = T8( value )
T8_calcula = getnthChebyshevPolynomOfFirstKind( 8, value, 1 )
printf( '\n' )

T9_correct = T9( value )
T9_calcula = getnthChebyshevPolynomOfFirstKind( 9, value, 1 )
printf( '\n' )



value = 0.6

T9_correct = T9( value )
T9_calcula = getnthChebyshevPolynomOfFirstKind( 9, value, 0 )
printf( '\n' )




