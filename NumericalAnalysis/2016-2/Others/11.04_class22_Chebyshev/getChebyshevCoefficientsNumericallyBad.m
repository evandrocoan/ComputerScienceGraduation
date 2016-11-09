

#
# Comparison with n = 10 and m = 1000 between:
# `getChebyshevCoefficientsNumerically` and
# `getChebyshevCoefficientsNumericallyBad`
#
# errorByPolinom__ =    5.23940178675363e-09
# errorNumerically =    5.23940189106483e-09
# 512*s^10 + 0*s^9 - 1280*s^8 + 0*s^7 + 1120*s^6 + 0*s^5 - 400*s^4 + 0*s^3 + 50*s^2 + 0*s^1 - 1
#    #                                                                   Function Attr     Time (s)   Time (%)        Calls
# -------------------------------------------------------------------------------------------------------------------------
#  122 getChebyshevCoefficientsNumerically>getnthChebyshevCoefficientsNumerically    R        6.004      54.78        26436
#  121                                        getChebyshevCoefficientsNumerically             3.538      32.28           21
#  103                                                                   __line__             0.169       1.54            6
#   10                                                                   binary +             0.124       1.14        68793
#   75                                                                __go_axes__             0.092       0.84            1
#   19                                                                      numel             0.090       0.82        26618
#  107                                                                    cellfun             0.088       0.80           16
#  108                                                                __go_line__             0.076       0.69            6
#
#
# errorByPolinom__ =    5.23940178675363e-09
# errorNumerically =    5.23940189106483e-09
# 512*s^10 + 0*s^9 - 1280*s^8 + 0*s^7 + 1120*s^6 + 0*s^5 - 400*s^4 + 0*s^3 + 50*s^2 + 0*s^1 - 1
#    #                                                                   Function Attr     Time (s)   Time (%)        Calls
# -------------------------------------------------------------------------------------------------------------------------
#  123 getChebyshevCoefficientsNumerically>getnthChebyshevCoefficientsNumerically    R       10.359      39.44        49258
#  122                   getChebyshevCoefficientsNumerically>calculateTheNthValue    R        9.633      36.68        31242
#  121                                        getChebyshevCoefficientsNumerically             4.123      15.70           21
#   10                                                                   binary +             0.299       1.14       191324
#  124                                                                      floor             0.169       0.64        49258
#   20                                                                  binary ==             0.160       0.61        85785
#  103                                                                   __line__             0.160       0.61            6
#    5                                                                   binary /             0.153       0.58        86581
#
#

#
# Evaluate the t variable at the k'th Chebyshev Polynom
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
function value = getChebyshevCoefficientsNumericallyBad( k, t, isToDiscartTheSavedRecursion = true )

    persistent chebyshevPolynomCoefficients;
    persistent filledCoeffients = 0;

    chebyshevPolynomCoefficients;

    t_size   = numel( t );

    % printf( 'Calling Chebyshev Polynom Coefficients with filledCoeffients = ' );
    % printf( '%d, k = %d and t_size: %d \n', filledCoeffients, k, t_size );

    % When the function is called for the first time, initialize the first element and also reset
    % the old data when a new variable `t` is calculated.
    if filledCoeffients == 0 || isToDiscartTheSavedRecursion

        % printf( '\n\n\n\n\n( getChebyshevCoefficientsNumerically ) Cleaning ' );
        % printf(  'chebyshevPolynomCoefficients! filledCoeffients: %d, k: %d\n', filledCoeffients, k );

        # Initialize the vector to allow it to be passed by along
        chebyshevPolynomCoefficient( 1 ).vector = 0;

        for i = 1 : t_size

            chebyshevPolynomCoefficient( i ).vector = calculateTheNthValue( 0, 0, 0, chebyshevPolynomCoefficient( 1 ).vector );

        end

        filledCoeffients             = 1;
        chebyshevPolynomCoefficients = chebyshevPolynomCoefficient;

    end

    t_size;
    t;
    k;
    chebyshevPolynomCoefficients.vector;

    for i = 1 : t_size

        % printf( '( getChebyshevCoefficientsNumerically ) Calculating the %dth t''s vector point.\n', i );
        [ value( i ), filledCoeffients, chebyshevPolynomCoefficients( i ).vector ] = getnthChebyshevCoefficientsNumerically( ...
                k, t( i ), filledCoeffients, chebyshevPolynomCoefficients( i ).vector );

    end

end

# Include the T0, T1, ... Chebyshev's Polynoms of First Kind
source( "ChebyshevPolynomsOfFirstKindList.m" )

#
# Are there functions that remember values they have found on Octave?
# http://stackoverflow.com/questions/40445316/are-there-functions-that-remember-values-they-have-found-on-octave
#
# @param k                           , the k'th Chebyshev Polynom.
# @param t                           , the value to evaluate at the k'th Chebyshev Polynom.
# @param chebyshevPolynomCoefficients, a vector within the cached Chebyshev Polynom sequences.
#
function [ result, filledCoeffients, chebyshevPolynomCoefficients ] = ...
        getnthChebyshevCoefficientsNumerically( k, t, filledCoeffients, chebyshevPolynomCoefficients )

    k;
    t;

    % printf( '( getnthChebyshevCoefficientsNumerically ) Calling with filledCoeffients = ' );
    % printf( ' %d and k = %d\n', filledCoeffients, k );

    % Compute in uncomputed `chebyshevPolynomCoefficients`. The indexes are `k + 1` shifted because
    % the b's Chebyshev Polynom Coefficients starts on 0, but octave only allow indexes starting
    % at 1. This starts calculating all the missing b's Chebyshev Polynom from the index `filledCoeffients`
    % until the requested coefficient `k`.
    maxElements   = floor( k / 2 ) + 1;
    targetElement = k + 1;

    if maxElements + 1 > filledCoeffients

        for i = filledCoeffients : maxElements

            % printf( '( getnthChebyshevCoefficientsNumerically ) Starting computing the %d ', i );
            % printf( 'coefficient of %d (k) coefficients.\n',  k );

            chebyshevPolynomCoefficients = calculateTheNthValue( i, t, maxElements, chebyshevPolynomCoefficients );

        end

        # Calculate the desired element
        filledCoeffients             = maxElements;
        chebyshevPolynomCoefficients = calculateTheNthValue( k, t, filledCoeffients, chebyshevPolynomCoefficients );

    elseif targetElement > filledCoeffients

        chebyshevPolynomCoefficients = calculateTheNthValue( k, t, filledCoeffients, chebyshevPolynomCoefficients );

    end

    k;
    chebyshevPolynomCoefficients;

    result = chebyshevPolynomCoefficients( targetElement );

end

#
# Calculate the ith Chebyshev Coefficient. To get the nth Chebyshev Coefficient, this must to be
# called sequencially from the first ith value = 0, until the desired nth Chebyshev Coefficient.
#
# Efficient Computation of Chebyshev Polynomials in Computer Algebra
# http://www.mathematik.uni-kassel.de/koepf/cheby.pdf
#
# Mathematics 4330/5344–#5 Approximation of Functions
# http://texas.math.ttu.edu/~gilliam/ttu/m4330/m4330_5.pdf
#
# @param i                           , the ith coefficient to calculate by the recursion rule.
# @param chebyshevPolynomCoefficients, the full vector within the Chebyshev Coefficients
#
function chebyshevPolynomCoefficients = calculateTheNthValue( i, t, filledCoeffients, chebyshevPolynomCoefficients )

    if i == 0

        chebyshevPolynomCoefficients( i + 1, : ) = 1;

    elseif i == 1

        chebyshevPolynomCoefficients( i + 1, : ) = t;

    elseif mod( i, 2 ) == 0

        chebyshevPolynomCoefficients( i + 1, : ) = 2.*getnthChebyshevCoefficientsNumerically( ...
                i/2, t, filledCoeffients, chebyshevPolynomCoefficients ).^2 .- 1;

    else

        chebyshevPolynomCoefficients( i + 1, : ) = 2.*getnthChebyshevCoefficientsNumerically( ...
                (i-1) / 2, t, filledCoeffients, chebyshevPolynomCoefficients ).*getnthChebyshevCoefficientsNumerically( ...
                (i+1) / 2, t, filledCoeffients, chebyshevPolynomCoefficients ) - t;

    end

end



format long;
split_long_rows(0)

values( 1 ).vector = 0.6;
values( 2 ).vector = 0.4;
values( 3 ).vector = [ 0.4, 0.6 ];
values( 4 ).vector = [ 0.4, 0.6, 0.8 ];
values( 5 ).vector = [ 0.2, 0.4, 0.6, 0.8 ];

sizeof = numel( values )

% T9_correct = T9( values( 1 ).vector )
% T9_calcula = getChebyshevCoefficientsNumerically( 9, values( 1 ).vector, false )
% printf( '\n' )



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



