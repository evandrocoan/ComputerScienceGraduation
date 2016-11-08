
#
# Evaluate the t variable at the k'th Chebyshev Polynom
#
# i = 1 : n
# fChebyshev( i ) = b0*T0( t(i) ) + b1*T1( t(i) ) + b2*T2( t(i) ) + b3*T3( t(i) ) + ...
#
# @param k, the k'th Chebyshev Polynom
# @param t, the value to evaluate at the k'th Chebyshev Polynom
#
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
function result = getChebyshevCoefficientsPolinom( k )

    # https://en.wikibooks.org/wiki/Octave_Programming_Tutorial/Polynomials
    persistent chebyshevPolynomCoefficients;
    computed = numel( chebyshevPolynomCoefficients );

    % printf( 'Calling Chebyshev Polynom Coefficients with computed = %d and k = %d\n', computed, k );

    % When the function is called for the first time, initialize the first element.
    if computed == 0

        computed                                  = 1;
        chebyshevPolynomCoefficients( 1 ).polynom = [ 1 ];

    end

    % Compute in uncomputed `chebyshevPolynomCoefficients`. The indexes are `k + 1` shifted because
    % the b's Chebyshev Polynom Coefficients starts on 0, but octave only allow indexes starting
    % at 1.
    if k + 1 > computed

        % This starts calculating all the missing b's Chebyshev Polynom from the index `computed` until
        % the requested coefficient `k`.
        for i = computed : k

            % printf( 'Starting computing the %d coefficient of %d (k) coefficients.\n', i, k );

            if i == 0

                # polyout( [ 1 ], "x" )
                # 1
                chebyshevPolynomCoefficients( i + 1 ).polynom = [ 1 ];

            elseif i == 1

                # polyout( [ 1, 0 ], "x" )
                # 1*x^1 + 0
                chebyshevPolynomCoefficients( i + 1 ).polynom = [ 1, 0 ];

            elseif mod( i, 2 ) == 0

                # 2*getChebyshevCoefficientsPolinom( n/2 )^2 - 1
                polynom        = getChebyshevCoefficientsPolinom( i/2 );
                polynom        = 2.*conv( polynom, polynom );
                polynom( end ) = polynom( end ) - 1;

                chebyshevPolynomCoefficients( i + 1 ).polynom = polynom;

            else

                # 2*getChebyshevCoefficientsPolinom( (n-1)/2 )*getChebyshevCoefficientsPolinom( (n+1)/2 ) - x
                polynom = 2.*conv( getChebyshevCoefficientsPolinom( (i-1) / 2 ), ...
                                   getChebyshevCoefficientsPolinom( (i+1) / 2 ) );
                
                polynom( end - 1 )                            = polynom( end - 1 ) - 1;
                chebyshevPolynomCoefficients( i + 1 ).polynom = polynom;

            end

        end

    end

    # Structure Arrays
    # https://www.gnu.org/software/octave/doc/v4.0.0/Structure-Arrays.html
    result = chebyshevPolynomCoefficients( k + 1 ).polynom;

end


# Include the T0, T1, ... Chebyshev's Polynoms of First Kind
source( "ChebyshevPolynomsOfFirstKindList.m" )




format long;
split_long_rows(0)

value = 0.6;
printf( '\n' )

T0_correct = polyout( T0(), "x" )
polynom    = getChebyshevCoefficientsPolinom( 0 );
n          = numel( polynom ) - 1;
T0_calcula = polyout( polynom, "x" )
correct_va = T0_( value )
calcula_va = polyval( getChebyshevCoefficientsPolinom( 0 ), value )
briotRunif = fPnPorBriotRunifi( n, fliplr( polynom ), value )
printf( '\n' )

T1_correct = polyout( T1(), "x" )
polynom    = getChebyshevCoefficientsPolinom( 1 );
n          = numel( polynom ) - 1;
T1_calcula = polyout( polynom, "x" )
correct_va = T1_( value )
calcula_va = polyval( getChebyshevCoefficientsPolinom( 1 ), value )
briotRunif = fPnPorBriotRunifi( n, fliplr( polynom ), value )
printf( '\n' )

T2_correct = polyout( T2(), "x" )
polynom    = getChebyshevCoefficientsPolinom( 2 );
n          = numel( polynom ) - 1;
T2_calcula = polyout( polynom, "x" )
correct_va = T2_( value )
calcula_va = polyval( getChebyshevCoefficientsPolinom( 2 ), value )
briotRunif = fPnPorBriotRunifi( n, fliplr( polynom ), value )
printf( '\n' )

T3_correct = polyout( T3(), "x" )
polynom    = getChebyshevCoefficientsPolinom( 3 );
n          = numel( polynom ) - 1;
T3_calcula = polyout( polynom, "x" )
correct_va = T3_( value )
calcula_va = polyval( getChebyshevCoefficientsPolinom( 3 ), value )
briotRunif = fPnPorBriotRunifi( n, fliplr( polynom ), value )
printf( '\n' )

T4_correct = polyout( T4(), "x" )
polynom    = getChebyshevCoefficientsPolinom( 4 );
n          = numel( polynom ) - 1;
T4_calcula = polyout( polynom, "x" )
correct_va = T4_( value )
calcula_va = polyval( getChebyshevCoefficientsPolinom( 4 ), value )
briotRunif = fPnPorBriotRunifi( n, fliplr( polynom ), value )
printf( '\n' )

T5_correct = polyout( T5(), "x" )
polynom    = getChebyshevCoefficientsPolinom( 5 );
n          = numel( polynom ) - 1;
T5_calcula = polyout( polynom, "x" )
correct_va = T5_( value )
calcula_va = polyval( getChebyshevCoefficientsPolinom( 5 ), value )
briotRunif = fPnPorBriotRunifi( n, fliplr( polynom ), value )
printf( '\n' )

T6_correct = polyout( T6(), "x" )
polynom    = getChebyshevCoefficientsPolinom( 6 );
n          = numel( polynom ) - 1;
T6_calcula = polyout( polynom, "x" )
correct_va = T6_( value )
calcula_va = polyval( getChebyshevCoefficientsPolinom( 6 ), value )
briotRunif = fPnPorBriotRunifi( n, fliplr( polynom ), value )
printf( '\n' )

T7_correct = polyout( T7(), "x" )
polynom    = getChebyshevCoefficientsPolinom( 7 );
n          = numel( polynom ) - 1;
T7_calcula = polyout( polynom, "x" )
correct_va = T7_( value )
calcula_va = polyval( getChebyshevCoefficientsPolinom( 7 ), value )
briotRunif = fPnPorBriotRunifi( n, fliplr( polynom ), value )
printf( '\n' )

T8_correct = polyout( T8(), "x" )
polynom    = getChebyshevCoefficientsPolinom( 8 );
n          = numel( polynom ) - 1;
T8_calcula = polyout( polynom, "x" )
correct_va = T8_( value )
calcula_va = polyval( getChebyshevCoefficientsPolinom( 8 ), value )
briotRunif = fPnPorBriotRunifi( n, fliplr( polynom ), value )
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
T9_correct = polyout( T9(), "x" )
polynom    = getChebyshevCoefficientsPolinom( 9 );
n          = numel( polynom ) - 1;
T9_calcula = polyout( polynom, "x" )
correct_va = T9_( value )
calcula_va = polyval( getChebyshevCoefficientsPolinom( 9 ), value )
briotRunif = fPnPorBriotRunifi( n, fliplr( polynom ), value )
printf( '\n' )



value = 0.4
printf( '\n' )

T6_correct = polyout( T6(), "x" )
polynom    = getChebyshevCoefficientsPolinom( 6 );
n          = numel( polynom ) - 1;
T6_calcula = polyout( polynom, "x" )
correct_va = T6_( value )
calcula_va = polyval( getChebyshevCoefficientsPolinom( 6 ), value )
briotRunif = fPnPorBriotRunifi( n, fliplr( polynom ), value )
printf( '\n' )

T7_correct = polyout( T7(), "x" )
polynom    = getChebyshevCoefficientsPolinom( 7 );
n          = numel( polynom ) - 1;
T7_calcula = polyout( polynom, "x" )
correct_va = T7_( value )
calcula_va = polyval( getChebyshevCoefficientsPolinom( 7 ), value )
briotRunif = fPnPorBriotRunifi( n, fliplr( polynom ), value )
printf( '\n' )

T8_correct = polyout( T8(), "x" )
polynom    = getChebyshevCoefficientsPolinom( 8 );
n          = numel( polynom ) - 1;
T8_calcula = polyout( polynom, "x" )
correct_va = T8_( value )
calcula_va = polyval( getChebyshevCoefficientsPolinom( 8 ), value )
briotRunif = fPnPorBriotRunifi( n, fliplr( polynom ), value )
printf( '\n' )

T9_correct = polyout( T9(), "x" )
polynom    = getChebyshevCoefficientsPolinom( 9 );
n          = numel( polynom ) - 1;
T9_calcula = polyout( polynom, "x" )
correct_va = T9_( value )
calcula_va = polyval( getChebyshevCoefficientsPolinom( 9 ), value )
briotRunif = fPnPorBriotRunifi( n, fliplr( polynom ), value )
printf( '\n' )



value = 0.6
printf( '\n' )

T9_correct = polyout( T9(), "x" )
polynom    = getChebyshevCoefficientsPolinom( 9 );
n          = numel( polynom ) - 1;
T9_calcula = polyout( polynom, "x" )
correct_va = T9_( value )
calcula_va = polyval( getChebyshevCoefficientsPolinom( 9 ), value )
briotRunif = fPnPorBriotRunifi( n, fliplr( polynom ), value )
printf( '\n' )

% polyval( [ 1, 1 ], 2 )









