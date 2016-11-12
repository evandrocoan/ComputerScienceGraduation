
#
# Evaluate the Chebyshev Polynom
#
# i = 1 : k
# fChebyshev( i ) = b0*T0( t(i) ) + b1*T1( t(i) ) + b2*T2( t(i) ) + b3*T3( t(i) ) + ...
#
# yAproximado = coef_b( 1)*T0( tInterPontos ) ...
#             + coef_b( 2)*T1( tInterPontos ) ...
#             + coef_b( 3)*T2( tInterPontos ) ...
#             + coef_b( 4)*T3( tInterPontos ) ...
#             + coef_b( 5)*T4( tInterPontos ) ...
#             + coef_b( 6)*T5( tInterPontos ) ...
#             + coef_b( 7)*T6( tInterPontos ) ...
#             + coef_b( 8)*T7( tInterPontos ) ...
#             + coef_b( 9)*T8( tInterPontos ) ...
#             + coef_b(10)*T9( tInterPontos )
#
#
# @param n                   , Chebyshev's series degree.
# @param b                   , the `n` b's Chebyshev coefficients.
# @param t                   , the [-1, 1] points to evaluate the Chebyshev Polynom.
# @param chebyshevPolynomType, a function handle to get the nth Chebyshev Polynom.
#
function value = evaluateChebyshevPolynom( n, b, t, chebyshevPolynomType )

    b;
    value = 0;

    # At the first time, reset the last values result, which are wrong for this new `t`'s values.
    printf( '( evaluateChebyshevPolynom ) Evaluating the %dth b''s coefficient by ', 0 ); chebyshevPolynomType
    value = value + b( 1 )*chebyshevPolynomType( 0, t, true );

    for i = 2 : n

        printf( '( evaluateChebyshevPolynom ) Evaluating the %dth b''s coefficient by ', i - 1 ); chebyshevPolynomType

        # When using n = 20 and m = 40. This is due the recursive remember feature set to false.
        #
        # Setting this to true cause to force the `getnthChebyshevCoefficientsNumerically` to do:
        # Time(s): 38.566, Time(%): 83.84 and Calls: 110.981
        #
        # Setting this to false cause to force the `getnthChebyshevCoefficientsNumerically` to do:
        # Time(s): 5.409, Time(%): 55.65 and Calls: 20.756
        #
        value = value + b( i )*chebyshevPolynomType( i - 1, t, false );

    end

end



