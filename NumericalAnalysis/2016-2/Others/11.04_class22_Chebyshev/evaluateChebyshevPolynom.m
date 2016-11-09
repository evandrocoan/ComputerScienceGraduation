
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
# @param n, Chebyshev's series degree.
# @param b, the `n` b's Chebyshev coefficients.
# @param t, the [-1, 1] points to evaluate the Chebyshev Polynom.
#
function value = evaluateChebyshevPolynom( n, b, t, chebyshevPolynomType )

    b;
    value = 0;

    for i = 1 : n

        i;
        value = value + b( i )*chebyshevPolynomType( i - 1, t, true );

    end

end



