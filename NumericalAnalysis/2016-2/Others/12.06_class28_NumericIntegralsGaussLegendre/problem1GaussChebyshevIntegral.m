

function gaussChebyshevIntegral = problem1GaussChebyshevIntegral( m, targetFunction, chebyshevPolynomDegree = 0 )

    summation               = 0;
    chebyshevPolynomDesired = getChebyshevPolinom( chebyshevPolynomDegree );

    %m 'nós' de Tchebyschev
    for k = 1 : m

        x = cos( (2*k-1) * pi/(2*m) );

        summation = summation + targetFunction( x )*polyval( chebyshevPolynomDesired, x );

    end

    gaussChebyshevIntegral = (pi/m)*summation;

end

