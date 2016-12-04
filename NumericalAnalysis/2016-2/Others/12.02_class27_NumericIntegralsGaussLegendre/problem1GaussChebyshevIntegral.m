

function gaussChebyshevIntegral = problem1GaussChebyshevIntegral( m, targetFunction )

    summation = 0;

    for k = 1 : m

        %m 'nós' de Tchebyschev
        x = cos((2*k-1)*pi/(2*m));

        summation = summation + targetFunction( x );

    end

    gaussChebyshevIntegral = (pi/m)*summation;

end

