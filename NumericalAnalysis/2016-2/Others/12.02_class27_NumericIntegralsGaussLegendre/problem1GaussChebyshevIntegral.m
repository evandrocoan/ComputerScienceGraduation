

function gaussChebyshevIntegral = problem1GaussChebyshevIntegral( m, targetFunction )

    summation = 0;

    for i = 1 : m

        summation = summation + C(m,i)*problem1Function( x(i) );

    end

    gaussChebyshevIntegral = (pi/m)*summation;

end

