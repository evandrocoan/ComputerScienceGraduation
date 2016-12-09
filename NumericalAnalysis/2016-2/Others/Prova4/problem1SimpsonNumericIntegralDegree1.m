


function aproximateIntegral = problem1SimpsonNumericIntegralDegree1( n, a, b )

    h = (b-a) / n;
    x = a : h : b;

    y = fProblem1a( x );

    soma = 0;

    for i = 2 : n

        soma = soma + y( i );

    end

    aproximateIntegral = 0.5 * h * ( y(1) + 2*soma + y(n+1) );

end



