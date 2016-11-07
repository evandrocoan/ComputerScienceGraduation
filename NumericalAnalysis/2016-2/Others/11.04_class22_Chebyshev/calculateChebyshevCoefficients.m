
#
# Calculate the b's Chebyshev coefficients.
#
# i = 1 : k
# fChebyshev( i ) = b0*T0( t(i) ) + b1*T1( t(i) ) + b2*T2( t(i) ) + b3*T3( t(i) ) + ...
#
# b0 = 1/m \sum_j=1^m f( t(j) ), m = 10
#
# @param n             , Input, Integer, Grau da Série de Chebyshev.
# @param m             , Input, Integer, Grau de precisão da Integral Numérica, e também o número de nós de Chebyshev.
# @param a             , Input, real   , the function handle original starting domain from [a, b].
# @param b             , Input, real   , the function handle original ending domain from [a, b].
# @param targetFunction, Input, real   , targetFunction( X ), a function handle.
#
function coef = calculateChebyshevCoefficients( n, m, a, b, targetFunction, chebyshevPolynomType )

    # Calculamos os t(j)
    # Para encontrar os polinômios T1, T2, ..., consulte a tabela
    t( 1 ) = 0;

    #k=1:m; t=cos((2.*k.-1).*pi./(2.*m))
    for j = 1 : m

        t( j ) = cos( ( ( 2*j - 1 ) / (2*m) ) * pi );

    end

    t;
    x = MaclaurinLinearTransformationDomainOut( t, a, b );


    # Agora calculamos o b0 no indice 1 do array b
    soma = 0;

    for j = 1 : m

        # x^0 = T0
        #
        % Assuming f( x ) = log( x ), in [1, 2]
        % soma =  0.690064519186718
        % soma =  1.35558520936599
        % soma =  1.97268975784344
        % soma =  2.51907280649452
        % soma =  2.97536868263748
        % soma =  3.32728023686338
        % soma =  3.56866028787727
        % soma =  3.70532754177620
        % soma =  3.75839116920959
        % soma =  3.76452812919195
        % soma =  3.76452812919195
        %
        % soma = soma + fLog( x(j) ) * T0( t(j) );
        soma = soma + targetFunction( x(j) ) * chebyshevPolynomType( 0, t(j) );

    end

    correct_value = 3.76452812919195;
    soma;
    coef(1) = ( 1 / m ) * soma;


    # Agora calculamos o b1 no indice 2 do array b
    soma = 0;

    for j = 1 : m

        # x^1 = T1
        #
        % Assuming f( x ) = log( x ), in [1, 2]
        % soma =  0.681568679859111
        % soma =  1.27455195679119
        % soma =  1.71091076772066
        % soma =  1.95896348102698
        % soma =  2.03034388231146
        % soma =  1.97529278658466
        % soma =  1.86570853659771
        % soma =  1.76907019459963
        % soma =  1.72179015635938
        % soma =  1.71572875253810
        %
        % soma = soma + fLog( x(j) ) * T1( t(j) );
        soma = soma + targetFunction( x(j) ) * chebyshevPolynomType( 1, t(j) );

    end

    correct_value = 1.71572875253810;
    soma;
    coef(2) = ( 2 / m ) * soma;


    # Agora calculamos os b(2) : n - 1, no indice 3 : n do array b (coef).
    for k = 2 : n - 1

        soma = 0;

        for j = 1 : m

            soma = soma + targetFunction( x(j) ) * chebyshevPolynomType( k, t(j) );

        end

        coef( k + 1 ) = ( 2 / m ) * soma;

    end

end

# Include the T0, T1, ... Chebyshev's Polynoms of First Kind
source( "ChebyshevPolynomsOfFirstKindList.m" )

