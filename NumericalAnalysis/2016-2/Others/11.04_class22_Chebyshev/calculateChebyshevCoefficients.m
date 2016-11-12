
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
# @param chebyshevPolynomType, a function handle to get the nth Chebyshev Polynom.
#
function coef = calculateChebyshevCoefficients( n, m, a, b, targetFunction, chebyshevPolynomType )

    # Calculamos os t(j)
    # Para encontrar os polinômios T1, T2, ..., consulte a tabela
    #
    t( 1 ) = 0;

    # k=1:m;
    # t=cos((2.*k.-1).*pi./(2.*m))
    k = 1 : m;
    t = cos( ( ( 2.*k .- 1 ) / (2*m) ) * pi );

    x           = ChebyshevDomainLinearTransformationOut( t, a, b );
    functionOnX = targetFunction( x );

    # Calculo do b0/coef(1) no indice 1 do array b
    # x^0 = T0
    #
    # Assuming f( x ) = log( x ), in [1, 2]
    # soma( 1  ) = 0.690064519186718
    # soma( 2  ) = 1.35558520936599
    # soma( 3  ) = 1.97268975784344
    # soma( 4  ) = 2.51907280649452
    # soma( 5  ) = 2.97536868263748
    # soma( 6  ) = 3.32728023686338
    # soma( 7  ) = 3.56866028787727
    # soma( 8  ) = 3.70532754177620
    # soma( 9  ) = 3.75839116920959
    # soma( 10 ) = 3.76452812919195
    # soma( 11 ) = 3.76452812919195
    #
    printf( '( calculateChebyshevCoefficients ) Calculating the %dth coefficient by ', 0 ); chebyshevPolynomType
    result = functionOnX.*chebyshevPolynomType( 0, t, true );

    soma      = sum( result );
    coef( 1 ) = ( 1 / m ) * soma;

    # Agora calculamos os b(2) : n - 1, no indice 1 : n do array b/coef.
    #
    for i = 1 : n - 1

        printf( '( calculateChebyshevCoefficients ) Calculating the %dth coefficient by ', i ); chebyshevPolynomType

        # When using n = 10 and m = 10000. This is due the recursive remember feature set to false.
        #
        # Setting this to true causes to force the `getnthChebyshevCoefficientsNumerically` to do:
        # Time(s): 181.631, Time(%): 79.51 and Calls: 620.000
        #
        # Setting this to false cause to force the `getnthChebyshevCoefficientsNumerically` to do:
        # Time(s): 51.683, Time(%): 60.30 and Calls: 220.000
        #
        result = functionOnX.*chebyshevPolynomType( i, t, false );

        soma          = sum( result );
        coef( i + 1 ) = ( 2 / m ) * soma;

    end

end

# Include the T0, T1, ... Chebyshev's Polynoms of First Kind
source( "ChebyshevPolynomsOfFirstKindList.m" )


clc
clear
close all

more off
format long
split_long_rows(0)



profile clear
profile on

# Numero de pontos do Gráfico e grau da Série de Chebyshev/MacLaurin
n = 50;

# Grau de precisão da Integral Numérica, e também o número de nós de Chebyshev
m = 40;

# Domínio
a = 1;
b = 2;

coef_b = calculateChebyshevCoefficients( n, m, a, b, @log, @getChebyshevCoefficientsNumerically )

profile off
profshow( profile ("info"), 8 )




% getChebyshevCoefficientsNumerically( 0, 0.6, true )
% getChebyshevCoefficientsNumerically( 1, 0.6, true )
% getChebyshevCoefficientsNumerically( 2, 0.6, true )
% getChebyshevCoefficientsNumerically( 2, [0.6, 0.4], true )


% value = [ 0.6, 0.4 ];
% T9_correct = T9( value )
% T9_calcula = getChebyshevCoefficientsNumerically( 9, value, true )
% printf( '\n' )












