

clc
clear
close all

more off
format long
split_long_rows(0)

#format rat
#output_precision(30)
#output_max_field_width(0)

#
printf( "\n\n\n##############################################################################################################\n" )
printf( "##############################################################################################################\n" )


FATOR_MULTIPLICATIVO_DO_VALOR_EXATO = 2

printf( "\n3a Prova (18/11/2016): (poste todos os algoritmos e imprima somente as respostas, de forma clara)\n" )
printf( "\n" )
printf( "A função composta\n" )
printf( " f ( x) = ∫ e − z 2 dz\n" )
printf( " em x∈[-1, +1], pode-se aproxima-la de diversas maneiras utilizando apenas\n" )
printf( "operações algébricas simples, como adição, multiplicação e divisão. Uma alternativa de representação é a expansão\n" )
printf( "de f(x) em termos da série de Maclaurin:\n" )
printf( "\n" )
printf( "(3,0) 1). Determine o grau ‘n’ mínimo necessário para que o erro de truncamento máximo ‘estimado’ entre Mn(x) e\n" )
printf( "f(x) seja da ordem de O(10 ) (<(√10).10 ).\n\n" )


# clc
# clear
# close all

profile clear
profile on


n = 1;

a = -1;
b = 1;
printf( "\n" )

erroMinimoDeMaclaurin = sqrt(10)*1e-6;
erroMaximoDeMaclaurin = 1;

printf( "\n" )

while( erroMaximoDeMaclaurin > erroMinimoDeMaclaurin && n < 100 )

    n = n + 1;
    h = (b-a) / n;

    # x                     = a : h : b;
    xInterPontosMaclaurin = a : h/20 : b;

    coefMaclaurinAproximado = calculateMaclaurinCoefficientsForEulerInteger( n );
    coefMaclaurinExato      = calculateMaclaurinCoefficientsForEulerInteger( n*FATOR_MULTIPLICATIVO_DO_VALOR_EXATO );

    yAproximadoMaclaurin = fPnPorHorner( n, coefMaclaurinAproximado, xInterPontosMaclaurin );
    yExatoMaclaurin      = fPnPorHorner( n*FATOR_MULTIPLICATIVO_DO_VALOR_EXATO, coefMaclaurinExato, xInterPontosMaclaurin );

    # Erro máximo deve ser calculado pelas formulas deve ser feito nos limites do nosso
    # intervalo [-1,1], ou seja, em -1 ou em 1.
    #
    # O gráfico do erro mostra que o erro é 0 no ponto 0 (do intervalo [-1,1]), por que foi ali
    # que fizemos a expansão da série de Maclaurin. O contrário da Sério de Chebyshev, que possui
    # um erro mais distribuído ao londo do intervalo (Comparar um Gráfico de Chebyshev e Maclaurin).
    errosDeMaclaurin      = abs( yAproximadoMaclaurin .- yExatoMaclaurin );
    erroMaximoDeMaclaurin = max( errosDeMaclaurin )

end


printf( "\nO valor de n mínimo necessário é:\n" )
n


printf( "\nE o grau da série de Euler necessário/equivalente para aquele n mínimo é:\n" )
grau = 2*n + 1



# plot( xInterPontosMaclaurin, yExatoMaclaurin, 'g', xInterPontosMaclaurin, yAproximadoMaclaurin, 'b' )
# plot( ...
#       xInterPontosMaclaurin, errosDeMaclaurin, "b;Erro de Maclaurin;" ...
#     );
# legend('location','north');
# grid on;

printf( "\n" );
profile off
# profshow( profile ("info"), 8 )
#


printf( "\n\n\n##############################################################################################################\n" )
printf( "##############################################################################################################\n" )

printf( "(4,0) 2).Uma outra alternativa de representação é a expansão de f(x) em termos da série de Padé Rnm(x). Determine\n" )
printf( "n, m e os coeficientes da aproximação de Padé, a partir de Maclaurin com grau total M=n+m, para que o erro de\n" )
printf( "truncamento máximo ‘estimado’ entre Rnm (x) e f(x) seja da ordem de O(10 ) (<(√10).10 ).\n\n" )

# clc
# clear
# close all

profile clear
profile on

# Numero de pontos do Gráfico e grau n da Série de Pade
n = 0;

# Grau do polinômio dividendo de Pade.
m = 0;

# Domínio
a = -1;
b = 1;
printf( "\n" );

erroMinimoDePade = sqrt(10)*1e-6;
erroMaximoDePade = 1;

printf( "\n" )

while( erroMaximoDePade > erroMinimoDePade && n < 10 )

    n = n + 1;
    m = m + 1;

    h                = (b-a) / n;
    xInterPontosPade = a : h/20 : b;


    # Calculo de Pade Exato
    grauDeMaclaurinParaPadeExato = n*FATOR_MULTIPLICATIVO_DO_VALOR_EXATO + m*FATOR_MULTIPLICATIVO_DO_VALOR_EXATO;
    coefMaclaurinParaPadeExato   = calculateMaclaurinCoefficientsForEulerInteger( grauDeMaclaurinParaPadeExato );

    # R32 = (a(1) + a(2)x + a(3)x^2 + a(4)x^3)/(1 + b(1)x + b(2)*x^2)
    #
    [ aPadeCoefficientsExato, bPadeCoefficientsExato ] = calculatePadeCoefficients( ...
            n*FATOR_MULTIPLICATIVO_DO_VALOR_EXATO, m*FATOR_MULTIPLICATIVO_DO_VALOR_EXATO, coefMaclaurinParaPadeExato );

    # yAproximado = fPnPorHorner( n, coefMaclaurin, xInterPontosPade )
    # yAproximadoPorPade = polyval( aPadeCoefficientsExato, xInterPontosPade ) / polyval( bPadeCoefficientsExato, xInterPontosPade );
    #
    yExatoPade = fPnPorHorner( ...
            n*FATOR_MULTIPLICATIVO_DO_VALOR_EXATO, aPadeCoefficientsExato, xInterPontosPade ) ./ fPnPorHorner( ...
            m*FATOR_MULTIPLICATIVO_DO_VALOR_EXATO, bPadeCoefficientsExato, xInterPontosPade );


    # Calculo de Pade Aproximado
    grauDeMaclaurinParaPadeAproximado = n + m;
    coefMaclaurinParaPadeAproximado   = calculateMaclaurinCoefficientsForEulerInteger( grauDeMaclaurinParaPadeAproximado );

    # R32 = (a(1) + a(2)x + a(3)x^2 + a(4)x^3)/(1 + b(1)x + b(2)*x^2)
    #
    [ aPadeCoefficientsAproximado, bPadeCoefficients ] = calculatePadeCoefficients( n, m, coefMaclaurinParaPadeAproximado );

    # yAproximadoPade = fPnPorHorner( n, coefMaclaurin, xInterPontosPade )
    # yAproximadoPorPade = polyval( aPadeCoefficientsAproximado, xInterPontosPade ) / polyval( bPadeCoefficients, xInterPontosPade );
    #
    yAproximadoPade = fPnPorHorner( ...
            n, aPadeCoefficientsAproximado, xInterPontosPade ) ./ fPnPorHorner( ...
            m, bPadeCoefficients, xInterPontosPade );


    # Erro máximo deve ser calculado pelas formulas deve ser feito nos limites do nosso
    # intervalo [-1,1], ou seja, em -1 ou em 1.
    #
    # O gráfico do erro mostra que o erro é 0 no ponto 0 (do intervalo [-1,1]), por que foi ali
    # que fizemos a expansão da série de Maclaurin. O contrário da Sério de Chebyshev, que possui
    # um erro mais distribuído ao londo do intervalo (Comparar um Gráfico de Chebyshev e Maclaurin).
    errosDePade      = abs( yAproximadoPade .- yExatoPade );
    erroMaximoDePade = max( errosDePade )

end


printf( "\nOs graus de n e m de Pade são:\n" )
n
m

printf( "\nOs coeficientes a e b de Pade são:\n" )
aPadeCoefficientsAproximado
bPadeCoefficients

printf( "\nO erro maximo de Pade para estes coeficientes foi:\n" )
erroMaximoDePade


# plot( xInterPontosPade, yExatoPade, 'g', xInterPontosPade, yAproximadoPade, 'b' )
# plot( ...
#       xInterPontosPade, errosDePade, "g;Erro de Pade;" ...
#     );
# legend('location','north');
# grid on;

printf( "\n" );
profile off
# profshow( profile ("info"), 8 )
#


printf( "\n\n\n##############################################################################################################\n" )
printf( "##############################################################################################################\n" )

printf( "0) 4). Determine e Plote os erros máximos, em cada caso.\n\n" )

# clc
# clear
# close all

profile clear
profile on



printf( "\nO valor e o ponto de erro máximo do Maclaurin são:\n" )
[ valorDoErroMaximoDeMaclaurin_______, indexDoPontoDeErroMaximoDeMaclaurin ] = max( errosDeMaclaurin );

valorDoErroMaximoDeMaclaurin_______
indexDoPontoDeErroMaximoDeMaclaurin
valorDoPontoErroMaximoDeMaclaurin__ = xInterPontosMaclaurin( indexDoPontoDeErroMaximoDeMaclaurin )


printf( "\nO valor e o ponto de erro máximo do Pade são:\n" )
[ valorDoErroMaximoDePade_______, indexDoPontoDeErroMaximoDePade ] = max( errosDePade );

valorDoErroMaximoDePade_______
indexDoPontoDeErroMaximoDePade
valorDoPontoErroMaximoDePade__ = xInterPontosPade( indexDoPontoDeErroMaximoDePade )


plot( ...
      xInterPontosPade     , errosDePade     , "g;Erro de Chebyshev;", ...
      xInterPontosMaclaurin, errosDeMaclaurin, "b;Erro de Maclaurin;" ...
    );
legend('location','north');
grid on;



printf( "\n" );
profile off
# profshow( profile ("info"), 8 )
#}



#
# No ponto 1, O valor exato de `fPnPorHorner` deve ser:
# 0.7468241328124270253994674361318530053544996868126063290276544989586053275617728314978484298229019197
#
# https://www.wolframalpha.com/input/?i=integrate+e%5E(-x%5E2)+dx+from+0+to+1
#
# No e no ponto 0.5:
# 0.4612810064127924603027963712520431727170944213867187500000000000000000000000000000000000000000000000

# n                                = 10
# xInterPontosMaclaurin            = 0.5
# coefMaclaurinParaHorner          = calculateMaclaurinCoefficientsForEulerInteger( n )
# coefMaclaurinCorretosPara0ponto5 = [ 5.00000000000000e-01   -4.16666666666667e-02   3.12500000000000e-03 ...
#                                     -1.86011904761905e-04    9.04224537037037e-06  -3.69910037878788e-07 ...
#                                      1.30417000534188e-08   -4.03671668320106e-10   1.11306526191206e-11 ...
#                                     -2.76639027083406e-13    6.25731132688656e-15 ]

# yMeu___ParaX_InterPontosMaclaurin = polyval( fliplr( coefMaclaurinParaHorner ), xInterPontosMaclaurin )
# yMeu___ParaX_InterPontosMaclaurin = fPnPorHorner( n, coefMaclaurinParaHorner, xInterPontosMaclaurin )
# yOctaveParaX_InterPontosMaclaurin = sqrt( pi )*erf( xInterPontosMaclaurin ) / 2


