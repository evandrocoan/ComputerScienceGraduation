

clc
clear
close all

more off
format long
split_long_rows(0)

#format rat
#output_precision(30)
#output_max_field_width(0)


printf( "\nUse a função f(x)=(sqrt(pi)/2)*erf(x) (aproximação para 'double' do Octave), que é a\n" )
printf( "mesma da  Prova 3 do dia 18.11.2016 e refaça as questões a seguir:\n\n" )

FATOR_MULTIPLICATIVO_DO_VALOR_ESTIMADO = 2



#
printf( "\n\n\n##############################################################################################################\n" )
printf( "##############################################################################################################\n" )

printf( "\n1. Dada a f(x) calcule os coeficientes de um polinômio interpolador de grau 11, \n" )
printf( "o erro máximo estimado e o erro exato;\n\n" )

# clc
# clear

n = 11

a = -1
b =  1



hEstimado = ( b - a ) / n;

xPontosEstimado = a : hEstimado : b;
yExatoEstimado  = erfFunction( xPontosEstimado );


hExato = ( b - a ) / ( n*FATOR_MULTIPLICATIVO_DO_VALOR_ESTIMADO ) ;

xPontosExato = a : hExato : b;
yExatoExato  = erfFunction( xPontosExato );


# coef_by_me =
#   -1.164279323185479   1.399845394498246  -0.235566071312767
#
# -1.164279323185479 + 1.399845394498246*x^1 - 0.235566071312767*x^2
#
coef_by_meAproximado = interpolacaoPolinomial( xPontosEstimado, yExatoEstimado, n );
coef_by_meEstimado   = interpolacaoPolinomial( xPontosExato   , yExatoExato   , n * FATOR_MULTIPLICATIVO_DO_VALOR_ESTIMADO );



xInterPontos = a : hEstimado/20 : b;

# Aqui calculamos o valor do polinômio nos pontos xInterPontos, utilizando o método de Horner e de Briot Rufini.
# Isso por que custa muito caro efetuar as operações de potência ao calcular o polinômio:
# a(1) + a(2)*x^1 + a(3)*x(^2)+...+a(n+1)*x^n
#
yAproximadoInterPontos = fPnPorHorner( n                                         , coef_by_meAproximado, xInterPontos );
yEstimadoInterPontos   = fPnPorHorner( n * FATOR_MULTIPLICATIVO_DO_VALOR_ESTIMADO, coef_by_meEstimado  , xInterPontos );


# Erros estimados
errosEstimadoInterpolador      = abs( yAproximadoInterPontos .- yEstimadoInterPontos );
erroMaximoEstimadoInterpolador = max( errosEstimadoInterpolador );


# Erros exatos
yExatoInterPontos = erfFunction( xInterPontos );

errosExatoDeInterpolador       = abs( yAproximadoInterPontos .- yExatoInterPontos );
erroMaximoExatoInterpolador___ = max( errosExatoDeInterpolador );


erroMaximoExatoInterpolador___
erroMaximoEstimadoInterpolador
coef_by_meAproximado





# plot( x, y, '*', xInterPontos, yAproximado, "g;Funcao Aproximadora do Consumo pelo interpolacaoPolinomial;" )
# legend('location','north');
# grid on;
#


#
printf( "\n\n\n##############################################################################################################\n" )
printf( "##############################################################################################################\n" )

printf( "\n2. Dada a f(x) calcule os coeficientes de uma série de Maclaurin de grau 11, \n" )
printf( " o erro máximo estimado e o erro exato;\n\n" )

# clc
# clear

# grau = 2*n + 1

a = -1
b =  1

grau = 11
n = fix( ( grau - 1 ) / 2 )

h = (b-a) / n;

# x                     = a : h : b;
xInterPontosMaclaurin = a : h/20 : b;

coefMaclaurinAproximado = calculateMaclaurinCoefficientsForEulerInteger( grau );
coefMaclaurinExato      = calculateMaclaurinCoefficientsForEulerInteger( grau*FATOR_MULTIPLICATIVO_DO_VALOR_ESTIMADO );

yAproximadoMaclaurin = fPnPorHorner( n, coefMaclaurinAproximado, xInterPontosMaclaurin );
yEstimadoMaclaurin   = fPnPorHorner( n*FATOR_MULTIPLICATIVO_DO_VALOR_ESTIMADO, coefMaclaurinExato, xInterPontosMaclaurin );


# Erro máximo deve ser calculado pelas formulas deve ser feito nos limites do nosso
# intervalo [-1,1], ou seja, em -1 ou em 1.
#
# O gráfico do erro mostra que o erro é 0 no ponto 0 (do intervalo [-1,1]), por que foi ali
# que fizemos a expansão da série de Maclaurin. O contrário da Sério de Chebyshev, que possui
# um erro mais distribuído ao londo do intervalo (Comparar um Gráfico de Chebyshev e Maclaurin).
errosEstimadosDeMaclaurin   = abs( yAproximadoMaclaurin .- yEstimadoMaclaurin );
erroMaximoEstimadoMaclaurin = max( errosEstimadosDeMaclaurin );


yExato = erfFunction( xInterPontosMaclaurin );

errosExatoDeMaclaurin       = abs( yAproximadoMaclaurin .- yExato );
erroMaximoExatoMaclaurin___ = max( errosExatoDeMaclaurin );


erroMaximoExatoMaclaurin___
erroMaximoEstimadoMaclaurin
coefMaclaurinAproximado



#
printf( "\n\n\n##############################################################################################################\n" )
printf( "##############################################################################################################\n" )

printf( "\n3. Dada a f(x) calcule os coeficientes de uma série de Tchebychev de grau 11, \n" )
printf( "o erro máximo estimado e o erro máximo exato; \n\n" )

# clc
# clear

a = -1
b =  1

n = 11
m = 100


# Gráfico de Chebyshev
h = (b-a)/n;

x = a : h : b;
y = erfFunction( x );

xInterPontosChebyshev = a : h/20 : b;
tInterPontos          = ChebyshevDomainLinearTransformationIn( xInterPontosChebyshev, a, b );


chebyshevAproximadoCoefficients = calculateChebyshevCoefficients( n, m, a, b, @erfFunction, @getChebyshevCoefficientsByPolinom );
chebyshevEstimadoCoefficients   = calculateChebyshevCoefficients( ...
        n*FATOR_MULTIPLICATIVO_DO_VALOR_ESTIMADO, m, a, b, @erfFunction, @getChebyshevCoefficientsByPolinom );


# For log( x ) in [1, 2]
correct_b = [  3.76452812919196e-001,  3.43145750507620e-001, -2.94372515228575e-002, ...
               3.36708925555306e-003, -4.33275888539416e-004,  5.94707115514397e-005, ...
              -8.50296480504678e-006,  1.25045018720205e-006, -1.87619547238052e-007, ...
               2.79406818857846e-008,  1.71766583014649e-014, -2.79406536352273e-008 ];

#
# printf( '\n\n\n\n\n( run_chebyshev_test ) Calling the evaluateChebyshevPolynom.\n' );

# i = 1 : n
# fChebyshev( i ) = b0*T0( t(i) ) + b1*T1( t(i) ) + b2*T2( t(i) ) + b3*T3( t(i) ) + ...
#
# yAproximado = coef_b( 1)*T0( tInterPontos ) ...
#             + coef_b( 2)*T1( tInterPontos ) ...
#             + coef_b( 3)*T2( tInterPontos ) ...
#             + coef_b( 4)*T3( tInterPontos );
#
yAproximado = evaluateChebyshevPolynom( n, chebyshevAproximadoCoefficients, tInterPontos, @getChebyshevCoefficientsByPolinom );
yEstimado   = evaluateChebyshevPolynom( ...
        n*FATOR_MULTIPLICATIVO_DO_VALOR_ESTIMADO, chebyshevEstimadoCoefficients  , tInterPontos, @getChebyshevCoefficientsByPolinom );


# Erro máximo deve ser calculado pelas formulas deve ser feito nos limites do nosso
# intervalo [-1,1], ou seja, em -1 ou em 1.
#
# O gráfico do erro mostra que o erro é 0 no ponto 0 (do intervalo [-1,1]), por que foi ali
# que fizemos a expansão da série de Maclaurin. O contrário da Sério de Chebyshev, que possui
# um erro mais distribuído ao londo do intervalo (Comparar um Gráfico de Chebyshev e Maclaurin).
#
erroEstimadoDeChebyshev       = abs( yAproximado .- yEstimado );
erroMaximoEstimadoDeChebyshev = max( erroEstimadoDeChebyshev );


yExato = erfFunction( xInterPontosChebyshev );

erroExatoDeChebyshev          = abs( yAproximado .- yExato );
erroMaximoExatoDeChebyshev___ = max( erroExatoDeChebyshev );


# plot( x, y, '*' )
# plot( x, y, '*', xInterPontosChebyshev, yInterPontos, 'g', xInterPontosChebyshev, yAproximado, 'b' );

printf( "\n" );


erroMaximoExatoDeChebyshev___
erroMaximoEstimadoDeChebyshev
chebyshevAproximadoCoefficients



#
printf( "\n\n\n##############################################################################################################\n" )
printf( "##############################################################################################################\n" )

printf( "\n4. Dados os coeficientes numéricos da serie de Maclaurin de grau 21 para f(x):\n" )
printf( "c=[  ...  ]\n" )
printf( ", calcule uma série racional de Padé R(6,5), o erro máximo estimado (comparando Padé R6,5 \n" )
printf( " com Padé R11,10) e o erro exato.\n\n" )

# clc
# clear

# Numero de pontos do Gráfico e grau n da Série de Pade
n = 6;

# Grau do polinômio dividendo de Pade.
m = 6;

# Domínio
a = -1;
b =  1;

# printf( "\n" );
# printf( "\n" )
h                = (b-a) / n;
xInterPontosPade = a : h/20 : b;



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



# Calculo de Pade Estimado
grauDeMaclaurinParaPadeEstimado = n*FATOR_MULTIPLICATIVO_DO_VALOR_ESTIMADO + m*FATOR_MULTIPLICATIVO_DO_VALOR_ESTIMADO;
coefMaclaurinParaPadeEstimado   = calculateMaclaurinCoefficientsForEulerInteger( grauDeMaclaurinParaPadeEstimado );

# R32 = (a(1) + a(2)x + a(3)x^2 + a(4)x^3)/(1 + b(1)x + b(2)*x^2)
#
[ aPadeCoefficientsEstimado, bPadeCoefficientsEstimado ] = calculatePadeCoefficients( ...
        n*FATOR_MULTIPLICATIVO_DO_VALOR_ESTIMADO, m*FATOR_MULTIPLICATIVO_DO_VALOR_ESTIMADO, coefMaclaurinParaPadeEstimado );

# yAproximado = fPnPorHorner( n, coefMaclaurin, xInterPontosPade )
# yAproximadoPorPade = polyval( aPadeCoefficientsEstimado, xInterPontosPade ) / polyval( bPadeCoefficientsEstimado, xInterPontosPade );
#
yEstimadoPade = fPnPorHorner( ...
        n*FATOR_MULTIPLICATIVO_DO_VALOR_ESTIMADO, aPadeCoefficientsEstimado, xInterPontosPade ) ./ fPnPorHorner( ...
        m*FATOR_MULTIPLICATIVO_DO_VALOR_ESTIMADO, bPadeCoefficientsEstimado, xInterPontosPade );



# Erro máximo deve ser calculado pelas formulas deve ser feito nos limites do nosso
# intervalo [-1,1], ou seja, em -1 ou em 1.
#
# O gráfico do erro mostra que o erro é 0 no ponto 0 (do intervalo [-1,1]), por que foi ali
# que fizemos a expansão da série de Maclaurin. O contrário da Sério de Chebyshev, que possui
# um erro mais distribuído ao londo do intervalo (Comparar um Gráfico de Chebyshev e Maclaurin).
errosEstimadoDePade      = abs( yAproximadoPade .- yEstimadoPade );
erroEstimadoMaximoDePade = max( errosEstimadoDePade );


yExato = erfFunction( xInterPontosPade );

erroExatoDePade          = abs( yAproximadoPade .- yExato );
erroMaximoExatoDePade___ = max( erroExatoDePade );


printf( "Os graus de n e m de Pade são:\n" )
n
m

printf( "\nO erro maximo de Pade para estes coeficientes foi:\n" )

erroMaximoExatoDePade___
erroEstimadoMaximoDePade

printf( "\nOs coeficientes a e b de Pade são:\n" )
aPadeCoefficientsAproximado
bPadeCoefficients


# plot( xInterPontosPade, yEstimadoPade, 'g', xInterPontosPade, yAproximadoPade, 'b' )
# plot( ...
#       xInterPontosPade, errosEstimadoDePade, "g;Erro de Pade;" ...
#     );
# legend('location','north');
# grid on;



#
printf( "\n\n\n##############################################################################################################\n" )
printf( "##############################################################################################################\n" )

printf( "\n\nImprima, para cada aproximaçao, “somente” os:\n" )
printf( "- nome do metodo\n" )
printf( "- grau \n" )
printf( "- coeficientes;\n" )
printf( "- erro máximo estimado e\n" )
printf( "- exato atingido em cada caso;\n" )
printf( "Qual das 4 aproximações foi a mais precisa?\n\n" )


printf( "\nClaramente como podemos ver a seguir, a melhor aproximação foi a de Chebyshev.\n" )
printf( "E também vemos que o interpolador polinomial em segundo lugar.\n" )


printf( "\nInterpolador\n" )
erroMaximoExatoInterpolador___
erroMaximoEstimadoInterpolador

printf( "\nMaclaurin\n" )
erroMaximoExatoMaclaurin___
erroMaximoEstimadoMaclaurin

printf( "\nChebyshev\n" )
erroMaximoExatoDeChebyshev___
erroMaximoEstimadoDeChebyshev

printf( "\nPade\n" )
erroMaximoExatoDePade___
erroEstimadoMaximoDePade



plot( ...
      xInterPontosPade       , errosEstimadoDePade       , "g;Erro de Pade;", ...
      erroEstimadoDeChebyshev, erroEstimadoDeChebyshev   , "g;Erro de Pade;", ...
      xInterPontosMaclaurin  , errosEstimadosDeMaclaurin , "b;Erro de Maclaurin;" ...
    );
legend('location','north');
grid on;




#}
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




