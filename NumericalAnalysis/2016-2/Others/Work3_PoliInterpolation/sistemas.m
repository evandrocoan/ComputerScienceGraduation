#{
Resolver em computador os exercicios:

1). Exercício 5.numeroexerciciocap5

2). Exercício 6.numeroexerciciocap6

---------------------------------------------------------------------------------------------------------------------------------------

ExemplodeGrupo.m

%Por Exemplo: Se

numerogrupo=5

if (mod(numerogrupo,6)==0) numeroexerciciocap5=6 else numeroexerciciocap5=mod(numerogrupo,6) end

if (mod(numerogrupo,2 )==0)  numeroexerciciocap6=4 else  numeroexerciciocap6=mod(numerogrupo,2) end

%-----------------------------------------------------------------------------------------------------------------------------------------

numeroexerciciocap5= 5
numeroexerciciocap6= 1

#}


clc
clear
close all

more off
format long
split_long_rows(0)

#format rat
#output_precision(30)
#output_max_field_width(0)

# addpath( 'Maclaurin' )



##############################################################################################################
##############################################################################################################


#{
printf( "\n\n\nGrupo 19, exercícios 5 = 1, 6 = 1\n\n" )
printf( "ExemplodeGrupo.m\n" )
printf( "numerogrupo=5\n" )
printf( "if (mod(numerogrupo,6)==0) numeroexercicio5=6 else numeroexercicio5=mod(numerogrupo,6) end\n" )
printf( "if (mod(numerogrupo,2 )==0)  numeroexercicio6= 4 else  numeroexercicio6=mod(numerogrupo,2 ) end\n" )
printf( "\n" )
printf( "6.1). Pode-se avaliar uma função como f(x)= sen(x), em xÎ[-1, +1] (radianos),\n" )
printf( "utilizando apenas operações algébricas simples, como adição/subtração,\n" )
printf( "multiplicação e divisão.\n" )
printf( "\n" )
printf( "6.1a). Uma alternativa de representação é o interpolador polinomial Pn(x).\n" )
printf( "Determine, ou monte um algoritmo de busca que determine, o grau ‘n’ mínimo\n" )
printf( "necessário e os coeficientes de Pn(x), para que o erro de truncamento máximo\n" )
printf( "‘exato’ entre Pn(x) e f(x) seja da ordem de O(10-2) (<√10*10-2).\n" )
printf( "Sugestão: Monte um algoritmo de busca que incremente sequencialmente o valor de\n" )
printf( "‘n’, enquanto o erro de truncamento máximo exato esteja maior que √10*10-2.\n\n" )

# Profiling
#
# Command: profile on
# Command: profile off
# Command: profile resume
# Command: profile clear
# Function File: S = profile ("status")
# Function File: T = profile ("info")
#
# https://www.gnu.org/software/octave/doc/v4.0.1/Profiling.html
profile on

passos         = 0;
tolerancia     = sqrt(10)*1e-2;
erroMaximoDePn = 1;

a = -1;
b =  1;
n =  0;

while erroMaximoDePn > tolerancia && n < 20

    n = n + 1;
    h = ( b - a ) / n;
    x = a : h : b;
    y = sin( x );

    # coef_by_me =
    #   -1.164279323185479   1.399845394498246  -0.235566071312767
    #
    # -1.164279323185479 + 1.399845394498246*x^1 - 0.235566071312767*x^2
    coef_by_me      = interpolacaoPolinomial( x, y, n );
    coef_by_polyfit = fliplr( polyfit( x, y, n ) );

    xInterPontos = a : h / 20 : b;
    yInterPontos = sin( xInterPontos );

    # Aqui calculamos o valor do polinômio nos pontos xInterPontos, utilizando o método de Horner e de Briot Rufini.
    # Isso por que custa muito caro efetuar as operações de potência ao calcular o polinômio:
    # a(1) + a(2)*x^1 + a(3)*x(^2)+...+a(n+1)*x^n
    #
    #yAproximado = fPnPorHorner( n, coef_by_me, x );
    yAproximado = fPnPorBriotRunifi( n, coef_by_me, xInterPontos );

    erroDePn       = abs( yAproximado .- yInterPontos );
    erroMaximoDePn = max( erroDePn );

end

coef_by_me
coef_by_polyfit

n
tolerancia
erroMaximoDePn

# plot( x, y, '*', xInterPontos, yAproximado, 'g', xInterPontos, yInterPontos, 'b' )
% plot( xInterPontos, erroDePn )


# Stop profiling. The collected data can later be retrieved and examined.
profile off

# Show the profile resume, displaying per-function profiler results.
#
# profshow (data, n)
# If data is unspecified, profshow will use the current profile dataset.
# If n is unspecified it defaults to 20.
profshow( profile ("info"), 8 )



##############################################################################################################
##############################################################################################################


printf( "\n\n6.1b). Uma outra alternativa de representação é a expansão de f(x) em termos da\n" )
printf( "série de Maclaurin Mn(x).\n" )
printf( "Determine, ou monte um algoritmo de busca que determine, o grau ‘n’ mínimo\n" )
printf( "necessário e os coeficientes de Mn(x), para que o erro de truncamento máximo\n" )
printf( "‘exato’ entre Mn(x) e f(x) seja da ordem de O(10-2) (<√10*10-2);\n\n" )

# Método de MacLaurin
#
# Derivatives
# ( u^n )' = n*u^(n-1)*u'   <-- Chain rule, the external derivative times the internal derivative.
#
# To create the MacLaurin coefficients we need to derivate several times aways applying them on the
# 0 point, because that is the MacLaurin Series, the Taylor series applied on the zero point.
#
# 1. Always to transform the domain from [a, b] to [-1, +1] using this formula:
#    x(t) = 0.5*( b-a )*t + 0.5*( b+a )
#
# Now we got f( x(t) ) with t on [-1, 1]. This is useful/necessary to the Chebyshev Series.
# And we always to apply the derivatives on this new domain [-1, +1] at the point 0, to deduce
# the nth derivate formula at the point 0 by backtracking.
# Como vamos padronizar o domínio [a, b] da aproximado para [-1, 1], pode-se fixar o x da série em 0.
#
function erroMaximoDeMaclaurin = run_maclarin_test( n, a, b, targetFunction )

    h = (b-a) / n;
    x = a : h : b;
    y = targetFunction( x );

    xInterPontos = a : h/20 : b;
    yInterPontos = targetFunction( xInterPontos );

    coefMaclaurin = calculateMaclaurinCoefficientsForSin( n, a, b );

    tInterPontos = ChebyshevDomainLinearTransformationIn( xInterPontos, a, b );
    yAproximado  = fPnPorBriotRunifi( n, coefMaclaurin, tInterPontos );

    # Erro máximo deve ser calculado pelas formulas deve ser feito nos limites do nosso
    # intervalo [-1,1], ou seja, em -1 ou em 1.
    #
    # O gráfico do erro mostra que o erro é 0 no ponto 0 (do intervalo [-1,1]), por que foi ali
    # que fizemos a expansão da série de Maclaurin. O contrário da Sério de Chebyshev, que possui
    # um erro mais distribuído ao londo do intervalo (Comparar um Gráfico de Chebyshev e Maclaurin).
    erroDeMaclaurin       = abs( yAproximado .- yInterPontos );
    erroMaximoDeMaclaurin = max( erroDeMaclaurin );

    # plot( x, y, '*' )
    # plot( x, y, '*', xInterPontos, yInterPontos, 'g', xInterPontos, yAproximado, 'b' )

end

# Profiling
#
# Command: profile on
# Command: profile off
# Command: profile resume
# Command: profile clear
# Function File: S = profile ("status")
# Function File: T = profile ("info")
#
# https://www.gnu.org/software/octave/doc/v4.0.1/Profiling.html
profile on

# Numero de pontos do Gráfico e grau n da Série de MacLaurin
#
n = 1

# Domínio
a = -1
b = 1

erroMinimoDeMaclaurin = sqrt(10)*1e-2
erroMaximoDeMaclaurin = 1;

while( erroMaximoDeMaclaurin > erroMinimoDeMaclaurin && n < 100 )

    erroMaximoDeMaclaurin = run_maclarin_test( n, a, b, @sin )
    n = n + 1

end

# Stop profiling. The collected data can later be retrieved and examined.
profile off

# Show the profile resume, displaying per-function profiler results.
#
# profshow (data, n)
# If data is unspecified, profshow will use the current profile dataset.
# If n is unspecified it defaults to 20.
profshow( profile ("info"), 8 )
#}


##############################################################################################################
##############################################################################################################

printf( "\n\n6.1c). Outra alternativa de representação é a expansão algébrica de f(x) em termos\n" )
printf( "da série de Tchebyschev Tn(x). Determine algébricamente os coeficientes da série\n" )
printf( "de ChebyschevTn(x), para n=3 e 5, e o seu erro máximo ‘exato’ entre Tn(x) e f(x). Os\n" )
printf( "erros máximos normalmente estão nas extremidades do intervalo [a, b], mas para a\n" )
printf( "série de Tchebyschev os erros estão distribuidos no intervalo, então calcule erros em\n" )
printf( "pelo menos 5 pontos de [a, b], e tome o maior destes erros como referência;\n\n" )



















