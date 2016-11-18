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


printf( "\nGrupo 19, exercícios 5 = 1, 6 = 1\n\n" )
printf( "ExemplodeGrupo.m\n" )
printf( "numerogrupo=5\n" )
printf( "if (mod(numerogrupo,6)==0) numeroexercicio5=6 else numeroexercicio5=mod(numerogrupo,6) end\n" )
printf( "if (mod(numerogrupo,2 )==0)  numeroexercicio6= 4 else  numeroexercicio6=mod(numerogrupo,2 ) end\n" )


printf( "\n\n\n##############################################################################################################\n" )
printf( "##############################################################################################################\n" )

printf( "Exercício de fixação:\n" )
printf( "5.1). Na função a seguir está representado o consumo de energia elétrica em um determinado local em\n" )
printf( "função dos anos. Determinar quando (t = ?) o consumo atingirá o limite instalado de 7.5 KW:\n" )
printf( "t(ano)      85 89  93  95  96  ... ?\n" )
printf( "Consumo(KW) 5  5.7 6.2 6.7 7.0 ... 7.5\n" )

profile clear
profile on




profile off
printf( "\n" );
# profshow( profile ("info"), 8 )

#{

printf( "\n\n\n##############################################################################################################\n" )
printf( "##############################################################################################################\n" )

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

profile clear
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


profile off
printf( "\n" );
# profshow( profile ("info"), 8 )



printf( "\n\n\n##############################################################################################################\n" )
printf( "##############################################################################################################\n" )

printf( "\n6.1b). Uma outra alternativa de representação é a expansão de f(x) em termos da\n" )
printf( "série de Maclaurin Mn(x).\n" )
printf( "Determine, ou monte um algoritmo de busca que determine, o grau ‘n’ mínimo\n" )
printf( "necessário e os coeficientes de Mn(x), para que o erro de truncamento máximo\n" )
printf( "‘exato’ entre Mn(x) e f(x) seja da ordem de O(10-2) (<√10*10-2);\n\n" )

profile clear
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

profile off
printf( "\n" );
# profshow( profile ("info"), 8 )



printf( "\n\n\n##############################################################################################################\n" )
printf( "##############################################################################################################\n" )

printf( "\n6.1c). Outra alternativa de representação é a expansão algébrica de f(x) em termos\n" )
printf( "da série de Tchebyschev Tn(x). Determine algebricamente os coeficientes da série\n" )
printf( "de ChebyschevTn(x), para n=3 e 5, e o seu erro máximo ‘exato’ entre Tn(x) e f(x). Os\n" )
printf( "erros máximos normalmente estão nas extremidades do intervalo [a, b], mas para a\n" )
printf( "série de Tchebyschev os erros estão distribuídos no intervalo, então calcule erros em\n" )
printf( "pelo menos 5 pontos de [a, b], e tome o maior destes erros como referência;\n\n" )

profile clear
profile on

# Grau da Série de Chebyshev
n = 5

# Grau de precisão da Integral Numérica, e também o número de nós de Chebyshev
m = 100

# Domínio
a = -1
b = 1

printf( "\nAqui os coeficientes de Chebyshev serao calculados utilizando a funcao generica\n" );
printf( "`getChebyshevCoefficientsByPolinom`para o calculo do nesimo coeficiente de Chebyshev\n" );
printf( "analiticamente pelos polinomios de Chebyshev.\n" );
printf( "A funcao getChebyshevCoefficientsByPolinom, utiliza a funcao generica `getChebyshevCoefficientsPolinom`\n" );
printf( "que calcula o nesimo polinomio de Chebyshev, e entao calcula o polinomio gerado nos pontos\n" );
printf( "necessarios.\n\n" );

printf( "Calculating the Chebyshev Polynom's\n", i );

for i = 0 : n

    chebyshevCoefficientsPolinom( i + 1 ).polynom = getChebyshevCoefficientsPolinom( i );

    printf( "The %d'th Chebyshev Polynom is: ", i );
    polyout( chebyshevCoefficientsPolinom( i + 1 ).polynom, "t" );

end
printf( "\n" );

n = 3
[ errorByPolinom_n3, chebyshevBCoefficients_n3 ] = run_chebyshev_test( a, b, @sin, n, m, @getChebyshevCoefficientsByPolinom );
printf( "\n" );

n = 5
[ errorByPolinom_n5, chebyshevBCoefficients_n5 ] = run_chebyshev_test( a, b, @sin, n, m, @getChebyshevCoefficientsByPolinom );
printf( "\n" );

printf( "\n" );
printf( "A seguir são apresentados os coeficientes de Chebyshev para n = 3 e 5,\n" );
printf( "calculados pelo algoritmo generico que calcula pelo polinomio o nesimo coeficiente\n" );
printf( "de Chebyshev.\n" );

chebyshevBCoefficients_n3
chebyshevBCoefficients_n5

printf( "\n" );
printf( "\n" );
printf( "A seguir são apresentados os erros de Chebyshev para n = 3 e 5.\n" );
printf( "O primeiro valor é o calculado pelo algoritmo generico que calcula pelo polinomio o\n" );
printf( "nesimo coeficiente de Chebyshev.\n" );
printf( "O segundo valor é a resposta que se encontra no livro de Calculo Numerico utilizado\n" );
printf( "para a resolucao deste exercicio.\n" );
printf( "\n" );
errorByPolinom_n3
errorByPolinom_n3 = 5.67503606437536e-04

printf( "\n" );
errorByPolinom_n5
errorByPolinom_n5 = 1.08876303214656e-04

profile off
printf( "\n" );
# profshow( profile ("info"), 8 )



printf( "\n\n\n##############################################################################################################\n" )
printf( "##############################################################################################################\n" )

printf( "\n6.1d). Determine numericamente, através de um algoritmo, os coeficientes da série\n" )
printf( "de Chebyschev Tn(x), para n=3 e 5, e o seus erros máximo exatos entre Tn(x) e f(x);\n\n" )

profile clear
profile on

# Grau da Série de Chebyshev
n = 5

# Grau de precisão da Integral Numérica, e também o número de nós de Chebyshev
m = 100

# Domínio
a = -1
b = 1
printf( "\n" );
printf( "Aqui os coeficientes de Chebyshev serao calculados utilizando a funcao generica\n" );
printf( "`getChebyshevCoefficientsNumerically`para o calculo no nesimo coeficiente de Chebyshev\n" );
printf( "numericamente.\n" );
n = 3
[ errorNumerically_n3, chebyshevBCoefficients_n3, ...
        xInterPontosChebyshev_n3, erroDeChebyshev_n3 ] = run_chebyshev_test( ...
        a, b, @sin, n, m, @getChebyshevCoefficientsNumerically );
printf( "\n" );

n = 5
[ errorNumerically_n5, chebyshevBCoefficients_n5, ...
        xInterPontosChebyshev_n5, erroDeChebyshev_n5 ] = run_chebyshev_test( ...
        a, b, @sin, n, m, @getChebyshevCoefficientsNumerically );

printf( "\n" );
printf( "\n" );
printf( "A seguir são apresentados os erro e os coeficientes de Chebyshev para n = 3 e 5.\n" );
printf( "O primeiro valor é o calculado pelo algoritmo generico que calcula numericamente o\n" );
printf( "nesimo coeficiente de Chebyshev.\n" );
printf( "O segundo valor é a resposta que se encontra no livro de Calculo Numerico utilizado\n" );
printf( "para a resolucao deste exercicio.\n" );

printf( "\n" );
chebyshevBCoefficients_n3
chebyshevBCoefficients_n3 = [ 2.83773005094190e-17 8.80101171489865e-01 3.41948691584548e-18 -3.91267079653375e-02 ]

printf( "\n" );
chebyshevBCoefficients_n5
chebyshevBCoefficients_n5 = [ 2.83773005094190e-17 8.80101171489865e-01 3.41948691584548e-18 -3.91267079653375e-02 ...
                              4.89386309254769e-17 4.99515460422549e-04 ]

printf( "\n" );
errorNumerically_n3
errorNumerically_n3 = 5.01502505832752e-04

printf( "\n" );
errorNumerically_n5
errorNumerically_n5 = 3.01373744460154e-06

profile off
printf( "\n" );
# profshow( profile ("info"), 8 )



printf( "\n\n\n##############################################################################################################\n" )
printf( "##############################################################################################################\n" )

printf( "\n6.1e). Ainda uma outra alternativa de representação é a expansão de f(x) em termos\n" )
printf( "da série racional de Padé Rnm(x). Determine, ou monte um algoritmo que detemine,\n" )
printf( "os coeficientes da aproximação de Padé R32(x), a partir de Maclaurin com grau total\n" )
printf( "M=5 e o seu erro de truncamento máximo ‘exato’ entre Rnm(x) e f(x).\n\n" )

profile clear
profile on

# Numero de pontos do Gráfico e grau n da Série de Pade
n = 3

# Grau do polinômio dividendo de Pade.
m = 2

# Domínio
a = -1
b = 1
printf( "\n" );

[ erroMaximoDePade, aPadeCoefficients, bPadeCoefficients, xInterPontosPade, erroDePade ] = run_pade_test( ...
        n, m, a, b, @sin, @calculateMaclaurinCoefficientsForSin );

aPadeCoefficients
bPadeCoefficients
erroMaximoDePade

profile off
printf( "\n" );
# profshow( profile ("info"), 8 )



printf( "\n\n\n##############################################################################################################\n" )
printf( "##############################################################################################################\n" )

printf( "\n6.1f). Plote os gráficos dos erros exatos entre as aproximações de Tchebyschev e\n" )
printf( "Padé com f(x).\n\n" )

plot( ...
      # xInterPontosChebyshev_n3, erroDeChebyshev_n3, "b;Erro de Chebyshev n = 3;", ...
      xInterPontosChebyshev_n5, erroDeChebyshev_n5, "g;Erro de Chebyshev n = 5;", ...
      xInterPontosPade        , erroDePade        , "r;Erro de Pade;" );
legend('location','north');
grid on;
#}
