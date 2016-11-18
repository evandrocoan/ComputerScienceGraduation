

more off
format long
split_long_rows(0)

#format rat
#output_precision(30)
#output_max_field_width(0)


#
printf( "\n\n\n##############################################################################################################\n" )
printf( "##############################################################################################################\n" )

printf( "\n3a Prova (18/11/2016): (poste todos os algoritmos e imprima somente as respostas, de forma clara)\n" )
printf( "\n" )
printf( "A função composta\n" )
printf( " f ( x) = ∫ e − z 2 dz\n" )
printf( " em x∈[-1, +1], pode-se aproxima-la de diversas maneiras utilizando apenas\n" )
printf( "\n" )
printf( "operações algébricas simples, como adição, multiplicação e divisão. Uma alternativa de representação é a expansão\n" )
printf( "de f(x) em termos da série de Maclaurin:\n" )
printf( "\n" )
printf( "(3,0) 1). Determine o grau ‘n’ mínimo necessário para que o erro de truncamento máximo ‘estimado’ entre Mn(x) e\n" )
printf( "f(x) seja da ordem de O(10 ) (<(√10).10 ).\n\n" )


clc
clear
close all

profile clear
profile on

n =  2;
a = -1;
b =  1;

erroMinimoDeMaclaurin = sqrt(10)*1e-6;
erroMaximoDeMaclaurin = 1;

printf( "\n" )

while( erroMaximoDeMaclaurin > erroMinimoDeMaclaurin && n < 100 )

    h            = (b-a) / n;
    xInterPontos = a : h/20 : b;

    coefMaclaurinAproximado = calculateMaclaurinCoefficientsForEulerInteger( n );
    coefMaclaurinExato      = calculateMaclaurinCoefficientsForEulerInteger( n^2 );
    
    yAproximado = fPnPorBriotRunifi( n  , coefMaclaurinAproximado, xInterPontos );
    yExato     = fPnPorBriotRunifi( n^2, coefMaclaurinExato     , xInterPontos );

    # Erro máximo deve ser calculado pelas formulas deve ser feito nos limites do nosso
    # intervalo [-1,1], ou seja, em -1 ou em 1.
    #
    # O gráfico do erro mostra que o erro é 0 no ponto 0 (do intervalo [-1,1]), por que foi ali
    # que fizemos a expansão da série de Maclaurin. O contrário da Sério de Chebyshev, que possui
    # um erro mais distribuído ao londo do intervalo (Comparar um Gráfico de Chebyshev e Maclaurin).
    errosDeMaclaurin       = abs( yAproximado .- yExato );
    erroMaximoDeMaclaurin = max( errosDeMaclaurin );

    n = n + 1;

end


printf( "\nO valor de n necessário é:\n" )
n


printf( "\nE o grau da série de Euler necessário é:\n" )
grau = 2*n + 1



% plot( xInterPontos, yExato, 'g', xInterPontos, yAproximado, 'b' )

printf( "\n" );
profile off
% profshow( profile ("info"), 8 )
#


printf( "\n\n\n##############################################################################################################\n" )
printf( "##############################################################################################################\n" )

printf( "(4,0) 2).Uma outra alternativa de representação é a expansão de f(x) em termos da série de Padé Rnm(x). Determine\n" )
printf( "n, m e os coeficientes da aproximação de Padé, a partir de Maclaurin com grau total M=n+m, para que o erro de\n" )
printf( "truncamento máximo ‘estimado’ entre Rnm (x) e f(x) seja da ordem de O(10 ) (<(√10).10 ).\n\n" )

clc
clear
close all

profile clear
profile on

# Numero de pontos do Gráfico e grau n da Série de Pade
n = 2;

# Grau do polinômio dividendo de Pade.
m = 1;

# Domínio
a = -1;
b = 1;
printf( "\n" );

erroMinimoDePade = sqrt(10)*1e-6;
erroMaximoDePade = 1;

printf( "\n" )

while( erroMaximoDePade > erroMinimoDePade && n < 100 )

    h            = (b-a) / n;
    xInterPontos = a : h/20 : b;


    # Calculo de Pade Exato
    grauDeMaclaurinParaPadeExato = n^2 + m^2;
    coefMaclaurinParaPadeExato   = calculateMaclaurinCoefficientsForEulerInteger( grauDeMaclaurinParaPadeExato );
    
    # R32 = (a(1) + a(2)x + a(3)x^2 + a(4)x^3)/(1 + b(1)x + b(2)*x^2)
    #
    [ aPadeCoefficientsExato, bPadeCoefficientsExato ] = calculatePadeCoefficients( n^2, m^2, coefMaclaurinParaPadeExato );
    
    # yAproximado = fPnPorBriotRunifi( n, coefMaclaurin, xInterPontos )
    # yAproximadoPorPade = polyval( aPadeCoefficients, xInterPontos ) / polyval( bPadeCoefficients, xInterPontos );
    #
    yExato = fPnPorBriotRunifi( ...
            n^2, aPadeCoefficientsExato, xInterPontos ) ./ fPnPorBriotRunifi( ...
            m^2, bPadeCoefficientsExato, xInterPontos );
    
    
    # Calculo de Pade Aproximado
    grauDeMaclaurinParaPade = n + m;
    coefMaclaurinParaPade   = calculateMaclaurinCoefficientsForEulerInteger( grauDeMaclaurinParaPade );

    # R32 = (a(1) + a(2)x + a(3)x^2 + a(4)x^3)/(1 + b(1)x + b(2)*x^2)
    #
    [ aPadeCoefficients, bPadeCoefficients ] = calculatePadeCoefficients( n, m, coefMaclaurinParaPade );

    # yAproximado = fPnPorBriotRunifi( n, coefMaclaurin, xInterPontos )
    # yAproximadoPorPade = polyval( aPadeCoefficients, xInterPontos ) / polyval( bPadeCoefficients, xInterPontos );
    #
    yAproximado = fPnPorBriotRunifi( ...
            n, aPadeCoefficients, xInterPontos ) ./ fPnPorBriotRunifi( ...
            m, bPadeCoefficients, xInterPontos );


    # Erro máximo deve ser calculado pelas formulas deve ser feito nos limites do nosso
    # intervalo [-1,1], ou seja, em -1 ou em 1.
    #
    # O gráfico do erro mostra que o erro é 0 no ponto 0 (do intervalo [-1,1]), por que foi ali
    # que fizemos a expansão da série de Maclaurin. O contrário da Sério de Chebyshev, que possui
    # um erro mais distribuído ao londo do intervalo (Comparar um Gráfico de Chebyshev e Maclaurin).
    errosDePade      = abs( yAproximado .- yExato );
    erroMaximoDePade = max( errosDePade );

    n = n + 1;
    m = m + 1;

end


printf( "\nOs graus de n e m de Pade são:\n" )
n
m

printf( "\nOs coeficientes a e b de Pade são:\n" )
aPadeCoefficients
bPadeCoefficients

printf( "\nO erro maximo de Pade para os coeficientes foram:\n" )
erroMaximoDePade


% plot( xInterPontos, yExato, 'g', xInterPontos, yAproximado, 'b' )

printf( "\n" );
profile off
% profshow( profile ("info"), 8 )



printf( "\n\n\n##############################################################################################################\n" )
printf( "##############################################################################################################\n" )

printf( "(4,0) 2).Uma outra alternativa de representação é a expansão de f(x) em termos da série de Padé Rnm(x). Determine\n" )
printf( "n, m e os coeficientes da aproximação de Padé, a partir de Maclaurin com grau total M=n+m, para que o erro de\n" )
printf( "truncamento máximo ‘estimado’ entre Rnm (x) e f(x) seja da ordem de O(10 ) (<(√10).10 ).\n\n" )

clc
clear
close all

profile clear
profile on


plot( xInterPontos, errosDePade, 'g', xInterPontos, errosDeMaclaurin, 'b' )



