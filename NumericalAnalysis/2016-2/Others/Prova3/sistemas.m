

clc
clear
close all

more off
format long
split_long_rows(0)

#format rat
#output_precision(30)
#output_max_field_width(0)

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


profile clear
profile on

n =  2
a = -1
b =  1

erroMinimoDeMaclaurin = sqrt(10)*1e-6
erroMaximoDeMaclaurin = 1;

printf( "\n" )

while( erroMaximoDeMaclaurin > erroMinimoDeMaclaurin && n < 10 )

    h            = (b-a) / n;
    xInterPontos = a : h/20 : b;

    coefMaclaurinAproximado = calculateMaclaurinCoefficientsForEulerInteger( n );
    coefMaclaurinExato      = calculateMaclaurinCoefficientsForEulerInteger( n^2 );
    
    yAproximado = fPnPorBriotRunifi( n  , coefMaclaurinAproximado, xInterPontos );
    yExtato     = fPnPorBriotRunifi( n^2, coefMaclaurinExato     , xInterPontos );

    # Erro máximo deve ser calculado pelas formulas deve ser feito nos limites do nosso
    # intervalo [-1,1], ou seja, em -1 ou em 1.
    #
    # O gráfico do erro mostra que o erro é 0 no ponto 0 (do intervalo [-1,1]), por que foi ali
    # que fizemos a expansão da série de Maclaurin. O contrário da Sério de Chebyshev, que possui
    # um erro mais distribuído ao londo do intervalo (Comparar um Gráfico de Chebyshev e Maclaurin).
    erroDeMaclaurin       = abs( yAproximado .- yExtato );
    erroMaximoDeMaclaurin = max( erroDeMaclaurin )

    n = n + 1;

end

% plot( xInterPontos, yExtato, 'g', xInterPontos, yAproximado, 'b' )

printf( "\n" );
profile off
profshow( profile ("info"), 8 )



printf( "\n\n\n##############################################################################################################\n" )
printf( "##############################################################################################################\n" )

printf( "(4,0) 2).Uma outra alternativa de representação é a expansão de f(x) em termos da série de Padé Rnm(x). Determine\n" )
printf( "n, m e os coeficientes da aproximação de Padé, a partir de Maclaurin com grau total M=n+m, para que o erro de\n" )
printf( "truncamento máximo ‘estimado’ entre Rnm (x) e f(x) seja da ordem de O(10 ) (<(√10).10 ).\n\n" )



profile clear
profile on

n =  2
a = -1
b =  1

erroMinimoDePade = sqrt(10)*1e-6
erroMaximoDeMaclaurin = 1;

printf( "\n" )

while( erroMaximoDeMaclaurin > erroMinimoDePade && n < 10 )

    h            = (b-a) / n;
    xInterPontos = a : h/20 : b;

    coefMaclaurinAproximado = calculateMaclaurinCoefficientsForEulerInteger( n );
    coefMaclaurinExato      = calculateMaclaurinCoefficientsForEulerInteger( n^2 );
    
    yAproximado = fPnPorBriotRunifi( n  , coefMaclaurinAproximado, xInterPontos );
    yExtato     = fPnPorBriotRunifi( n^2, coefMaclaurinExato     , xInterPontos );

    # Erro máximo deve ser calculado pelas formulas deve ser feito nos limites do nosso
    # intervalo [-1,1], ou seja, em -1 ou em 1.
    #
    # O gráfico do erro mostra que o erro é 0 no ponto 0 (do intervalo [-1,1]), por que foi ali
    # que fizemos a expansão da série de Maclaurin. O contrário da Sério de Chebyshev, que possui
    # um erro mais distribuído ao londo do intervalo (Comparar um Gráfico de Chebyshev e Maclaurin).
    erroDeMaclaurin       = abs( yAproximado .- yExtato );
    erroMaximoDeMaclaurin = max( erroDeMaclaurin )

    n = n + 1;

end

% plot( xInterPontos, yExtato, 'g', xInterPontos, yAproximado, 'b' )

printf( "\n" );
profile off
profshow( profile ("info"), 8 )






