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


format long
split_long_rows(0)
#format rat
#output_precision(30)
#output_max_field_width(0)

#addpath( 'polynoms' )

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


function x = f( x )
    x = sin( x );
end

passos         = 0;
tolerancia     = sqrt(10)*1e-2
erroMaximoDePn = 1;

a = -1;
b =  1;
n =  0;

while erroMaximoDePn > tolerancia && passos < 20

    n = n + 1;
    h = ( b - a ) / n;
    x = a : h : b;
    y = f( x );

    passos = passos + 1;

    # coef_by_me =
    #   -1.164279323185479   1.399845394498246  -0.235566071312767
    #
    # -1.164279323185479 + 1.399845394498246*x^1 - 0.235566071312767*x^2
    coef_by_me      = interpolacaoPolinomial( x, y, n );
    coef_by_polyfit = fliplr( polyfit( x, y, n ) );

    xInterPontos = a : h / 20 : b;
    yInterPontos = f( xInterPontos );

    # Aqui calculamos o valor do polinômio nos pontos xInterPontos, utilizando o método de Horner e de Briot Rufini.
    # Isso por que custa muito caro efetuar as operações de potência ao calcular o polinômio:
    # a(1) + a(2)*x^1 + a(3)*x(^2)+...+a(n+1)*x^n
    #
    #yAproximado = fPnPorHorner( n, coef_by_me, x );
    yAproximado = fPnPorBriotRunifi( n, coef_by_me, xInterPontos );

    erroDePn       = abs( yAproximado .- yInterPontos );
    erroMaximoDePn = max( erroDePn );

end

n
passos;
erroDePn;

coef_by_me
coef_by_polyfit
erroMaximoDePn

plot( x, y, '*', xInterPontos, yAproximado, 'g', xInterPontos, yInterPontos, 'b' )
% plot( xInterPontos, erroDePn )

#}

##############################################################################################################
##############################################################################################################


printf( "\n\n6.1b). Uma outra alternativa de representação é a expansão de f(x) em termos da\n" )
printf( "série de Maclaurin Mn(x).\n" )
printf( "Determine, ou monte um algoritmo de busca que determine, o grau ‘n’ mínimo\n" )
printf( "necessário e os coeficientes de Mn(x), para que o erro de truncamento máximo\n" )
printf( "‘exato’ entre Mn(x) e f(x) seja da ordem de O(10-2) (<√10*10-2);\n\n" )



