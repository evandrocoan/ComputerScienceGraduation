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

tolerancia = sqrt(10)*1e-2






