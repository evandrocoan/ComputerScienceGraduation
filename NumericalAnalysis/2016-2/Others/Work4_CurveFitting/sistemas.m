#{
Resolver um exercicio do cap. 7 (Ajustes) e outro do cap. 8 (Integração), conforme numero do seu grupo:

numerogrupo=15

if (mod(numerogrupo,4)==0) numeroexerciciocap7=4 else numeroexerciciocap7=mod(numerogrupo,4) end

if (mod(numerogrupo,7)==0) numeroexerciciocap8=7 else numeroexerciciocap8=mod(numerogrupo,7) end

--------------------------------------------------------------------------------------------------

Ex. numerogrupo = 15

numeroexerciciocap7 = 3
numeroexerciciocap8 = 1

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


printf( "\nGrupo 19, exercícios 7 = 3, 8 = 5\n\n" )
printf( "ExemplodeGrupo.m\n" )
printf( "numerogrupo=19\n" )
printf( "if (mod(numerogrupo,4)==0) numeroexerciciocap7=4 else numeroexerciciocap7=mod(numerogrupo,4) end\n" )
printf( "if (mod(numerogrupo,7)==0) numeroexerciciocap8=7 else numeroexerciciocap8=mod(numerogrupo,7) end\n" )



printf( "\n\n\n##############################################################################################################\n" )
printf( "##############################################################################################################\n" )

printf( "\n7.3). A tabela abaixo relaciona, experimentalmente, a medição do volume de álcool gerado em uma\n" )
printf( "mistura em função da sua temperatura de reação:\n" )
printf( "Temperatura(oC) 13.9 37.0 67.8 79.0 85.5 93.1 99.2\n" )
printf( "Volume    (cm3) 1.04 1.18 1.29 1.35 1.28 1.21 1.06\n" )
printf( "\n" )

profile clear
profile on

printf( "7.3a). Determine funções representativas dos pontos tabelados por ajuste de curvas na faixa medida\n" )
printf( "(sugestão use função polinomial de grau n=1 e 2).\n" )
printf( "\n" )



printf( "7.3b). Determine uma função representativa dos pontos tabelados por interpolação polinomial na\n" )
printf( "faixa medida.\n" )
printf( "\n" )



printf( "7.3c). Plote um gráfico com os pontos experimentais, as funções aproximadoras obtidos em (7.3a) e\n" )
printf( "em (7.3b);\n" )
printf( "\n" )



printf( "7.3d). Calcule o Coeficiente de Determinação R2;\n" )
printf( "\n" )



printf( "7.3e). Faça o gráfico da função ajustada e estabeleça a metodologia mais adequada para representar\n" )
printf( "o comportamento do volume de álcool em função da temperatura na faixa medida. Justifique.\n" )
printf( "\n" )





# plot( x, y, '*', xInterPontos, yAproximado, "g;Funcao Aproximadora do Consumo pelo interpolacaoPolinomial;" )
# legend('location','north');
# grid on;

profile off
printf( "\n" );
# profshow( profile ("info"), 8 )
#



printf( "\n\n\n##############################################################################################################\n" )
printf( "##############################################################################################################\n" )

printf( "\n8.5). Considere a integral I x dx = ∫ ln( )\n" )
printf( "\n" )

profile clear
profile on

printf( "8.5a). Quais métodos podem ser aplicados para calcular numericamente esta\n" )
printf( "integral imprópria? Justifique;\n" )
printf( "\n" )


printf( "8.5b). Monte uma function Gm fGm a b m = ( , , ) , para integrar numericamente um\n" )
printf( "função f x ( ) entre [ , ] a b pelo método de Gauss-Legendre, com até m = 7 pontos\n" )
printf( "com precisão double;\n" )
printf( "\n" )



printf( "5c). Determine e imprima Gm m ( ) e os erros exatos de Gm m ( ) , com m = 2 até 7\n" )
printf( "pontos.\n" )
printf( "\n" )





# plot( x, y, '*', xInterPontos, yAproximado, "g;Funcao Aproximadora do Consumo pelo interpolacaoPolinomial;" )
# legend('location','north');
# grid on;

profile off
printf( "\n" );
# profshow( profile ("info"), 8 )
#



