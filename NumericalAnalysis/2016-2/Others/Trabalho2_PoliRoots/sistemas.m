#{
1). Dada a seguinte eq. polinomial de coeficientes complexos com grau n=5,
determine e imprima as suas n raízes (em precisão double), respectivas
multiplicidades M e o Resto limite escolhido Rlim. Use critério de parada no
limite da precisão da sua variável (sugestão (abs(dx)+abs(R(1))<1e-12 (altere
se necessário) e 120 iterações máximas). Use o método de Newton com a correção
pela multiplicidade M.

P5(x)=1 x^5+(- 3.9 - 1. I) x^4+(5.7 + 3.9 I) x^3+(-3.7 - 5.7 I) x^2+(0.9 + 3.7 I) x+(0. - 0.9 I) = 0


(polinômio fatorado (x-0.9)(x-1)3(x-I), I=sqrt(-1) -> complexo)

2). Determine essas raízes de P5(x)=0 pela função roots() do próprio octave,
do WolframAlpha e compare com os seus resultados.

3). Exercício 3.numeroexercicio3

4). Exercício 4.numeroexercicio4

---------------------------------------------------------------------------------------------------------------------------------------

ExemplodeGrupo.m

numerogrupo=5

if (mod(numerogrupo,16)==0) numeroexercicio3=16 else numeroexercicio3=mod(numerogrupo,16) end

if (mod(numerogrupo,3 )==0)  numeroexercicio4= 3 else  numeroexercicio4=mod(numerogrupo,3 ) end

-----------------------------------------------------------------------------------------------------------------------------------------

numeroexercicio3 = 5
numeroexercicio4 = 2

#}


format long
split_long_rows(0)
#output_precision(30)
#output_max_field_width(0)

printf( "1). Dada a seguinte eq. polinomial de coeficientes complexos com grau n=5, \n" )
printf( "determine e imprima as suas n raízes (em precisão double), respectivas\n" )
printf( "multiplicidades M e o Resto limite escolhido Rlim. Use critério de parada no\n" )
printf( "limite da precisão da sua variável (sugestão (abs(dx)+abs(R(1))<1e-12 (altere\n" )
printf( "se necessário) e 120 iterações máximas). Use o método de Newton com a correção\n" )
printf( "pela multiplicidade M.\n" )
printf( "\n" )
printf( "P5(x)=1 x^5+(- 3.9 - 1. I) x^4+(5.7 + 3.9 I) x^3+(-3.7 - 5.7 I) x^2+(0.9 + 3.7 I) x+(0. - 0.9 I) = 0\n" )
printf( "\n" )
printf( "\n" )
printf( "(polinômio fatorado (x-0.9)(x-1)3(x-I), I=sqrt(-1) -> complexo)\n\n\n" )


maximoDeIteracoes       = 120
criterioDeParada        = 1e-15
restoLimiteEscolhido    = 1e-4
coeficientesDoPolinomio = [
                            1,
                            complex( -3.9, -1.0 ),
                            complex(  5.7,  3.9 ),
                            complex( -3.7, -5.7 ),
                            complex(  0.9,  3.7 ),
                            complex(  0.0, -0.9 )
                          ]


grauDoPolinomio = length( coeficientesDoPolinomio ) - 1
printf( "\n\nStarting the my roots calculation:\n\n" )

[ a, b, c ] = fRoots(
                       coeficientesDoPolinomio,
                       restoLimiteEscolhido,
                       maximoDeIteracoes,
                       criterioDeParada
                    );

raizesDoPolinomioPeloMeuRoots       = a
multiplicidadeDasRaizesPeloMeuRoots = b
iteracoesDaRaiz                     = c



##############################################################################################################
##############################################################################################################

printf( "\n\n2). Determine essas raízes de P5(x)=0 pela função roots() do próprio octave, \n\
        do WolframAlpha e compare com os seus resultados.\n\n" )


# Wolfram Mathematica 10, code used:
#
# OrderedForm=HoldForm[+##]&@@MonomialList[#][[Ordering[Total[#]&@@@CoefficientRules[#],All,GreaterEqual]]]&;
#
# coef = {
#          Complex[1,0],
#          Complex[-3.9,-1.0],
#          Complex[5.7,3.9],
#          Complex[-3.7,-5.7],
#          Complex[0.9,3.7],
#          Complex[0.0,-0.9]
#        }
#
# grau =Length[ coef ] - 1
# poli = x^Range[ 0, grau ].Reverse[ coef ];
# poli //OrderedForm
# Roots[ poli==0, x ]
#
# Output:
# Out[126]= {1,-3.9-1. I,5.7 +3.9 I,-3.7-5.7 I,0.9 +3.7 I,0. -0.9 I}
# Out[127]= 5
# Out[129]= x^5-(3.9 +1. I) x^4+(5.7 +3.9 I) x^3-(3.7 +5.7 I) x^2+(0.9 +3.7 I) x-(0. +0.9 I)
# Out[130]= x==0. +1. I||x==0.9||x==1.||x==1.||x==1.
raizesDoPolinomioPeloOctave  = roots(  coeficientesDoPolinomio )

raizesDoPolinomioPeloWolfram = [
                                 complex( 0,  1.0 ),
                                 0.9,
                                 1.0,
                                 1.0,
                                 1.0
                               ]

raizesDoPolinomioPeloProfessor = [
                                   complex( 0,  1.0 ),
                                   0.9,
                                   1.0,
                                   1.0,
                                   1.0
                                 ]





