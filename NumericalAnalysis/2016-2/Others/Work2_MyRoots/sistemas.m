#{
1). Dada a seguinte eq. polinomial de coeficientes complexos com grau n=5,
determine e imprima as suas n raízes (em precisão double), respectivas
multiplicidades M e o Resto limite escolhido Rlim. Use critério de parada no
limite da precisão da sua variável (sugestão (abs(dx)+abs(R(1))<1e-12 (altere
se necessário) e 120 iterações máximas). Use o método de Newton com a correção
pela multiplicidade M.

P5(x)= 1*x^5 + ( -3.9 -1*I)*x^4 + ( 5.7 + 3.9*I )*x^3 + ( -3.7 -5.7*I)*x^2 + ( 0.9 + 3.7*I )*x + ( 0.0 - 0.9*I ) = 0


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
#format rat
#output_precision(30)
#output_max_field_width(0)

addpath( 'polynoms' )
addpath( 'nonlinears' )



##############################################################################################################
##############################################################################################################

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

printf( "\n\n\n2). Determine essas raízes de P5(x)=0 pela função roots() do próprio octave, \n\
        do WolframAlpha e compare com os seus resultados.\n\n" )


# Wolfram Mathematica 10 (WolframAlpha program for computers, as octave/matlab):
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
#
# Calculo pelo Wolfram Alpha Free:
#
# https://www.wolframalpha.com/input/?i=x%5E5-(3.9+%2B1.+I)+x%5E4%2B(5.7+%2B3.9+I)+x%5E3-(3.7+%2B5.7+I)+x%5E2%2B(0.9+%2B3.7+I)+x-(0.+%2B0.9+I)+%3D+0
#
# x = 0.9
# x = 0.99999
# x = 1. i
# x = 1.00001+8.96723×10^-6 i
# x = 1.00001-8.96723×10^-6 i

raizesDoPolinomioPeloWolfram = [
                                 0.9,
                                 0.99999,
                                 complex( 0      ,  1.0        ),
                                 complex( 1.00001,  8.96723e-6 ),
                                 complex( 1.00001,  8.96723e-6 ),
                               ]

raizesDoPolinomioPeloOctave    = roots( coeficientesDoPolinomio )
raizesDoPolinomioPeloProfessor = [
                                   complex( 0,  1.0 ),
                                   0.9,
                                   1.0,
                                   1.0,
                                   1.0
                                 ]

diferencaEntreMeuRootsVesusOctave(1)   = abs( raizesDoPolinomioPeloMeuRoots(1) .- raizesDoPolinomioPeloOctave(3) );
diferencaEntreMeuRootsVesusOctave(1) .+= abs( raizesDoPolinomioPeloMeuRoots(1) .- raizesDoPolinomioPeloOctave(4) );
diferencaEntreMeuRootsVesusOctave(1) .+= abs( raizesDoPolinomioPeloMeuRoots(1) .- raizesDoPolinomioPeloOctave(5) );
diferencaEntreMeuRootsVesusOctave(2)   = abs( raizesDoPolinomioPeloMeuRoots(3) .- raizesDoPolinomioPeloOctave(2) );
diferencaEntreMeuRootsVesusOctave(3)   = abs( raizesDoPolinomioPeloMeuRoots(2) .- raizesDoPolinomioPeloOctave(1) );

printf( "Os resultados dados pelo WolframMathematica foram iguais as raízes fornecidas pelo\n\
         Professor, como podemos ver logo acima. Entretanto, as raízes do octave foram\n\
         não tão boa, contendo mais erros, quando comparados com as raízes calculadas\n\
         pelo meu roots. A seguir podemos ver essas diferenças de resíduos, e nelas \n\
         notamos que pelo fato de calcularmos a multiplicidade da raiz, conseguimos um \n\
         erro menor no calculo da raiz de multiplicidade três: \n\n" )

diferencaEntreMeuRootsVesusOctave.'



##############################################################################################################
##############################################################################################################

printf( "\n\n\nGrupo 19, exercícios 3 = 3, 4 = 1\n\n" )
printf( "ExemplodeGrupo.m\n" )
printf( "numerogrupo=5\n" )
printf( "if (mod(numerogrupo,16)==0) numeroexercicio3=16 else numeroexercicio3=mod(numerogrupo,16) end\n" )
printf( "if (mod(numerogrupo,3 )==0)  numeroexercicio4= 3 else  numeroexercicio4=mod(numerogrupo,3 ) end\n" )
printf( "\n" )
printf( "3.3) Compare a eficiência do Método da Newton tradicional de 1ª ordem com o Método de Newton de\n" )
printf( "2ª ordem, partindo de uma mesma condição inicial x0 na determinação de uma raiz positiva de \n" )
printf( "x^10 – 2 = 0 ( x = 2^(1/10) ) com todos os dígitos exatos na variável double.\n" )
printf( "Sugestão para valor inicial xi = 1.\n\n" )

tolerancia = 1e-15

printf( "\nStarting the fMetodoDeNewtonOrdem1...\n" )
xi                        = 1
[ x1, passos, diferenca ] = fMetodoDeNewtonOrdem1( xi, tolerancia )

printf( "\nStarting the fMetodoDeNewtonOrdem2...\n" )
xi                        = 1
[ x2, passos, diferenca ] = fMetodoDeNewtonOrdem2( xi, tolerancia )

printf( "\nA diferença da precisão de ambos não é perceptível pois já estamos no limite da\n\
         variável double. Mas podemos perceber que a segunda ordem custou 1 - 4/6 = 33.33%%,\n\
         passos a menos que o método de primeira ordem. A seguir podemos ver a diferença entre\n\
         a os resultados de ordem primeira e segunda ordem:\n\n" )

diferencaEntrePrimeiraOrdemVersusSegundaOrdem = abs( x1 - x2 )



##############################################################################################################
##############################################################################################################

printf( "\n\n\n4.1) Use o seguinte sistema linear para os próximos exercícios:" )
printf( "\n" )
printf( "function x = f1( x1, x2 )\n" )
printf( "    x = sin( x1 ) + cos( x2 ) - 1\n" )
printf( "end\n" )
printf( "\n" )
printf( "function x = f2( x1, x2 )\n" )
printf( "    x = x1^2 + x2^2 - 3\n" )
printf( "end\n" )
printf( "\n" )
printf( "1a). Calcule a solução X para o sistema de n=2 equações não lineares pelo método de Newton.\n" )
printf( "Considerando como valores iniciais X0=[+1 +1] e como critério limite de parada \n\
         max(|Δxj)|)<10^-2 ∀ j.)\n\n" )

xi       = [ 1, 1 ]
criterio = 1e-2

[ x, passos, residuo_fgauss, residuo_sistema ] = fNewtonSistemasNaoLineares_a( xi, criterio )



printf( "\n\n1b). Monte um algoritmo que determine e imprima a solução X do sistema de n=2 equações não\n" )
printf( "lineares pelo método de Newton.\n" )
printf( "Considerando como valores iniciais X0=[+1 +1] e como critério limite de parada \n\
         max(|Δxj)|)<10-14 ∀ j.\n\n" )

xi       = [ 1, 1 ]
criterio = 1e-14

[ x, passos, residuo_fgauss, residuo_sistema ] = fNewtonSistemasNaoLineares_a( xi, criterio )



printf( "\n\n1c). Aplique um algoritmo que determine e imprima a solução X do sistema de n=2 equações não\n" )
printf( "lineares pelo método de Newton com derivadas calculadas numericamente.\n" )
printf( "Considerando como valores iniciais X0=[+1 +1] e como critério limite de parada \n\
         max(|Δxj)|)<10-14 ∀ j.\n\n" )

xi       = [ 1, 1 ]
criterio = 1e-14

[ x, passos, residuo_fgauss, residuo_sistema ] = fNewtonSistemasNaoLineares_c( xi, criterio )



printf( "\n\n1d). Aplique um algoritmo que determine e imprima a solução X do sistema de n=2 equações não\n" )
printf( "lineares pelo método de Broyden com derivadas calculadas numericamente.\n" )
printf( "Considerando como valores iniciais X0=[+1 +1] e como critério limite de parada\n" )
printf( "Σ j=1..n, |Δx_j| < 10^-14 ∀j.\n\n" )

xi                                         = [ 1, 1 ]
[ x, passos, diferenca, residuo_sistema ]  = fBroyden( xi )






