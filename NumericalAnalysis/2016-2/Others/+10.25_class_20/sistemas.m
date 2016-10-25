
#{
Determine uma aproximação para

function x = f( x )
    x = log( x );
end

para x \in [ 1, 2 ] com erro máximo da ordem de 1e-6 usando apenas +, - e *.

A solução/uma proposta de aproximação é calcular o logaritmo natural ln(x) utilizando
polinômios de grau n.

function x = f_aprox( x )
    x = a(1) + a(2)*x^1 + a(3)*x(^2)+...+a(n+1)*x^n
end

Mas como determinar o `n` em função do erro com 1e-6
e o `a`?
    Error(x) = Pn(x) - f(x)
    O erro é: ErroMax = max( ValorAproximado .- ValorExato )

Exemplo:

n = 2,
Pn(x) = a(1) + a(2)*x^1 + a(3)*x^2

Resultando na equação:
f(x) = a(1) + a(2)*x^1 + a(3)*x^2

Então, para resolver esse problema basta criar um sistema com 3 equações,
utilizando/escolhendo 3 pontos para calcular a função f(x).

Dividindo o nosso intervalo [ 1, 2 ] em 3 pontos 1, 1.5 e 2 então temos três incógnitas a(1), a(2) e a(3).

Para x = 1, 1.5 e 2:
f(x) = a(1) + a(2)*x^1 + a(3)*x^2
f(x) = a(1) + a(2)*x^1 + a(3)*x^2
f(x) = a(1) + a(2)*x^1 + a(3)*x^2

-->
a( 1 )*x(1)^0 + a( 2 )*x(1)^1 + a( 3 )*x(1)^2 = f( x(1) )
a( 1 )*x(2)^0 + a( 2 )*x(2)^1 + a( 3 )*x(2)^2 = f( x(2) )
a( 1 )*x(3)^0 + a( 2 )*x(3)^1 + a( 3 )*x(3)^2 = f( x(3) )



Questões importantes de prova:

1. Como calcular o polinômio?
    Ele tem que passar em cima dos pontos, assim ele fica próximo dos pontos.

2. Qual o valor do erro quando calculado em cima dos pontos do interpolador?
    O erro é zero, pois o polinômio tem que passar em cima dos pontos. Mas
    claro, terá um pequeno resíduo.

#}

clc
clear
format long
split_long_rows(0)
#format rat
#output_precision(30)
#output_max_field_width(0)

#addpath( 'polynoms' )
#addpath( 'nonlinears' )

function x = f( x )
    x = log( x );
end



passos         = 0;
tolerancia     = sqrt( 10 )*1.e-8;
erroMaximoDePn = 1;

a = 1;
b = 2;
n = 1;

while erroMaximoDePn > tolerancia && passos < 20

    h = ( b - a ) / n;
    x = 1 : h : 2;
    y = f( x );

    passos          = passos + 1;
    coef_by_polyfit = polyfit( x, y, n );

    # O Octave/Mathematica invertem os coeficientes do polinômio, por isso a utilizamos
    # a funções fliplr para inverte-los.
    # 
    # coef_by_polyfit = fliplr( coef_by_polyfit )

    # coef_by_polyfit =
    #   -0.235566071312766   1.399845394498243  -1.164279323185477
    # 
    # -0.235566071312766*x^2 + 1.399845394498243*x^1 - 1.164279323185477*x^0
    #

    coef_by_me = interpolacaoPolinomial( x, y, n );

    # coef_by_me =
    #   -1.164279323185479   1.399845394498246  -0.235566071312767
    # 
    # -1.164279323185479 + 1.399845394498246*x^1 - 0.235566071312767*x^2

    xp = a : h / 20 : b;
    #yp = f( xp );

    # Aqui calculamos o valor do polinômio nos pontos xp, utilizando o método de Horner e de Briot Rufini.
    # Isso por que custa muito caro efetuar as operações de potência ao calcular o polinômio:
    # a(1) + a(2)*x^1 + a(3)*x(^2)+...+a(n+1)*x^n
    # 
    #yp = fPnPorBriotRunifi( n, coef_by_me, xp )
    yp = fPnPorHorner( n, coef_by_me, xp );

    yExato = f( xp );

    erroDePn       = abs( yp .- yExato );
    erroMaximoDePn = max( erroDePn );

    n = n + 1;

end

n;
passos;
erroDePn;
erroMaximoDePn;

#plot( x, y, '*', 2.2, 0, xp, yp, 'r', xp, yExato, 'c' )

% plot( xp, erroDePn, 'm', 'linewidth', 2 )
% legend( 'Erro de P(n)', 'location', 'north' )

# Erro de Pn Maximo ( Serie de Taylor ) = max( abs( f_devirade_de_ordem( n + 1 )( x ) ) )*h( n + 1 ) / 4*( n + 1 )
# h = ( b - a ) / n
# Se n = 2, f( x ) = log( x )
# f'( x )     = x^-1
# f''( x )    = ( -1 )*x^-2
# f'''( x )   = ( -1 )*( -2 )*x^-3
# f''''( x )  = ( -1 )*( -2 )*( -3 )*x^-4
#
# max( abs( f'''( x )_x=1 = abs( ( -1 )*( -2 )*( 1 )^-3
# = 2
#
# ErroDePn = abs( 2 )*( 0.5 )^( 2 + 1 ) / 4( 2 + 1 )
#
# ErroDePnEstimado = 0.
#
# Estimativa conservativa é uma estimativa que visa sempre o pior caso, ou seja, tende a ser mais
# alta do que o erro máximo real. Assim serve para fazer uma previsão.
# 
# 
# Se for maior do que 10^( 1/2 ) = 3.16... A ordem do erro é 1.(e+1)
# Se for menor do que 10^( 1/2 ) = 3.16... A ordem do erro é 1.(e)
# 
# Exemplo: 
# 4e-6, tem ordem de erro igual a -5
# 3e-6, tem ordem de erro igual a -6
# 
# O erro do polinômio de interpolação tem maior erro nas pontos por que elas estão mais soltas,
# pois na sua esquerda e direita não há outros pontos para prende-las.
# 










#{

Pn(x) = a(1) + a(2)*x^1 + a(3)*x^2    <-- para n + 1 coeficientes do polinômio


Base Canônica dos Polinômios:

Base  = { x^0, x^1, x^2, ..., x^n }   <-- para n + 1 coeficientes, onde n é o número de pontos em que o
                                          intervalo foi dividido.


Base de Lagrange:

Pn( x ) = \sum_i=1(y i )^n+1 * \prod_j=1&j!=i( (x - x( j )) / (x( i ) - x( j )) )^n+1

Simplificando: 
Pn( x ) = \sum_i=1(y i )^n+1 * auxilar
onde auxilar = \prod_j=1&j!=i( (x - x( j )) / (x( i ) - x( j )) )^n+1

n = 2, f(x) = ln( x )
h = (b - a)/n = (2 - 1)/0.5

x(i) | y(i)
-------------
1    | 0
1.5  | 0.405
2.0  | 0.693

P2( x ) =   y(1) * (( x - x(2) )*( x - x(3) )) / ( (x(1) - x(2) )*( x(1) - x(3) ))
          + y(2) * (( x - x(1) )*( x - x(3) )) / ( (x(2) - x(1) )*( x(2) - x(3) ))
          + y(3) * (( x - x(1) )*( x - x(2) )) / ( (x(3) - x(1) )*( x(3) - x(2) ))

Determine a expressão do poliônimo de Lagrange: (pergunta de prova)

P2(x) =   0            * (( x - 1.5 )*( x - 2   )) / (( 1   - 1.5 )*( 1   - 2   ))
        + 0.4054651081 * (( x - 1   )*( x - 2   )) / (( 1.5 - 1   )*( 1.5 - 2   ))
        + 0.6931478056 * (( x - 1   )*( x - 1.5 )) / (( 2   - 1   )*( 2   - 1.5 ))

P2(x) = coef(1) + coef(2)*x + coef(3)*x^2

#}


clc
clear

passos         = 0;
tolerancia     = sqrt( 10 )*1.e-8;
erroMaximoDePn = 1;

a = 1;
b = 2;
n = 2;
h = ( b - a ) / n;

# Pontos para o gráfico e para o calculo do erro.
x = 1 : h : 2; 
y = f( x );

xp = a : h / 20 : b;
yExato = f( xp );

#yp = f( xp );
yp = fPnCalculoPorLangrangeSemOsCoeficientes( n, x, y, xp )


plot( x, y, '*', 2.2, 0, xp, yp, 'r', xp, yExato, 'b' )

% plot( xp, erroDePn, 'm', 'linewidth', 2 )
% legend( 'Erro de P(n)', 'location', 'north' )

