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
    O erro é: Erro = ValorAproximado - ValorExato
    Error(x) = Pn(x) - f(x)

Exemplo:

n = 2,
Pn(x) = a(1) + a(2)*x^1 + a(3)*x^2

Resultando na equação:
f(x) = a(1) + a(2)*x^1 + a(3)*x^2

Então, para resolver esse problema basta criar um sistema com 3 equações,
utilizando/escolhendo 3 pontos para calcular a função f(x).

Dividindo o nosso intervalo [ 1, 2 ] em 3 pontos, pois temos três incógnitas a(1), a(2) e a(3).

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


a = 1
b = 2
n = 2

h = ( b - a ) / n
x = 1: h : 2
y = f( x )


coef_by_polyfit = polyfit( x, y, n )

# O Octave/Mathematica invertem os coeficientes do polinômio, por isso a utilizamos
# a funções fliplr para inverte-los.
# 
# coef_by_polyfit = fliplr( coef_by_polyfit )

# coef_by_polyfit =
#   -0.235566071312766   1.399845394498243  -1.164279323185477
# 
# -0.235566071312766*x^2 + 1.399845394498243*x^1 - 1.164279323185477*x^0
#

coef_by_me = interpolacaoPolinomial( x, y, n )

# coef_by_me =
#   -1.164279323185479   1.399845394498246  -0.235566071312767
# 
# -1.164279323185479 + 1.399845394498246*x^1 - 0.235566071312767*x^2

xp = a : h / 20 : b;
#yp = f( xp );

function x = fPn( n, a, xp )
    
    aux1 = a(n) + a(n+1)*xp;
    
    aux2 = 
    
    #yp  = aux*xp + a(n-1); 
    
end

yp = fPn( n, coef_by_me, xp )

plot( x, y, '*', 2.2, 0, xp, xp, 'k' )

