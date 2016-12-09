

clc
clear
close all

more off
format long
split_long_rows(0)

#format rat
#output_precision(30)
#output_max_field_width(0)

#
printf( "\nQuestão 1.a\n" )

a = 0
b = 1
# printf( "\n" )

# Problem 1 integral
integralAtIntervalab = erf(1);


printf( "\nSimpson's degree 1\n" )
n = 2700

integralAtIntervalab
aproximateIntegral__ = problem1SimpsonNumericIntegralDegree1( n  , a, b )
estimatedIntegral___ = problem1SimpsonNumericIntegralDegree1( 2*n, a, b )

printf( "\n" )

erroExato___ = abs( integralAtIntervalab - aproximateIntegral__ )
erroEstimado = abs( aproximateIntegral__ - estimatedIntegral___ )
#





#
printf( "\n\n\nQuestão 1.b\n" )

a = 0
b = 1
# printf( "\n" )

# Problem 1 integral
integralAtIntervalab = erf(1);

printf( "\nSimpson's degree 2\n" )
n = 32

integralAtIntervalab
aproximateIntegral__ = problem1SimpsonNumericIntegralDegree2( n  , a, b )
estimatedIntegral___ = problem1SimpsonNumericIntegralDegree2( 2*n, a, b )

printf( "\n" )

erroExato___ = abs( integralAtIntervalab - aproximateIntegral__ )
erroEstimado = abs( aproximateIntegral__ - estimatedIntegral___ )

#




#
printf( "\n\n\nQuestão 1.c\n" )

a = 0
b = 1
printf( "\n" )

# GaussLegendre
#
# It is useful when/ALWAYS USE this Integral when its is not continue on its interval
# extremities as [0,1] for 1/x, which does not converge.
#
m = 5
printf( "\n" )

# Problem 1 integral
integralAtIntervalab_ = erf(1);


gaussLegendreIntegralAproximate = problem1GaussLegendreIntegral( m  , a, b );
gaussLegendreIntegralEstimado__ = problem1GaussLegendreIntegral( m*2, a, b );

erroEstimado = abs( gaussLegendreIntegralAproximate - gaussLegendreIntegralEstimado__ );
erroExato___ = abs( gaussLegendreIntegralAproximate - integralAtIntervalab_ );

integralAtIntervalab_
gaussLegendreIntegralAproximate

printf( "\n" )

erroExato___
erroEstimado

# printf( "\n" )


#



#
printf( "\n\n\nQuestão 2.\n\n" )


Coef_6 = 4.72935285881143e-03;

# Numero de nós de Chebyshev, i.e., grau da precisão numérica da integral.
m = 10

# Calcule o segundo 3º coeficiente da serie de Chebyshev de f(x) = cos(x)
#
#     b(2) = (2/pi)*integral( f(x)*T(1)) / sqrt(1-x^2) ) dx
#
# b(2) multiplica T(2) `Polinômio` de Chebyshev. T(2) = x^2 - 1;
# Find out how to create a function here and pass it. The @cos here is supposed to be `cos(x)*x`.
#
b6____ = ( 2/pi ) * problem1GaussChebyshevIntegral( m, @erf, 5 );


erroExato = abs( b6____ - Coef_6 );

b6____
Coef_6

printf( "\n" )
erroExato


#



printf( "\n\n\nQuestão 3.\n" )

printf( "\n" )

printf( "\n\n\nQuestão 3.a\n" )

printf( "\n" )


# Construcao do sistema do ajuste polinomial
function a = fdetajustePn(n,m,x,y)

    #N. de equacoes e incognitas
    neq = n+1;

    for i=1:neq

        for j=1:neq

            soma1 = 0;

            for k=1:m

                soma1 = soma1 + (x(k))^(i+j-2);

            end %k

            A(i,j)=soma1;

        end %j

        soma2 = 0;

        for k=1:m

            soma2 = soma2 + y(k)*(x(k))^(i-1);

        end %k

        b(i) = soma2;

    end %i

    [A transpose(b)];
    a = fCholesky(neq,A,b);

end

#Pn(xi)=a(1)+a(2)*xi+a(3)*xi^2+...+a(n)*xi^(n-1)+a(n+1)*xi^n
#Pn(xi)=a(1)+xi*(a(2)+xi*(a(3)+...+xi*(a(n)+xi*a(n+1))...))% HORNER
function y=fPnH(n,a,xi)

    #calcula y p/ cada elemento de xi
    for ip=1:length(xi)

        y(ip)=a(n+1);

        for i=n:-1:1

            y(ip)=a(i)+y(ip)*xi(ip);

        end

    end

end

function D=fdesvioPn(n,a,m,x,y)

    D=0;

    for k=1:m

        D=D+(fPnH(n,a,x(k))-y(k))^2;

    endfor

end

function R2=fCoefDeterminacaoPn(n,a,m,x,y)

    #valor médio
    ym=0;

    for k=1:m

        ym=ym+y(k);

    end

    ym=ym/m;
    SQT=0;

    #soma dos quadrados totais
    for k=1:m

       SQT=SQT+(y(k)-ym )^2;

    end

    SQE=0;

    #soma do quadrado dos residuos
    for k=1:m

        SQE=SQE+(y(k)-fPnH(n,a,x(k)))^2;

    end

    #coeficiente de determinação simplificado
    R2=1-SQE/SQT;

end

# Algoritmo de Cholesky
#Matriz de coeficientes simétrica, positiva definida, de ordem n x n
function x=fCholesky(n,a,b)

    k=1;
    L(1,1)=sqrt(a(1,1));

    for i=2:n

        L(i,1)=a(i,1)/L(1,1);

    endfor

    for k=2:n-1

        soma=0;

        for r=1:k-1

            soma=soma+L(k,r)^2;

        endfor %r

        L(k,k)=sqrt(a(k,k)-soma);

        for i=k+1:n

            soma=0;

            for r=1:k-1

                soma=soma+L(i,r)*L(k,r);

            endfor %r

            L(i,k)=(a(i,k)-soma)/L(k,k);

        endfor %i

    endfor %k

    k=n;
    soma=0;

    for r=1:k-1

        soma=soma+L(k,r)^2;

    endfor %r

    L(k,k)=sqrt(a(k,k)-soma);

    # Resolvendo o sistema para o termo independente b
    #L.C=b
    c(1)=b(1)/L(1,1);

    for i=2:n

        soma=0;

        for j=1:i-1

            soma=soma+L(i,j)*c(j);

        end

        c(i)=(b(i)-soma)/L(i,i);

    end

    #U*X=C ->LT*X=C
    x(n)=c(n)/L(n,n);

    for i=n-1:-1:1
        soma=0;

        for j=i+1:n

            soma=soma+L(j,i)*x(j);%Note o uso da matriz U=transposta de L

        end

        x(i)=(c(i)-soma)/L(i,i);

    end

end





# 3. Dado um conjunto de m=9 pontos experimentais:
m = 9

# printf( "\n" )

x=[0.1 0.2 0.4 0.6 0.8 1.0 1.2 1.4 1.6];
y=[0.1 0.9 1.7 2.3 2.8 3.1 3.4 3.6 3.7];

#Numero de pontos experimentais
# m = numel( x );

xinicial=min(x);
xfinal  =max(x);

# Numero de intervalos para analise e plotagem
np= 64;

# Valor espaçamento de cada intervalo para plotagem
hp=(xfinal-xinicial)/np;

# Valores dos 'np+1' pontos xp(k) (k=1:np+1)
xp=xinicial:hp:xfinal;

#
# Aproximação de função por Ajuste de curvas (INDICADA PARA PONTOS EXPERIMENTAIS)
#

# Ajuste a um polinomio de 1. grau, escolhido em função de seu comportamento grafico;
n=3;


m;
coefficientsForPn3=fdetajustePn(n,m,x,y);

# Calculo dos valores do polinomio ajustado em todos os xp
yAjuste3                  = fPnH( n, coefficientsForPn3, xp );
desvioParaP3              = fdesvioPn( n, coefficientsForPn3, m, x, y );
coeficienteDeterminacaoR3 = fCoefDeterminacaoPn( n, coefficientsForPn3, m, x, y );

n
printf( "\n" )
coefficientsForPn3

coeficienteDeterminacaoR3
desvioParaP3
printf( "\n" )



# Interpolador polinomial
a = min( x );
b = max( x );

n = numel( x ) - 1;
h = ( b - a ) / n;


# coef_by_me =
#   -1.164279323185479   1.399845394498246  -0.235566071312767
#
# -1.164279323185479 + 1.399845394498246*x^1 - 0.235566071312767*x^2
#
# coef_by_me      = interpolacaoPolinomial( x, y, n );
coef_by_polyfit = fliplr( polyfit( x, y, n ) );

xInterPontos = a - 0 : h / 20 : b + 0;

# yAproximado  = fPnPorHorner( n, coef_by_me, xInterPontos );
yAproximado  = fPnPorHorner( n, coef_by_polyfit, xInterPontos );

# coef_by_me
coef_by_polyfit


desvioParaIntepolador     = fdesvioPn( n, coef_by_polyfit, m, x, y );
coeficienteDeterminacaoR1 = fCoefDeterminacaoPn( n, coef_by_polyfit, m, x, y );


printf( "\n" )
desvioParaIntepolador
coeficienteDeterminacaoR1
printf( "\n" )


hold all;
grid on;


plot(x,y,'*','markersize',20);
legendTextProblem4b(end+1) = { 'f(x) tabelada' };


plot(xp,yAjuste3,'-r','LineWidth',2);
legendTextProblem4b(end+1) = { 'P3(x) ajuste' };

plot( xInterPontos, yAproximado, '-k', 'LineWidth', 2 );
legendTextProblem4b(end+1) = { 'Interpolacao Polinomial' };


# legend(legendTextProblem71e,'location','northwest');
# hold off;




printf( "\n\n\nQuestão 3.b\n" )



coeficientes = ajusteDeCurvasLinearesParaCos( m, x, y )

# Os pontos podem não estar ordenados, portanto calculamos os intervalos a partir dos pontos
# mínimo e máximo de `x`.
xInterpontos = min( x ) - 0.0 : 0.01 : max( x ) + 0.0;
yInterpontos = coeficientes(1).*sin(xInterpontos) .+ coeficientes(2).*log(xInterpontos);


desvioMedioParaBLinear = sum( ( coeficientes(1).*sin(x) .+ coeficientes(2).*log(x) ) .- y ) / m;


# printf( "\n" )
desvioMedioParaBLinear
printf( "\n" )


# hold all;
# grid on;

plot( xInterpontos, yInterpontos );
legendTextProblem4b(end+1) = { 'Ajuste Linear' };

# legend(legendTextProblem4b,'location','northwest');
# hold off;




aInicial = [ 1.32 0.38 ];

printf( "\n" )
f1Problema1aInicial = f1Problema1( aInicial )
f2Problema1aInicial = f2Problema1( aInicial )

printf( "\n" )
functions(1).vector = @f1Problema1;
functions(2).vector = @f2Problema1;

aSolucao = fNewtonSistemasNaoLineares( aInicial, functions );

# Podemos perturbar a os coeficientes aSolucao, 10% para cima ou -10% para baixo.
# Caso ele esteja em um ponto de minimo, descolar um pouco para esquerda (*0.9) ou para direita (*1.1)
# o valor do desvio vai subir.
aSolucao = aSolucao.*1.0;

# Os pontos podem não estar ordenados, portanto calculamos os intervalos a partir dos pontos
# mínimo e máximo de `x`.
xInterpontosNaoLinear           = min( x ) - 0.0 : 0.01 : max( x ) + 0.0;
yInterPontosAproximadoNaoLinear = sin( aSolucao(1).*xInterpontosNaoLinear ) .+ log( aSolucao(2).*xInterpontosNaoLinear );



# A se a média dos desvios for zero, então temos que o desvio quadrático está correto, pois
# partimos do principio:
#
#     Usando a(1) e a(2) de modo que D seja mínimo:
#     desvioLocal = a(1)*x(k) + a(2)*cos(x(k)) - y(k)
#     D( a(1), a(2) ) = \sum_k=1^m ( desvioLocal )^2
#
# Desvio quadrático médio = sum(desvioLocal.^2) / m
#

desvioLocal      = aSolucao(1) .* sin( x.*aSolucao(2) ) .- y
desvioLocalMedio = sum( desvioLocal ) / m

desvioLocalQuadratico      = desvioLocal.^2
desvioLocalQuadraticoMedio = sum( desvioLocalQuadratico ) / m

printf( "\n" )
f1Problema1 = f1Problema1( aSolucao )
f2Problema1 = f2Problema1( aSolucao )




plot( xInterpontosNaoLinear, yInterPontosAproximadoNaoLinear );
legendTextProblem4b(end+1) = { 'Ajuste Nao Linear' };




legend(legendTextProblem4b,'location','northwest');
hold off;


aSolucao



