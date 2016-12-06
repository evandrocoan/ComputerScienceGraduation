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

#{

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

# profile clear
# profile on

printf( "7.3a). Determine funções representativas dos pontos tabelados por ajuste de curvas na faixa medida\n" )
printf( "(sugestão use função polinomial de grau n=1 e 2).\n" )
printf( "\n" )

% Construcao do sistema do ajuste polinomial
function a = fdetajustePn(n,m,x,y)

    %N. de equacoes e incognitas
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

%Pn(xi)=a(1)+a(2)*xi+a(3)*xi^2+...+a(n)*xi^(n-1)+a(n+1)*xi^n
%Pn(xi)=a(1)+xi*(a(2)+xi*(a(3)+...+xi*(a(n)+xi*a(n+1))...))% HORNER
function y=fPnH(n,a,xi)

    %calcula y p/ cada elemento de xi
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

    %valor médio
    ym=0;

    for k=1:m

        ym=ym+y(k);

    end

    ym=ym/m;
    SQT=0;

    %soma dos quadrados totais
    for k=1:m

       SQT=SQT+(y(k)-ym )^2;

    end

    SQE=0;

    %soma do quadrado dos residuos
    for k=1:m

        SQE=SQE+(y(k)-fPnH(n,a,x(k)))^2;

    end

    %coeficiente de determinação simplificado
    R2=1-SQE/SQT;

end

% Algoritmo de Cholesky
%Matriz de coeficientes simétrica, positiva definida, de ordem n x n
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

    % Resolvendo o sistema para o termo independente b
    %L.C=b
    c(1)=b(1)/L(1,1);

    for i=2:n

        soma=0;

        for j=1:i-1

            soma=soma+L(i,j)*c(j);

        end

        c(i)=(b(i)-soma)/L(i,i);

    end

    %U*X=C ->LT*X=C
    x(n)=c(n)/L(n,n);

    for i=n-1:-1:1
        soma=0;

        for j=i+1:n

            soma=soma+L(j,i)*x(j);%Note o uso da matriz U=transposta de L

        end

        x(i)=(c(i)-soma)/L(i,i);

    end

end


# Algoritmo de ajuste polinomial (Ex. 7.2):

clear
clc
format long

x=[ 13.9, 37.0, 67.8, 79.0, 85.5, 93.1, 99.2 ];
y=[ 1.04, 1.18, 1.29, 1.35, 1.28, 1.21, 1.06 ];

%Numero de pontos experimentais
m = numel( x );

xinicial=min(x);
xfinal  =max(x);

% Numero de intervalos para analise e plotagem
np= 64;

% Valor espaçamento de cada intervalo para plotagem
hp=(xfinal-xinicial)/np;

% Valores dos 'np+1' pontos xp(k) (k=1:np+1)
xp=xinicial:hp:xfinal;

%
% Aproximação de função por Ajuste de curvas (INDICADA PARA PONTOS EXPERIMENTAIS)
%

% Ajuste a um polinomio de 1. grau, escolhido em função de seu comportamento grafico;
n=1;
m;
coefficientsForPn1=fdetajustePn(n,m,x,y);

% Calculo dos valores do polinomio ajustado em todos os xp
ya1=fPnH(n,coefficientsForPn1,xp);
desvioParaP1=fdesvioPn(n,coefficientsForPn1,m,x,y);
R1=fCoefDeterminacaoPn(n,coefficientsForPn1,m,x,y);

n
coefficientsForPn1


% Ajuste a um polinomio de 2. grau, escolhido em funçãode seu comportamento grafico;
n=2;
coefficientsForPn2=fdetajustePn(n,m,x,y);

% Calculo dos valores do polinomio ajustado em todos os xp
ya2=fPnH(n,coefficientsForPn2,xp);
desvioParaP2=fdesvioPn(n,coefficientsForPn2,m,x,y);
R2=fCoefDeterminacaoPn(n,coefficientsForPn2,m,x,y);

n
coefficientsForPn2


% Ajuste a um polinomio de 3. grau, escolhido em funçãode seu comportamento grafico;
n=3;
coefficientsForPn3=fdetajustePn(n,m,x,y);

% Calculo dos valores do polinomio ajustado em todos os xp
ya3=fPnH(n,coefficientsForPn3,xp);
desvioParaP3=fdesvioPn(n,coefficientsForPn3,m,x,y);
R3=fCoefDeterminacaoPn(n,coefficientsForPn3,m,x,y);

n
coefficientsForPn3


% Ajuste a um polinomio de 4. grau, escolhido em funçãode seu comportamento grafico;
n=4;
coefficientsForPn4=fdetajustePn(n,m,x,y);

% Calculo dos valores do polinomio ajustado em todos os xp
ya4=fPnH(n,coefficientsForPn4,xp);
desvioParaP4=fdesvioPn(n,coefficientsForPn4,m,x,y);
R4=fCoefDeterminacaoPn(n,coefficientsForPn4,m,x,y);

n
coefficientsForPn4


% Ajuste a um polinomio de 5. grau, escolhido em funçãode seu comportamento grafico;
n=5;
coefficientsForPn5=fdetajustePn(n,m,x,y);

% Calculo dos valores do polinomio ajustado em todos os xp
ya5=fPnH(n,coefficientsForPn5,xp);
desvioParaP5=fdesvioPn(n,coefficientsForPn5,m,x,y);
R5=fCoefDeterminacaoPn(n,coefficientsForPn5,m,x,y);

n
coefficientsForPn5


# % Ajuste a um polinomio de 6. grau, escolhido em funçãode seu comportamento grafico;
# n=6;
# coefficientsForPn6=fdetajustePn(n,m,x,y);

# % Calculo dos valores do polinomio ajustado em todos os xp
# ya6=fPnH(n,coefficientsForPn6,xp);
# desvioParaP6=fdesvioPn(n,coefficientsForPn6,m,x,y);
# R6=fCoefDeterminacaoPn(n,coefficientsForPn6,m,x,y);

# n
# coefficientsForPn6


m
x
y

# Normally, high-level plot functions like plot or mesh call newplot to initializ
# the state of the current axes so that the next plot is drawn in a blank window with
# default property settings. To have two plots superimposed over one another, use the
# hold function.
# https://www.gnu.org/software/octave/doc/v4.0.0/Manipulation-of-Plot-Windows.html#XREFhold
hold all;

# You can open multiple plot windows using the `figure(1)` function.
# https://www.gnu.org/software/octave/doc/v4.0.3/Multiple-Plot-Windows.html
# figure(1);

# Octave can display more than one plot in a single figure. The simplest way to do
# this is to use the subplot function to divide the plot area into a series of subplot
# windows that are indexed by an integer.
# https://www.gnu.org/software/octave/doc/v4.2.0/Multiple-Plots-on-One-Page.html
# subplot (2, 1, 1)

% Plotagem dos pontos originais (*) e ajustadores
plot(x,y,'*','markersize',20);
legendText(end+1) = { 'f(x) tabelada' };

# figure(2);
# subplot (2, 1, 2)

plot(xp,ya1,'--b','LineWidth',2);
legendText(end+1) = { 'P1(x) ajuste' };

plot(xp,ya2,'-k','LineWidth',2);
legendText(end+1) = { 'P2(x) ajuste' };

plot(xp,ya3,'-g','LineWidth',2);
legendText(end+1) = { 'P3(x) ajuste' };

plot(xp,ya4,'-r','LineWidth',2);
legendText(end+1) = { 'P4(x) ajuste' };

plot(xp,ya5,'-c','LineWidth',2);
legendText(end+1) = { 'P5(x) ajuste' };

# plot(xp,ya6,'-y','LineWidth',2);
# legendText(end+1) = { 'P6(x) ajuste' };

legend(legendText,'location','northwest');
grid on;
hold off;

# By default, Octave refreshes the plot window when a prompt is printed, or when
# waiting for input. The drawnow function is used to cause a plot window to be updated.
# https://www.gnu.org/software/octave/doc/v4.0.0/Manipulation-of-Plot-Windows.html
# drawnow();

printf( "Dentre as funções testadas a que melhor parece se adequar aos pontos é\n" );
printf( "o Polinômio de grau 4. Essa conclusao foi feita analizando o gráfico das funções.\n\n\n" );

#}


printf( "7.3b). Determine uma função representativa dos pontos tabelados por interpolação polinomial na\n" )
printf( "faixa medida.\n" )
printf( "\n" )

x=[ 13.9, 37.0, 67.8, 79.0, 85.5, 93.1, 99.2 ];
y=[ 1.04, 1.18, 1.29, 1.35, 1.28, 1.21, 1.06 ];

a = min( x );
b = max( x );

n = numel( x ) - 1;
h = ( b - a ) / n;


# coef_by_me =
#   -1.164279323185479   1.399845394498246  -0.235566071312767
#
# -1.164279323185479 + 1.399845394498246*x^1 - 0.235566071312767*x^2
#
coef_by_me      = interpolacaoPolinomial( x, y, n );
coef_by_polyfit = fliplr( polyfit( x, y, n ) );

xInterPontos = a - 1 : h / 20 : b + 1;
yAproximado  = fPnPorHorner( n, coef_by_me, xInterPontos );

coef_by_me


# To plot the graphcs
hold all;
grid on;
plot( x, y , '*', 'markersize', 20 );
legendText(end+1) = { 'f(x) tabelada' };

plot( xInterPontos, yAproximado, '-k', 'LineWidth', 2 );
legendText(end+1) = { 'Interpolacao Polinomial' };

legend(legendText,'location','north');
hold off;




#{

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

#}




# profile off
# printf( "\n" );
# profshow( profile ("info"), 8 )
#




