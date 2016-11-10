


clc
clear
close all

more off
format long
split_long_rows(0)


###################################################################################################
###################################################################################################
###################################################################################################
# Método de MacLaurin
#
# Derivatives
# ( u^n )' = n*u^(n-1)*u'   <-- Chain rule, the external derivative times the internal derivative.
#
# To create the MacLaurin coefficients we need to derivate several times aways applying them on the
# 0 point, because that is the MacLaurin Series, the Taylor series applied on the zero point.
#
# 1. Always to transform the domain from [a, b] to [-1, +1] using this formula:
#    x(t) = 0.5*( b-a )*t + 0.5*( b+a )
#
# Now we got f( x(t) ) with t on [-1, 1]. This is useful/necessary to the Chebyshev Series.
# And we always to apply the derivatives on this new domain [-1, +1] at the point 0, to deduce
# the nth derivate formula at the point 0 by backtracking.
# Como vamos padronizar o domínio [a, b] da aproximado para [-1, 1], pode-se fixar o x da série em 0.
#
function run_maclarin_test( n, a, b, targetFunction )

    h = (b-a)/n

    coefMaclaurin = calculateMaclaurinCoefficients( n, a, b, targetFunction )

    x = a : h : b
    y = targetFunction( x )

    xInterPontos = a : h/20 : b
    yInterPontos = targetFunction( xInterPontos )

    tInterPontos = MaclaurinLinearTransformationDomainIn( xInterPontos, a, b )
    yAproximado  = fPnPorBriotRunifi( n, coefMaclaurin, tInterPontos )

    # Erro máximo deve ser calculado pelas formulas deve ser feito nos limites do nosso
    # intervalo [-1,1], ou seja, em -1 ou em 1.
    #
    # O gráfico do erro mostra que o erro é 0 no ponto 0 (do intervalo [-1,1]), por que foi ali
    # que fizemos a expansão da série de Maclaurin. O contrário da Sério de Chebyshev, que possui
    # um erro mais distribuído ao londo do intervalo (Comparar um Gráfico de Chebyshev e Maclaurin).
    erroDeMaclaurin       = abs( yAproximado .- yInterPontos )
    erroMaximoDeMaclaurin = max( erroDeMaclaurin )

    % plot( x, y, '*' )
    % plot( x, y, '*', xInterPontos, yInterPontos, 'g', xInterPontos, yAproximado, 'b' )

end




# Profiling
#
# Command: profile on
# Command: profile off
# Command: profile resume
# Command: profile clear
# Function File: S = profile ("status")
# Function File: T = profile ("info")
#
# https://www.gnu.org/software/octave/doc/v4.0.1/Profiling.html
profile on





# Numero de pontos do Gráfico e grau n da Série de:
# 1. Chebyshev
# 2. MacLaurin
# 3. Pade
#
n = 5

# Grau de precisão da:
# 1. Integral Numérica de Chebyshev e número de nós de Chebyshev.
# 2. M de Pade.
#
m = 2

# Domínio
a = 1
b = 2


% run_maclarin_test( n, a, b, @fLog )



# 4-aproximação racional de padé
#
# yPade(x) = (a(1) + a(2)x + a(3)x^2 + a(4)x^3)/(1 + b(1)x + b(2)*x^2)
#
# R32 = (a(1) + a(2)x + a(3)x^2 + a(4)x^3)/(1 + b(1)x + b(2)*x^2)
#

h = (b-a)/n;

x = a : h : b;
y = fLog( x );


grauDeMaclaurinParaPade = n + m;
coefMaclaurinParaPade   = calculateMaclaurinCoefficients( grauDeMaclaurinParaPade, a, b, @fLog )

xInterPontos      = a : h/20 : b;
yInterPontosExato = fLog( xInterPontos );

# R32 = (a(1) + a(2)x + a(3)x^2 + a(4)x^3)/(1 + b(1)x + b(2)*x^2)
#
[ aPadeCoefficients, bPadeCoefficients ] = calculatePadeCoefficients( n, m, coefMaclaurinParaPade );
tInterPontos = MaclaurinLinearTransformationDomainIn( xInterPontos, a, b );

# For fLog( x ) in [1, 2] with n = 3 and m = 2
ap_correct_value = [ 0.4054651   0.4955194   0.0912933   0.0012346 ];
aPadeCoefficients;

# For fLog( x ) in [1, 2] with n = 3 and m = 2

bp_correct_value = [ 1.00000     0.40000     0.03333     0.00000   ];
bPadeCoefficients;

# yAproximado = fPnPorBriotRunifi( n, coefMaclaurin, tInterPontos )
% yAproximadoPorPade = polyval( aPadeCoefficients, tInterPontos ) / polyval( bPadeCoefficients, tInterPontos );
yAproximadoPorPade = fPnPorBriotRunifi( ...
        n, aPadeCoefficients, tInterPontos ) ./ fPnPorBriotRunifi( ...
        m, bPadeCoefficients, tInterPontos );

#
# O erro de Pade é como o erro de Maclaurin, ele é 0 no ponto 0 entre [-1,1], como o Maclaurin.
# O Maclaurin tem esse erro por que o Maclaurin é expandido em torno do ponto 0.
# Mas mesmo o Pade sendo feito em base do Maclaurin, ele melhora um pouco o erro de Maclaurin com
# seu polinômio interpolador.
#
erroDePade       = abs( yAproximadoPorPade .- yInterPontosExato );
erroMaximoDePade = max( erroDePade )

% plot( x, y, '*' )
plot( x, y, '*', xInterPontos, yInterPontosExato, 'g', xInterPontos, yAproximadoPorPade, 'b' )








# Como determinar o m e n do método de Pade?
# Não é como saber, somente experimentando valores e verificando o resultado/erro.
#
# A método de Pade é útil para funções assintoticas como 1/x.
# Ele é o contrário da Serie de Chebyshev, que não funciona para funções assintóticas.
#
# Por exemplo, a função log( x ) é assintótica entre perto de 0 até 1. Mas podemos utilizar
# Chebyshev no função log( x ) no intervalo [1, 2] ou mais, onde a função é suave/bem comportada.
#









# Stop profiling. The collected data can later be retrieved and examined.
profile off

# Interactively explore hierarchical profiler output.
# profexplore()

# Show the profile resume, displaying per-function profiler results.
#
# profshow (data, n)
# If data is unspecified, profshow will use the current profile dataset.
# If n is unspecified it defaults to 20.
profshow( profile ("info"), 8 )




