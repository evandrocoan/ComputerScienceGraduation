

clc
clear
close all

more off
format long
split_long_rows(0)

# Derivatives
# 
# ( u^n )' = n*u^(n-1)*u'   <-- Chain rule, the external derivative times the internal derivative.
# 
# To create the MacLaurin coefficients we need to derivate several times aways applying them on the
# 0 point, because that is the MacLaurin Series, the Taylor series applied on the zero point.
# 
# 1. Always to transform the domain from [a, b] to [-1, +1] using this formula:
#    x(t) = 0.5*( b-a )*t + 0.5*( b+a )
# 
# Now we got f( x(t) ) with t on [-1, 1]. This is useful/necessary to the Chebchev Series.
# And we always to apply the derivatives on this new domain [-1, +1] at the point 0, to deduce
# the nth derivate formula at the point 0 by backtracking.
# Como vamos padronizar o domínio [a, b] da aproximado para [-1, 1], pode-se fixar o x da série em 0.
# 

function x = fLog( x )
    
    x = log( x );
    
end

# Linear transformation to convert the [a, b] domain to [-1, 1] domain.
# We may call it as `t(x)`.
# 
function t = MaclaurinLinearTransformationDomainIn( x, a, b )
    
    t = ( 2*x - (b+a) ) / ( b-a );
    
end

# Linear transformation to convert the [-1, 1] domain to [a, b] domain.
# 
# We may call it as `x(t)`. On this way, we apply the the approximation
# methods to the `f(x(t))`, were `x` belongs to the Domain [a, b].
# 
function x = MaclaurinLinearTransformationDomainOut( t, a, b )
    
    x = ( (b-a)*t + (b+a) ) / 2;
    
end

# 
# Function: log( x )
# For the Domain [-1, 1]
# MaclaurinSeries( 0 ) = f( 0 ) + (f'( 0 )*z^1) / 1! + (f''( 0 )*z^2) / 2! + ... + (f^n'( 0 )*z^n) / n!
# 
# For the Domain [a, b]
# MaclaurinSeries( 0 ) = f( 0 )
#                        + (f'( 0 )*z^1) / 1!
#                        + (f''( 0 )*z^2) / 2!
#                        + ...
#                        + (f^n'( 0 )*z^n) / n!
# 
function coef = fMaclaurinForLog( n, a, b )
    
    # Here we applicate the first coefficient from the Maclaurin series on its Domain [-1, 1]
    # middle point 0. Before apply our function `fLog` we need to convert from the Domain [-1, 1]
    # to the original or correct Domain [a, b] for the function `fLog`.
    # This is the whole reason why we may apply the derivative functions on the 0 point.
    MaclaurinDomainPointZero = MaclaurinLinearTransformationDomainOut( 0, a, b )
    
    cache = ( b - a ) / ( b + a )
    coef( 1 ) = fLog( MaclaurinDomainPointZero )
    
    for i = 2 : n + 1
        
        coef( i ) =  (-1)^(i) * (cache^(i-1)) / (i-1);
        
    end
    
end

#{
n = 7
a = 1
b = 2
h = (b-a)/n

coefMaclaurin = fMaclaurinForLog( n, a, b )

x = a : h : b
y = fLog( x )

xInterPontos = a : h/20 : b
yInterPontos = fLog( xInterPontos )

tInterPontos = MaclaurinLinearTransformationDomainIn( xInterPontos, a, b )
yAproximado  = fPnPorBriotRunifi( n, coefMaclaurin, tInterPontos )

# Erro máximo deve ser calculado pelas formulas deve ser feito nos limites do nosso
# intervalo [-1,1], ou seja, em -1 ou em 1. 
# 
# O gráfico do erro mostra que o erro é 0 no ponto 0 (do intervalo [-1,1]), por que foi ali
# que fizemos a expansão da série de Maclaurin. O contrário da Sério de Tchebyshev, que possui
# um erro mais distribuído ao londo do intervalo (Comparar um Gráfico de Tchebyshev e Maclaurin).
erroDeMaclaurin       = abs( yAproximado .- yInterPontos )
erroMaximoDeMaclaurin = max( erroDeMaclaurin )

% plot( x, y, '*' )
% plot( x, y, '*', xInterPontos, yInterPontos, 'g', xInterPontos, yAproximado, 'b' )
#}


# Método de Tchebyshev
# 
# f(t) = log( .5*t + 1.5 ) in [-1, 1]
# 
# fTchebyshev( t ) = b0*T0( t ) + b1*T1( t ) + b2*T2( t ) + b3*T3( t ) + 
# b0 = 1/m \sum_j=1^m f( t(j) ), m = 10
# 
# t(j) = cos( ( 2*j - 1 ) * pi / 2*m ), j = 1 : m
# 

# Grau da Série de Tchebyshev
k = 3

# Grau de precisão da Integral Numérica, e também o número de nós de Tchebyshev
m = 10

function coef = TchebyshevForLog( k, m )
    
    # Calculamos os t(j)
    # Para encontrar os polinômios T1, T2, ..., consulte a tabela
    t( 1 ) = 0

    for j = 1 : m
        
        t( j ) = cos( ( 2*j - 1 ) * pi / 2*m );
        
    end


    # Agora calculamos o b0 no indice 1 do array b
    b(1) = 0

    for j = 1 : m
        
        b( 1 ) = b( 1 ) + fLog( t(j) );
        
    end

    b(1) = b(1) / m


    # Agora calculamos o b1 no indice 2 do array b
    b(2) = 0

    for j = 1 : m
        
        # x^1 = T1 = 1
        b( 2 ) = b( 2 ) + fLog( t(j) ) * t(j);
        
    end

    b(2) = b(2) * 2 / m


    # Agora calculamos o b2 no indice 3 do array b
    b(3) = 0

    for j = 1 : m
        
        # x^2 = T2 = 2*x^2 - 1
        b( 3 ) = b( 3 ) + fLog( t(j) ) * ( 2*t(j)^2 - 1 );
        
    end

    b(3) = b(3) * 2 / m


    # Agora calculamos o b3 no indice 4 do array b
    b(4) = 0

    for j = 1 : m
        
        # x^2 = T2 = 4*x^3-3x
        b( 4 ) = b( 4 ) + fLog( t(j) ) * ( 4*t(j)^3 - 3*t(j) );
        
    end

    b(4) = b(4) * 2 / m

end


# Gráfico de Tchebyshev
n = 7
a = 1
b = 2
h = (b-a)/n

x = a : h : b
y = fLog( x )

xInterPontos = a : h/20 : b
yInterPontos = fLog( xInterPontos )

tInterPontos = MaclaurinLinearTransformationDomainIn( xInterPontos, a, b )
b            = TchebyshevForLog( k, m )

# fTchebyshev( t ) = b0*T0( t ) + b1*T1( t ) + b2*T2( t ) + b3*T3( t ) + ...
function x = T1( x )
    x = x;
end
function x = T2( x )
    x = 2*x^2 - 1;
end
function x = T3( x )
    x = 4*x^3 - 3*x;
end
function x = T4( x )
    x = 8*x^4 - 8*x^2 + 1;
end
function x = T5( x )
    x = 16*x^5 - 20*x^3 + 5*x;
end

yAproximado = b(1) + b(2)*T1( tInterPontos ) + b(3)*T2( tInterPontos ) + b(4)*T3( tInterPontos )

# Erro máximo deve ser calculado pelas formulas deve ser feito nos limites do nosso
# intervalo [-1,1], ou seja, em -1 ou em 1. 
# 
# O gráfico do erro mostra que o erro é 0 no ponto 0 (do intervalo [-1,1]), por que foi ali
# que fizemos a expansão da série de Maclaurin. O contrário da Sério de Tchebyshev, que possui
# um erro mais distribuído ao londo do intervalo (Comparar um Gráfico de Tchebyshev e Maclaurin).
erroDeMaclaurin       = abs( yAproximado .- yInterPontos )
erroMaximoDeMaclaurin = max( erroDeMaclaurin )

% plot( x, y, '*' )
% plot( x, y, '*', xInterPontos, yInterPontos, 'g', xInterPontos, yAproximado, 'b' )




