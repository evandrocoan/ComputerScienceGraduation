

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
function t = MaclaurinLinearTransformationDomainIn( x, a, b )
    
    t = ( 2*x - (b+a) ) / ( b-a );
    
end

# Linear transformation to convert the [-1, 1] domain to [a, b] domain.
function x = MaclaurinLinearTransformationDomainOut( t, a, b )
    
    x = ( (b-a)*t + (b+a) ) / 2;
    
end

# 
# For the Domain [-1, 1]
# MaclaurinSeries( 0 ) = f( 0 ) + (f'( 0 )*q^1) / 1! + (f''( 0 )*q^2) / 2! + ... + (f^n'( 0 )*q^n) / n!
# 
# For the Domain [a, b]
# MaclaurinSeries( 0 ) = f( 0 )
#                        + (f'( 0 )*q^1) / 1!
#                        + (f''( 0 )*q^2) / 2!
#                        + ...
#                        + (f^n'( 0 )*q^n) / n!
# 
function coef = fMaclaurinForLog( n, a, b )
    
    # Here we applicate the first coefficient from the Maclaurin series on its Domain [-1, 1]
    # middle point 0. Before apply our function `fLog` we need to convert from the Domain [-1, 1]
    # to the original or correct Domain [a, b] for the function `fLog`.
    # This is the whole reason why we may apply the derivative functions on the 0 point.
    MaclaurinDomainPointZero = MaclaurinLinearTransformationDomainOut( 0, a, b )
    OriginalDomainPointZero  = MaclaurinLinearTransformationDomainIn ( 1.5, a, b )
    
    their = ( b - a ) / ( b + a )
    
    coef( 1 ) = fLog( MaclaurinDomainPointZero )
    
    for i = 1 : n
        
        coef( i + 1 ) =  (-1)^(i+1) * ( their^( i ) ) / ( i );
        
    end
    
end

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

erroDeMaclaurin       = abs( yAproximado .- yInterPontos )
erroMaximoDeMaclaurin = max( erroDeMaclaurin )

% plot( x, y, '*' )
plot( x, y, '*', xInterPontos, yInterPontos, 'g', xInterPontos, yAproximado, 'b' )



