

clc
clear
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
# 

function coef = fMaclaurinForLog( n )
    
    coef( 1 ) = log( 1.5 );
    
    for i  = 1 : n
        
        coef( i + 1 ) = ((-1)^(i-1)) / ( i*3^i );
        
    end
    
end


n             = 7
coefMaclaurin = fMaclaurinForLog( n )

a = 1;
b = 2;
h = (b-a)/n;

x = a : h : b;
y = log( x );

xInterPontos = a : h/20 : b;
tInterPontos = (2.*xInterPontos .- (b+a)) / (b-a);

yAproximado  = fPnPorBriotRunifi( n, coefMaclaurin, tInterPontos )
yInterPontos = log( tInterPontos );

erroDeMaclaurin       = abs( yAproximado .- yInterPontos )
erroMaximoDeMaclaurin = max( erroDeMaclaurin )

plot( x, y, '*' )
% plot( x, y, '*', xInterPontos, yAproximado, 'g', xInterPontos, yInterPontos, 'b' )



