
clear
clc
format long

function result = f1( x )
    
    result = e^x( 1 ) + x( 2 ) - 1;
    
end

function result = f2( x )
    
    result = x( 1 )^2 + x( 2 )^2 - 4;
    
end


xi = [ 1, -1 ]


# Dado o sistema de equações não lineares:
#
# f1( x( 1 ), x( 2 ) ) = e^x( 1 ) + x( 2 ) - 1 = 0
# f2( x( 1 ), x( 2 ) ) = x( 1 )^2 + x( 2 )^2 - 4 = 0 
#
x              = fNewtonSistemasNaoLineares( xi )
residuo_maximo = max( abs( f1( x ) ), abs( f2( x ) ) )










