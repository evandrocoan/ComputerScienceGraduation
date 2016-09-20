clear
clc
format long

split_long_rows(0)
#output_precision(50)
#output_max_field_width(0)

x = [ 1.76: 0.001 : 1.77 ];
y = x .* log( x ) .- 1;

%plot( x, y )
%grid

xi         = 1.5
tolerancia = 1e-15
#{
# método da iteração linear
xAprox = fMetodoDaIteracaoLinear( xi,     tolerancia     );
xExato = fMetodoDaIteracaoLinear( xAprox, tolerancia ^ 2 );


xAprox ;
xExato ;

# erroDoX é menor que 1e-14
erroDoX = abs( xAprox - xExato );

#}
# método de newton
xAprox = fMetodoDeNewton( xi,     tolerancia     );
xExato = fMetodoDeNewton( xAprox, tolerancia ^ 2 );


xAprox 
xExato 

# erroDoX é menor que 1e-14
erroDoX = abs( xAprox - xExato )



printf( 'here' )








