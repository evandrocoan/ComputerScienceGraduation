function x = reciproco( c ) 
	if ( c > 1 )
		x = + eps
	else
		x = - eps
	end
	erro = 1;
	contador = 0;
	while( erro > 1.e-8 && contador < 100 )
		contador = contador + 1
		xi = x
		x = x * ( 2 - x * c )
		erro = abs( x - xi)
	end
end
