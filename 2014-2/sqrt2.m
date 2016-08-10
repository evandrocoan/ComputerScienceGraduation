function x = sqrt2( c )
	if ( c > 1 )
		x = c / 2
	else
		x = c * 2
	end
	erro = 1;
	contador = 0;
	while( erro > 1.e-15 && contador < 100 )
		contador = contador + 1
		xi = x
		x = .5 * ( x * x + c  ) / x
		erro = abs( x - xi)
	end
end
