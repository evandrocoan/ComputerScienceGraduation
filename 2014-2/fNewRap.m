function [ x erro cont ] = fNewRap( a, b, erroMax)
	erro = erroMax + 1;
	cont = 0;
   xi = (a + b)/2;
	while ( ( erro > erroMax ) & ( cont < 100 ) )
	cont = cont + 1
%inicio do núcleo do método (nunca mexa!)
		%calcula a média entre os pontos
	   dX = - f(xi)/df(xi)
      x = xi + dX;
		% erro = abs(dX);
      xi = x;
%fim do núcleo do método
		%cálculo do erro
		erro = abs( f(x) ); 
	end

end
