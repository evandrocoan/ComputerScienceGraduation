function  x  = newtonRaphson( a, b, erroMax)
	erro = erroMax + 1;
	cont = 0;
   xi = (a + b)/2;
	while ( ( erro > erroMax ) && ( cont < 100 ) )
	cont = cont + 1;
%inicio do n?cleo do m?todo (nunca mexa!)
		%calcula a m?dia entre os pontos
	   dX = - f(xi)/df(xi);
      x = xi + dX;
		% erro = abs(dX);
      xi = x;
%fim do n?cleo do m?todo
		%c?lculo do erro
		erro = abs( f(x) ); 
	end
end

