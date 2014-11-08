function [ x erro cont ] = fNewRapNumerico( a, b, erroMax)
	erro = erroMax + 1;
	cont = 0;
   xi = (a + b)/2;
	dX = 1.e-6; % define o valor inicial do incremento dX, para calculo da derivada numerica
	while ( ( erro > erroMax ) & ( cont < 100 ) )
	cont = cont + 1;
%inicio do núcleo do método (nunca mexa!)
		%calcula a média entre os pontos
		dfNumerico =  ( f( xi + dX ) - f(xi) ) / dX; 
	   dX = - f(xi)/dfNumerico;
      x = xi + dX;
		% erro = abs(dX);
      xi = x;
%fim do núcleo do método
		%cálculo do erro
		erro = abs( f(x) ); 
	end
end
