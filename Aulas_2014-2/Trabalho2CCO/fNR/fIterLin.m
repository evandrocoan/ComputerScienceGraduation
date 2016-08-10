function [ raiz erro cont ] = fIterLin( a, b, erroMax)
	erro = erroMax + 1;
	cont = 0;
   xi = (a + b)/2;
	while ( ( erro > erroMax ) & ( cont < 100 ) )
	cont = cont + 1;
%inicio do núcleo do método (nunca mexa!)
		%calcula a média entre os pontos
		%x = 1/log(xi); NAO CONVERGE!!
      x = exp(1/xi);
		% erro = abs( x - xi);
      xi = x;
%fim do núcleo do método
		%cálculo do erro
		erro = abs( f(x) ); 
	end
	%retor
	raiz = x;

end
