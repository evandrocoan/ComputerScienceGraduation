function [ raiz erro cont ] = fFalsaPos( a, b, erroMax)
	erro = erroMax + 1;
	cont = 0;
	while ( ( erro > erroMax ) & ( cont < 100 ) )
	cont = cont + 1;
%inicio do núcleo do método (nunca mexa!)
		%calcula a média entre os pontos
		x = (a*f(b) - b * f(a))/(f(b) - f(a));
		%verifica o intervalo da raíz
		if ( f(a) * f(x) <= 0 )
			%a = a, comentário redundante
			b = x;
		else
			a = x;
			%b = b, comentário redundante
		end	
%fim do núcleo do método
		%cálculo do erro
		erro = abs( f(x) );
	end
	%retor
	raiz = x;

end
