% n, a -> conjunto de coeficientes do polinomio
% xi -> ponto inicial para o cálculo
function [ x erro cont ] = fNRPoli( n, a, xi, erroMax) 
	xi = 1.1;
	erro = erroMax + 1;
	cont = 0;
	while ( ( erro > erroMax ) & ( cont < 1 ) )
	cont = cont + 1
%inicio do núcleo do método (nunca mexa!)
		%fPn -> função calculo o valor numérico de Pn( x = xi ), para qualquer polinômio de entrada (n e a)
		b = fDivBrio( n  , a, xi ); %b = polinomio quociente (de grau n-1) e b(n+1) = resto
		c = fDivBrio( n-1, b, xi ); %c = polinomio quociente (de grau n-2) e b(n) = resto
		resto1 = b( n+1 ) %resto1 é igual ao valor da função polinomial Pn(x = xi)
		resto2 = c( n ) % é a derivada da função polinomial Pn(x = xi) no ponto x = xi
		dX = -resto1 / resto2 %dX do newton raphson
      x = xi + dX
		% erro = abs(dX);
      xi = x;
%fim do núcleo do método
		erro = abs( dX ) %cálculo do erro
	end

end
