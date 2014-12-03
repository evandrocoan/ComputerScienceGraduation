% n, a -> conjunto de coeficientes do polinomio
% xi -> ponto inicial para o cálculo
function [ x M erro cont ] = fNRPoliM( n, a, xi, erroMax) 
	erro = erroMax + 1;
	cont = 0;
	while ( ( erro > erroMax ) && ( cont < 200 ) )
	cont = cont + 1;
%inicio do núcleo do método (nunca mexa!)
		%fPn -> função calculo o valor numérico de Pn( x = xi ), para qualquer polinômio de entrada (n e a)
		[R M] = fRestos(n,a,xi);
		%M = 1; -> desabilita a correção das multiplicidades
		dX = -R(M)/(R(M+1)*M);%dX do newton raphson para raizes multiplas
      x = xi + dX;
		% erro = abs(dX);
      xi = x;
%fim do núcleo do método
		erro = abs( dX ); %cálculo do erro
	end

end
