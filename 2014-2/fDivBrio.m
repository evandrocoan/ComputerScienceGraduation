%n = grau do polinômio
%a = polinômio
%xi = valor inicial para trabalhar
%calcula o valor numérico do polinomio do ponto
function b = fDivBrio( n, a , xi ) 
	b(1) = a(1);
	for i = 2 : n + 1
		b(i) = a(i) + xi * b( i - 1 );
	end 
end
