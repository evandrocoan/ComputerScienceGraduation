function yP = fPnGregNew( n, x, y, difDiv, xP )
	yP = y(1);
	%n grau do polinomio
	for k = 1 : n
		produtorio = 1;
		for j = 1 : k
			produtorio = produtorio * ( xP - x(j) );
		end
		yP = yP + difDiv(1, k) * produtorio;
	end
end
