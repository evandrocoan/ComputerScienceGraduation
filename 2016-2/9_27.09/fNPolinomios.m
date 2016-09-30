function [x, M] = fNPolinomios(n, a, xi)
    x = xi;
    tol = 1e-15;
	iteracoes = 0;
	criterio = 1;
	while(criterio > tol && iteracoes < 100)
		iteracoes = iteracoes + 1
		R = fRestos(n, a, xi)
		M = fMultiplicidade(R)
		x = xi + (-R(M) / (M*R(M+1)))
		criterio = abs(x-xi) + abs(R(1))
		xi = x;
	end
end
		%P3 = (xi^3 - 3*xi^2 + 3*xi - 1) 
		%dP3 = (3*xi^2 - 6*xi + 3)
		% x = xi - (xi^3 - 3*xi^2 + 3*xi - 1) / (3*xi^2 - 6*xi + 3)