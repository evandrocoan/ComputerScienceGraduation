function [x, M] = fNPolinomios(n, a, xi, restoLimiteEscolhido, numeroMaximoDeIteracoes, toleranciaMinima)
    x = xi;
	iteracoes = 0;
	criterio = 1;
	while(criterio > toleranciaMinima && iteracoes < numeroMaximoDeIteracoes)
		iteracoes = iteracoes + 1;
		R = fRestos(n, a, xi);
		M = fMultiplicidade(R, restoLimiteEscolhido);
		x = xi + (-R(M) / (M*R(M+1)));
		criterio = abs(x-xi) + abs(R(1));
		xi = x;
	end
end
		%P3 = (xi^3 - 3*xi^2 + 3*xi - 1) 
		%dP3 = (3*xi^2 - 6*xi + 3)
		% x = xi - (xi^3 - 3*xi^2 + 3*xi - 1) / (3*xi^2 - 6*xi + 3)
