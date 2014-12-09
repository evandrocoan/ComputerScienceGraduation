function resMax = fResiduo2(n, Aaux, x)
	for k = 1 : columns( x )
		% Cálculo do resíduo
		for i = 1: n
		  aux = 0;
		  for j = 1: n
			 aux = aux + Aaux(i,j) * x(j, k);
		  end
		  res(i, k) = abs(aux - Aaux(i, n + 1));
		end
		resMax(k) = max( res(:, k) );
	end
end
