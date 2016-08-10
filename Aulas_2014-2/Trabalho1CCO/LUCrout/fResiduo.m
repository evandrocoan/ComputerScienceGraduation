function resMax = fResiduo(n, Aaux,x)
	% Cálculo do resíduo
	for i = 1: n
	  aux = 0;
	  for j = 1: n
	    aux = aux + Aaux(i,j) * x(j);
	  end
	  res(i) = abs(aux - Aaux(i, n + 1));
	end
	resMax = max( res );

end
