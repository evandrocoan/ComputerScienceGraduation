function coef = fInterCoef( n, x, y )
	nP = n + 1;
	for i = 1 : nP
		P(i, 1) = 1;
		P(i, 2) = x(i);
		for j = 3 : nP
			P(i, j) = P( i, j-1) * x(i);
		end
		P(i, nP + 1) = y(i);
	end	
	P;
	coef = fGauss(nP, P);
end
