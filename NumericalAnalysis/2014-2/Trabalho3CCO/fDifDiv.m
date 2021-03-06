function difDiv = fDifDiv( n, x, y )
	nP = n + 1;		%número de pontos
	k = 1;		%primeira ordem das diferenças divididas
	for i = 1 : nP - k
		difDiv(i, k) = (y(i+1)-y(i))/(x(i+1)-x(i));
	end
	for k = 2 : n
		for i = 1 : nP - k
			difDiv(i,k) = (difDiv(i+1, k-1)-difDiv(i, k-1))/(x(i+k)-x(i));
		end
	end
end
