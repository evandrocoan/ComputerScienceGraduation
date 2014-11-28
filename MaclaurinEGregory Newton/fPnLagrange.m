%
function yP = fPnLagrange( n, x, y, xP )
	yP = 0;
	%somatório
	for i = 1 : n + 1
		%produtorio
		produtorio = 1;
		for j = 1 : n + 1
			if( i != j )
				produtorio *= (xP - x(j))/(x(i)-x(j));
			end
		end % final do produtório
		yP += y(i) * produtorio;
	end
end
