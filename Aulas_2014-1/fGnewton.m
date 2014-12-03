function yiGN = fGnewton(N, x, y, difdiv, xi)

	nx = length(xi);
	for t=1:nx
		soma = y(1);
		for k=1:N
			prod=1;
			for j=0+1:k-1+1
				prod = prod * (xi(t) - x(j));
			end #for
			soma = soma + difdiv(1,k)*prod;
		end #for
		yiGN(t) = soma;
	end#for

end #function