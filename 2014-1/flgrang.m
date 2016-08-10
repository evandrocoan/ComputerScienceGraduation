function yi = flgrang(n, x, y, xi)
	nx = length(xi);
	for k=1:nx
		soma = 0;
		for i=1:n+1
			prod = 1;
			for j=1:n+1
				if j != i
					prod = prod*((xi(k)-x(j))/(x(i)-x(j)));
				end
			end
			soma=soma+y(i)*prod;
		end
		yi(k)=soma;
	end
end