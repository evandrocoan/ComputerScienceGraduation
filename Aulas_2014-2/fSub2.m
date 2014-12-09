function x = fSub2( n, A, b )
	for k = 1 : columns(b)
		c(1, k) = b(1, k) / A(1, 1);
		for i = 2 : n
			aux = 0;
			for r = 1: i - 1
				aux = aux + A(i, r) * c(r, k);
			end
			c(i, k) = ( b(i, k) - aux ) / A(i, i);
		end
		x(n, k) = c(n, k);	
		for i = n - 1 : -1: 1
			aux = 0;		
			for r = i + 1: n 
				aux = aux + A(i, r) * x(r, k);
			end
			x(i, k) = c(i, k) - aux;
		end
	end
end
