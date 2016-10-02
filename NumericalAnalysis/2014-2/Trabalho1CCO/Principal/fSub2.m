function [x o] = fSub2( n, A, b )
	o = 0;
	c(1) = b(1) / A(1, 1);
	for i = 2 : n
		aux = 0;
		for r = 1: i - 1
			aux = aux + A(i, r) * c(r);
			o = o + 2;
		end
		c(i) = ( b(i) - aux ) / A(i, i);
		o = o + 2;
	end
	x(n) = c(n);	
	for i = n - 1 : -1: 1
		aux = 0;		
		for r = i + 1: n 
			aux = aux + A(i, r) * x(r);
			o = o + 2;
		end
		x(i) = c(i) - aux;
		o = o + 1;
	end
	o;
end
