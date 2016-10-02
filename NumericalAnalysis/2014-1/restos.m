function [M R] = restos(n, a, xi)
	g = n;
	b(1) = a(1);
	for i = 2 : n+1
		b(i) = a(i) + xi*b(i-1);
	end #for i
	R(1) = b(n+1);
	n = n-1;
	a = b;
	M = 1;
	for iR = 2 : g+1
		b(1) = a(1);
		for i = 2 : n+1
			b(i) = a(i) + xi*b(i-1);
		end #for i
		R(iR) = b(n+1);
		n = n-1;
		a = b;
		if (abs(R(iR)) < 1.e-1 && abs(R(iR-1)) < 1.e-1)
			M = iR;
		end #if
	end #for iR
end #function