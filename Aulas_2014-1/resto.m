function y = resto(N, c, x)
Nx = length(x);
for k=1:Nx
	b(N+1) = c(N+1);
	for i=N:-1:1
		b(i) = c(i) + b(i+1)*x(k);
	end #for
	y(k) = b(1);
end #for
end #function