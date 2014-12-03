function x = subsDupla(N,A,b)
#L*c = b
c(1) = b(1)/A(1,1);
for i = 2 : N
	somatorio = 0;
	for j = 1 : i-1
		somatorio = somatorio+A(i,j)*c(j);
	end #for j
	c(i) = (b(i)-somatorio)/A(i,i);
end #for i
c
#U*x = c
x(N) = c(N);
for i = N-1 : -1 : 1
	somatorio = 0;
	for j = i+1 : N
		somatorio = somatorio+A(i,j)*x(j);
	end #for j	
	x(i) = c(i) - somatorio;
end #segundo for i
x
end #function