function x = retrosubs(N,A)
x(N) = A(N, N+1)/A(N,N);
for i=N-1:-1:1
	soma =0;
	for j=i+1:N
		soma = soma + A(i,j)*x(j);
	end #for j
	x(i) = (A(i, N+1)- soma)/A(i,i);
end #for i
end #function