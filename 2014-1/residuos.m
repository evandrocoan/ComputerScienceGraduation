function rmax=residuos(N,A,x)
rmax = 0;
	for i=1:N
		soma=0;
		for j=1:N
			soma = soma + A(i,j) * x(j);
		end %for j
		r(i) = abs(soma-A(i,N+1)); 
		
		if r(i) > rmax
			rmax = r(i);
		end %if
	end %for i
end %function