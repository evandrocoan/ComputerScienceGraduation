function A = escalonamento(N,A)
for k=1:N-1
	A = pivotacao(N,A,k);
	for i=k+1:N
		aux = A(i,k)/A(k,k);
		for j=k+1:N+1
			A(i,j)= A(i,j)- aux * A(k,j);
		end #for j
		A(i,k)= 0;
	end #for i
end #for k
end #function