function x = fcrout(N, A, b,A_original)
k = 1;
#li1=ai1 para todo i
#pivotação
[A b] = pivotaLU(N,A,b,k);
#u1j = a1j/lii para todo j
for j=2 : N 
	A(1,j) = A(1,j)/A(1,1);
end #for
for k=2 : N-1
	for i = k : N
		somatorio = 0;
		for r = 1 : k-1
			somatorio = somatorio+A(i,r)*A(r,k);
		end #for r
		A(i,k) = A(i,k)-somatorio;
	end #for i
	[A b] = pivotaLU(N,A,b,k);
	for j = k+1 : N
		somatorio = 0;
		for r = 1 : k-1
			somatorio = somatorio+A(k,r)*A(r,j);
		end #for r
		A(k,j) = (A(k,j)-somatorio)/A(k,k);
	end #for j
end #for k
k = N;
i = N;
somatorio = 0;
for r = 1 : k-1
	somatorio = somatorio+A(i,r)*A(r,k);
end #for r
A(i,k) = A(i,k)-somatorio;
A
malcond(N,A,A_original)
x = subsDupla(N,A,b);
end #function
