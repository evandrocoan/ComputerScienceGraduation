function A = pivotacao(k,n,A)

	Amax = abs(A(k,k));
	imax = k;

	for i=1+k:n

		if abs(A(i,k))>Amax

			Amax = abs(A(i,k));
			imax = i;

		endif
		
	endfor

	for j=1:n+1

		aux = A(k,j);
		A(k,j) = A(imax,j);
		A(imax,j) = aux;

	endfor

endfunction
