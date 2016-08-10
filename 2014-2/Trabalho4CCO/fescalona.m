function A = fescalona(n,A)

	for k=1:n-1

		A = pivotacao(k,n,A);

		for i=k+1:n

			aux = A(i,k)/A(k,k);

			for j = k+1:n+1

				A(i,j) = A(i,j)-(aux)*A(k,j);

			endfor

			A(i,k) = 0.;

		endfor

	endfor

endfunction
