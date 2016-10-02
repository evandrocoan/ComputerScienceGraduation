function [A b] = pivotaLU(N,A,b,k)
	amax = abs(A(k,k));
	imax = k;
	for i=k+1:N
		if abs(A(i,k)) > amax
			amax = abs(A(i,k));
			imax = i;
		end %if
	end %for
	aux = A(k,:); 			bAux = b(k);
	A(k,:) = A(imax, :); 	b(k) = b(imax);
	A(imax,:) = aux;		b(imax) = bAux;
end %function