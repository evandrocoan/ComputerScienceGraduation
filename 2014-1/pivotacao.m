function A = pivotacao(N,A,k)
	amax = abs(A(k,k));
	imax = k;
	for i=k+1:N
		if abs(A(i,k)) > amax
			amax = abs(A(i,k));
			imax = i;
		end %if
	end %for
	aux = A(k,:);
 	A(k,:) = A(imax, :);
	A(imax,:) = aux;
end %function