function A = fEscalona(n, A)
	% Complexidade O(n³)
	for k = 1: n - 1
		k
		A = fPermutacao(k, n, A )
	  	for i = k + 1: n
	    	aux = A(i,k) / A(k,k);
	    	A(i,k) = 0;
	    	for j = k + 1: n+1
	      	% Li <= Li - x*Lk
	      	A(i,j) = A(i,j) - aux * A(k,j); 
	    	end
	  	end
		A
	end
end
