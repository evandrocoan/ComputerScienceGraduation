function [A o] = fEscalona2(n, A)
	o = 0;
	for k = 1: n - 1
		k;
		A = fPermutacao(k, n, A );
	  	for i = k + 1: n
		    	aux = A(i,k) / A(k,k);
		    	o = o+1;
		    	A(i,k) = 0;
		    	for j = k + 1: n+1
				% Li <= Li - x*Lk
				A(i,j) = A(i,j) - aux * A(k,j);
				o = o+2;
		    	end
	  	end
	end
end
