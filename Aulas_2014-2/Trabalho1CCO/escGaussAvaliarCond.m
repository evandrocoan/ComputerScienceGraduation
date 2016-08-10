%função que retorna 1 se o sistema é bem condicionado, 0 caso contrário
function r = escalonamentoGauss( A )
	n = columns(A) - 1;
	for k = 1: n - 1
	  for i = k + 1: n
		aux = A(i,k) / A(k,k);
		A(i,k) = 0;
		for j = k + 1: n+1
		  % Li <= Li - x*Lk
		  A(i,j) = A(i,j) - aux * A(k,j); 
		end
	  end
	end
	r = 1;
	if A(n, n - 1 ) ==  0  & A( n, n ) == 0
		r = 0;
	end
end