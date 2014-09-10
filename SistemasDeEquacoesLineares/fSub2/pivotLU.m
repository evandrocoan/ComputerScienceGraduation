function [A b] = pivotLU(k, n, A, b);
	%k indica a linha a ser melhorada (eliminar zeros, tornar diagonal maior, ...)
	%* Encontrar o maior valor da primeira coluna da matriz, a partir da linha k fornecida
	Amax = abs( A(k,k) );
	Imax = k ;
	for i = k + 1 : n
		if( abs(A(i,k) ) > Amax )
			Amax  = abs( A(i, k) );
			Imax = i;
		end
	end
	if( k != Imax )
		Laux = A(k, :); 			Baux         = b(k, :);
		A(k, : ) = A( Imax, : ); 	b(k, :)      = b(Imax, :);
		A(Imax, : ) = Laux; 		b(Imax, :)   = Baux;
 	end
end
