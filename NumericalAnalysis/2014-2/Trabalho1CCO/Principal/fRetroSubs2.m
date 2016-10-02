function [x o] = fRetroSubs2(n, A)
	o = 0;
	% Resolução das variáveis	
	if( abs( A(n, n) ) > 1.e-14 )
		x(n) = A(n,n + 1) / A(n,n);
		o = o+2;
	else
		if( abs( A(n, n + 1) ) < 1.e-14 )		
			x(n) = 1; % Valor particular de uma das infinitas soluções para o sistema indefinido
		else
			printf( "\nSistema impossivel\n\n" )
			exit( 0 );
		end
	end
	% "-1" significa decremento	
	for i = n - 1: -1: 1 
	  aux = 0;
	  for j = i + 1: n
	    aux = aux + A(i,j) * x(j);
	    o = o+2;
	  end
	  x(i) = ( A(i, n + 1) - aux ) / A(i,i);
	  o = o+2;
	end
end
