function X = aula3_fgauss( A )
	
	
	n = size( A, 1)
	
	
	for k = 1 : n - 1
		
		for i = k + 1 : n
			
			aux = A( i, k ) / A( k, k )
			A( i, : ) = A( i, : ) - aux * A(k, :)
			
		end
		
	end
	
	A
	
end





