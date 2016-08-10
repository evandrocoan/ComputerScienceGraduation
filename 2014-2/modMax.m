function alpha = modMax( n, a )	
	alpha = 1 + max( abs( a( 2: n + 1 ) )  ) / abs( a(1) );
end
