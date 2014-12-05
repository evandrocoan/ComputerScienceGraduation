function x = fNewton( m, T, V )
	xInicial = [ .5, .5 ];
	contador = 0;
	erro = 1;
	while erro > 1.e-6 && contador < 20
		dx = [ 1.e-4, 1.e-4 ];
		contador = contador + 1;
		A(1,1) = ( g1( m, T, V, [ xInicial(1) + dx(1), xInicial(2) ] )  - g1( m, T, V, xInicial) )	/ dx(1);
		A(1,2) = ( g1( m, T, V, [ xInicial(1), xInicial(2) + dx(2) ] )  - g1( m, T, V, xInicial) )	/ dx(2);
		A(1,3) = -g1( m, T, V, xInicial );

		A(2,1) = ( g2( m, T, V, [ xInicial(1) + dx(1), xInicial(2) ] )  - g2( m, T, V, xInicial) )	/ dx(1);
		A(2,2) = ( g2( m, T, V, [ xInicial(1), xInicial(2) + dx(2) ] )  - g2( m, T, V, xInicial) )	/ dx(2);
		A(2,3) = -g2( m, T, V, xInicial );

		dx = fGauss(2, A);
		x = xInicial + dx;
		xInicial = x;
		erro = max( abs( dx ) );
	end
end
