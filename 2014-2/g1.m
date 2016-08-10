function y = g1( m, T, V, x )
	y = 0;
	for k = 1 : m
		y = y + ( sin( x(1) + x(2) * T(k) ) - V(k) ) * ( cos( x(1) + x(2) * T(k) ) );
	end
end
