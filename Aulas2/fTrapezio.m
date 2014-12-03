function Tn = fTrapezio( n, a, b )
	somatorio = 0;
	h = ( b - a ) / n;
	x = a : h : b;
	for i = 2 : n
		somatorio += g( x(i) );
	end
	somatorio *= 2;
	Tn = .5 * h * ( g( x(1) ) + somatorio + g( x( n + 1 ) ) );
end
