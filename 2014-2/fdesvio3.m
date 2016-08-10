function D = fdesvio3( m, y, yAjustado )
	D = 0
	for i = 1 : m 
		D = D + ( y( i ) - yAjustado( i ) ) ^ 2;
	end
	D = D / m;
end