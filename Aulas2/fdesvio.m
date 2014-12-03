function D = fdesvio(m, y, yAjustado )
	D = 0;
	for i = 1 : m
		D = D + ( yAjustado(i) - y(i) ) ^ 2;
	end
	D = D / m;
end
