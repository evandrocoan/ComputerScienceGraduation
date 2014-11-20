% dado xP calcula yP = Pn(x = xP) por Briot-Ruffini
function yP = fPnBrio( n, coef, xP ) 
	b(n+1) = coef(n + 1);
	for i = n : -1 : 1
		b(i) = coef(i) + xP * b(i + 1);
	end
	yP = b(1);
end
