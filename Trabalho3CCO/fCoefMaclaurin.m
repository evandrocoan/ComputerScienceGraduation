function cMaclaurin = fCoefMaclaurin( nMaclaurin, a, b )
	%tB = 0; ( Maclaurin )
	j = 1;
	cMaclaurin(1) = ( 0.5 * (b - a) * 0 + .5 * (b + a) )^(1/2);
	for i = 2 : nMaclaurin + 1
		k = 2 * i - 3;
		aux = k * j;
		% CM(i+1) = f'(x(t=0))/i!
		cMaclaurin(i) = (((-1)^i) * ((aux)/(2^i)) * ((.5*(b+a))^(-(2*i-1)/2)) * (.5*(b-a))^i) / factorial(i);
		j = aux;
	end
end
