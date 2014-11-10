function cMac = fCoefMaclaurin( nMaclaurin, a, b )
	%tB = 0; ( Maclaurin )
	aux = 1;
	k = 1;
	cMac(1) = ( 0.5 * (b - a) * 0 + .5 * (b + a) )^(1/2);
	
	fi = ((aux)/(2)) * ((0.5*(b+a))^(-(0.5*((2)-1)))) * ((0.5*(b-a)));
	cMac(2) = fi;
	
	for i = 3 : nMaclaurin + 1
		k = 2 * i - 3;
		iO = (i-1);
		aux = k * aux;
		% CM(i+1) = fi(x(t=0))/i!
		fi = ((-1)^i) * ((aux)/(2^iO)) * ((0.5*(b+a))^(-(0.5*((2*iO)-1)))) * ((0.5*(b-a))^(iO));
		cMac(i) = fi/factorial(i);
	end
end
