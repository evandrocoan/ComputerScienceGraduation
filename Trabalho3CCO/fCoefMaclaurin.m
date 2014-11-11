function cMac = fCoefMaclaurin( nMaclaurin, a, b )
	%tB = 0; ( Maclaurin )
	den = 1;
	k = 1;
	cMac(1) = (.5 * (b + a))^(1/2);

	cMac(2) = (0.5) * ((0.5*(b+a))^(-(0.5))) * ((0.5*(b-a)));
	
	for i = 3 : nMaclaurin + 1
		iO = (i-1);
		k = 2 * iO - 3;
		
		den = k * den;
		div = (2^iO);
		
		mult = (den)/div;
		
		fi = ((-1)^i) * (mult) * ((0.5*(b+a))^(-(0.5*((2*iO)-1)))) * ((0.5*(b-a))^(iO));
		fat = factorial(iO);
		
		cMac(i) = fi/fat;
	end
end
