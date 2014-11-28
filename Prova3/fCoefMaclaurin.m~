function mac = fMaclaurin(n)

	mac(1) = 1; %n = 0
	neg = 1;
	for i = 2 : n+1 %n = 1
		g = i-1;
		if(mod(g,2) == 1);
			mac(i) = 0;
		else
			neg = neg*(-1);
			mac(i) = (neg)/(factorial(g));
		end
	end
	
	%for i = 1 : 1 : n
	%	mac(2*i+1) = ((-1)^(i))/(factorial(2*(i)))
	%end
	
end
