function y = integralT2(a, b)
	yb = b + (b^2)/2 + (b^3)/3 - (b^4)/4 + (b^5)/5 + (b^6)/6 - (b^7)/7
	ya = a + (a^2)/2 + (a^3)/3 - (a^4)/4 + (a^5)/5 + (a^6)/6 - (a^7)/7
	y = yb - ya;
end
