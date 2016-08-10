function y = integralT(a, b)
	yb = b + (b^2)/2 + (b^3)/3 - (b^4)/4 + (b^5)/5 - (b^6)/6;
	ya = a + (a^2)/2 + (a^3)/3 - (a^4)/4 + (a^5)/5 - (a^6)/6;
	y = yb - ya;
end
