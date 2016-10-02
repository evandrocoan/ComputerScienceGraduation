function Tn = fTn(a,b,n)
	h=(b-a)/n;
	x=a:h:b;
	y=g(x);
	Tn=0;
	for i=2:n
		Tn+=y(i);
	end%for
	Tn = (0.5*h)*(y(1)+2*Tn+y(n+1));
end%function