function Sn = fSn(a,b,n)
	h=(b-a)/n;
	x=a:h:b;
	y=g(x);
	Snpar=0;
	for i=2:2:n
		Snpar+=y(i);
	end%for
	Snimpar=0;
	for i=3:2:n-1
		Snimpar+=y(i);
	end%for
	Sn = (h/3)*(y(1)+4*Snpar+2*Snimpar+y(n+1));
end%function