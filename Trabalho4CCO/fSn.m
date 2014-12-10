function Sn=fSn(n,a,b)
	h=(b-a)/n;
	x=a:h:b;
	y=fh(x);
	somadepares=0;
	for i=2:2:n
	somadepares=somadepares+y(i);
	end
	somadeimpares=0;
	for i=3:2:n-1
	somadeimpares=somadeimpares+y(i);
	end
	% Sn = h/3*(y(1)+y(n+1)+4*(somadepares)+2*(somadeimpares))
	Sn = h/3*(y(1)+y(n+1)+4*(somadepares)+2*(somadeimpares));
end
