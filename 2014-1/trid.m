function x = trid(n,t,r,d,b)
	for i=2:n
		aux = t(i)/r(i-1);
		r(i)=r(i)-aux*d(i-1);
		b(i)=b(i)-aux*b(i-1);
	end
	x(n)=b(n)/r(n);
	for i=n-1:-1:1
		x(i)=(-d(i)*x(i+1)+b(i))/r(i);
	end
end