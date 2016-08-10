function Tn=fTn(n,a,b,x)
	h=(b-a)/n;
	x=a:h:b;
	y=fh(x);
	soma=0;
	for i=2:n
	soma=soma+y(i);
	end
	%Tn=0.5*h(y(1) + 2(somatorio de termos intermediarios y(i), com i de 2 a n)+ y(n+1))
	Tn=0.5*h*(y(1)+2*soma+y(n+1));
end
