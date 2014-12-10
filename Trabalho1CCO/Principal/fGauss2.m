function o = fGauss2(n, A)
	o = 0;
	
	[A o1] = fEscalona2(n, A);

	[x o2] = fRetroSubs2(n, A);
	
	o = o1 + o2;
end
