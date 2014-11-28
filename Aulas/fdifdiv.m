function difdiv = fdifdiv(x,y,N)
	for i=1:N
		difdiv(i,1) = (y(i+1) - y(i)) / (x(i+1) - x(i));		
	end #for
	for k=2:N
		for i=1:N+1-k
			difdiv(i,k) = (difdiv(i+1,k-1) - difdiv(i,k-1)) / (x(i+k) - x(i));		
		end #for
	end #for
end #function