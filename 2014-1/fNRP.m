function [x erro cont] = fNRP(n,a,xi)
	erro = 1.0;
	cont = 0.0;
	while( erro > 1.e-16 & cont < 100 )
		cont = cont + 1;
		b(1) = a(1);
		for i = 2 : n+1
			b(i) =a(i) +xi*b(i-1);	
		end
		R1 = b(n+1);
		c(1) = b(1);
		for i = 2 : n
			c(i) =b(i) +xi*c(i-1);
		end
		R2 = c(n);
		dx= -R1/R2;
		x=xi+dx;
		xi=x;
		b(1) = a(1);
		for i = 2 : n+1
			b(i) =a(i) +x*b(i-1);	
		end 
		RN = b(n+1);
		erro=abs(RN);
	end
end