function [x erro cont] = fNRPM(n,a,xi)
	erro = 1.0;
	cont = 0.0;
	while( erro > 1.e-12 & cont < 100 )
		cont = cont + 1;
		# Temos que descobrir o M e os Rs (restos)
		[M R] = restos(n, a, xi);
		%para desabilitar coloque M = 1
		dx = -R(M)/(R(M+1)*M);
		# dx= -R1/R2;
		x=xi+dx;
		xi=x;
		b(1) = a(1);
		for i = 2 : n+1
			b(i) =a(i) +x*b(i-1);	
		end 
		RN = b(n+1);
		erro=abs(RN)+abs(dx);
	end
end