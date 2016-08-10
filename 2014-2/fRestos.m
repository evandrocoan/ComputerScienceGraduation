function [R M] = fRestos(n,a,xi)
%R é um vetor
	nraizes = n;
	b = fDivBrio( n,a,xi ); %b = polinomio quociente (de grau n-1) e b(n+1) = resto
	R(1) = b(n+1);	
	n--;
	a = b;
	M = 1;
	i = 1;
	residuo = 1e-1;
	while(i<=M)
		i++;
		b = fDivBrio( n,a,xi ); %b = polinomio quociente (de grau n-1) e b(n+1) = resto
		R(i) = b(n+1);	
		a = b;
		n--;
		if (abs(R(i)) < residuo && abs(R(i-1)) < residuo)
			M = i;
		end
	end
end
