function [xInicio xFinal] = fLocaliza(A , B)
	h = 0.1;
	%indice da raiz
	ir = 0;
	a = A;
	b = a + h;
	while b < B-h
		%
		if( f(a) * f(b) < 0 )
			ir = ir + 1		
			xInicio(ir) = a;
			xFinal(ir) = b;		
		end 
		a = b;
		b = a + h;
		%
	end
end
