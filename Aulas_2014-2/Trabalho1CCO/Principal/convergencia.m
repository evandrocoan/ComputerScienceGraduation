function c = convergencia(A);
	c = false;
	cG = [];
	for i = 1 : rows(A)
		sL = 0;
		for j = 1 : columns (A)
			if(i!=j)
				sL = sL + abs(A(i,j));
			endif
		end
		if(abs(A(i,i))>=sL)
			if(abs(A(i,i))>sL)
				cG(i) = true;
			else
				cG(i) = false;
			endif
		endif
	end
	
	for i = 1 : rows (A)
		if (cG == true)
			c = true;
		endif
	end
end
