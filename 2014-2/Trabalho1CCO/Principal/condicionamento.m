function c = condicionamento(A)
	detA = abs(det(A));
	alfa = [];
	for i = 1: rows(A)
		a=0;
		for j = 1: columns(A)
			a = a+A(i,j)^2;
		end
		alfa(i) = sqrt(a);
	end
	palfa = prod(alfa);
	detNorm = detA/palfa;
	c = (detNorm > 1e-6);
end
