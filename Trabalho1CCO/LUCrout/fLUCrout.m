%Recebe parâmetros n e A e retorna uma matriz 
%A = [L\U]
function [A b] = fLUCrout(n, A, b)
	k = 1;
	%Li1 = Ai1
	[A b] = pivotLU(k, n, A, b);
	for j = 2 : n
		A(k, j) = A(k, j) / A(k, k);
	end
	for k = 2 : ( n - 1 )
		%Cálculo do Lik da fórmula
		for i = k : n
			%Somatório calcula o auxilar da fórmula
			%re-inicializamos com 0 por garantia
			aux = 0;
			for r = 1 : k - 1		
				aux = aux + ( A(i, r) * A(r, k) ) ;
			end
			A(i, k) = A(i, k) - aux;
		end
		[A b] = pivotLU(k, n, A, b);
		%Cálculo do Ukj da fórmula
		for j = k + 1 : n
			%re-inicializamos com 0 por garantia
			aux = 0;
			%Somatório calcula o auxiliar da fórmula
			for r = 1 : k - 1
				aux = aux + A(k, r) * A(r, j);
			end
			A(k, j) = ( A(k, j) - aux ) / A(k, k);
		end
	end
	k = n;
	i = k;
	%Somatório 
	%re-inicializamos com 0 por garantia
	aux = 0;
	for r = 1 : k - 1		
		aux = aux + ( A(i, r) * A(r, k) ) ;
	end
	A(i, k) = A(i, k) - aux;
	A;
end
