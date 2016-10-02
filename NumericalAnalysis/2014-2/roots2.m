%a = polinomio a ter raízes encontradas
function x = roots2( a )
	erroMax = eps;

	%cálculo do grau do polinomio, size retorna as dimensões da matriz, e max pega seu maior valor
	n = max( size( a ) ) - 1 ;
	xi = fLocalizaPoli( n, a )
	nRaizes = n; ai = a; %guarda os valores iniciais para se preservar a precisão
	%ir -> indica qual raíz se está trabalhando
	ir = 1;
	while(ir < nRaizes)
		% x vetor das raízes encontradas
		[ x(ir) M erro cont] = fNRPoliM( n, a, xi(ir), erroMax); % a raíz obtida no polinomio reduzido

		cont;

		% a raíz obtida no polinomio original
		[ x2(ir) M erro cont ] = fNRPoliM( nRaizes, ai, x(ir), erroMax)
		
		cont;

		%reduzir o grau do polinômio ( Pn(x) / (x - x(ir) ) )
		for i = 1:M
			a = fDivBrio( n, a, x2(ir) ); 
			n--;	
		end
		raizes(ir:ir+M-1) = x2(ir);
		ir = ir+M;
		%printf( "\nResto da divisão: %.20f\n", a(n + 1) );
	end

	x = raizes';

end
