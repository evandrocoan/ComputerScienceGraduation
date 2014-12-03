%-alpha, alpha = intervalo de busca das raízes
function xi = fLocalizaPoli( n, a)

	alpha = modMax( n, a );
	%localização das raízes reais
	h = 0.1;
	%indice da raiz
	ir = 0;
	%define-se os pequenos intervalos de busca de comprimento 'h'
	xa = -alpha;
	xb = xa + h;
	while xb < alpha - h
		% núcleo de método de busca
			%valores do polinomio nos pontos xa e xb (calculados por brio..)
			coefDivA = fDivBrio( n, a , xa );
			coefDivB = fDivBrio( n, a , xb );
			if( coefDivA( n+1) * coefDivB( n+1 ) < 0 ) % teorema de bolzano, teste de raízes REAIS
				ir = ir + 1;
				xi(ir) = ( xa + xb ) * .5;	
			end 
			%pula para o próximo intervalo a, b
			xa = xb;
			xb = xa + h;
		% final do núcleo
	end

	%construção do vetor inicial para cálculo das raízes, menores que alpha, que indica o limite máximo para uma raíz
	%completa com valores (complexos) restantes para o cálculo
	aux = sqrt( alpha ) * .5;
	xi( ir+1 : n ) = complex( aux, aux );
end
