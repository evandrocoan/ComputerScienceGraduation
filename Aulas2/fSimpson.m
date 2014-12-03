%implementação do metodo de simpson
function Sn = fSimpson( n, a, b )
	somatorioPar = 0;
	somatorioImpar = 0;
	h = ( b - a ) / n;
	x = a : h : b;
	for i = 2 : 2 : n % incremento de dois em dois
		somatorioPar += g( x(i) );
	end
	for i = 3 : 2 : n - 1 % incremento de dois em dois
		somatorioImpar += g( x(i) );
	end
	Sn = h/3 * ( g( x(1) ) + g( x( n + 1 ) ) + 4 * somatorioPar + 2 * somatorioImpar );
end
