% derivada parcial de T em relação ao primeiro coeficiente
%x são os coeficientes, m é o número de pontos
%N é um vetor de pontos, C e T são outros vetores de pontos (por ex. numero de clientes, ...)
function y = h1( x, m, N, C, T )
	y = 0;
	for k = 1 : m
		y = y + ( ( x(1) + x(2) * N(k) ^ 2 ) / ( 1 + x(3) * C(k) ) - T(k) ) / ( 1 + x(3) * C(k) ) ;
	end
end
