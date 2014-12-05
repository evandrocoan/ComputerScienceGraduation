clc
clear

%número de pontos, do teste experimental
m = 5
T = [ 0, 0.39, .78, 1.18, 1.57 ]
V = [ 0,  .38, .71,  .92, 1    ]

resposta = fNewton( m, T, V );
a = resposta(1);
b = resposta(2);

for i = 1 : m
	VAjustado(i) = sin( a + b * T(i) );
end

D = fdesvio( m, V, VAjustado )