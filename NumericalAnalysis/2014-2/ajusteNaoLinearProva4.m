clc
clear

%número de pontos, do teste experimental
m = 5
T = [ 0, 0.39, .78, 1.18, 1.57 ]
V = [ 0,  .38, .71,  .92, 1    ]


resposta = fNewton( m, T, V )
a = resposta(1)
b = resposta(2)
%plotagem dos pontos 
nPlotagem = 200;
hPlotagem = ( T(m) - T(1) ) / nPlotagem;
Tp = T(1): hPlotagem : T(m); % 201 pontos

for i = 1 : nPlotagem + 1
	Vp(i) = sin( a + b * Tp(i) );
end

%calculo do erro
%um polinomio de grau igual ao numero de pontos fornecidos, passa sobre todos os pontos
%por exemplo, um polinomio de grau 7 possui 8 coeficientes e passa portanto sobre 8 pontos
for i = 1 : m
	VAjustado(i)  =  sin( a + b * T(i) );
end
D=fdesvio(m, V, VAjustado )

plot( T, V, "*;pontos testes;", Tp, Vp, "b;função ajustadora: sin( a + b * Tp(i) );" );
