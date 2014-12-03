clc
clear
format long

%dominio da função
A = realmin;
B = 10;

%cria um vetor de 3000 posições com variação de 0.001
x = 0.001 : 0.1 : 3;

%cria outro vetor de 3000 valores
y = f(x);

[xInicio xFinal] = fLocaliza(A , B) %funcao que localiza intervalos que contenham as raizes reais

nRaizes = rows(xInicio)

erroMaximo = 1.e-6;
for ir = 1 : nRaizes
	%[ erro raiz(ir) ] = fBissecao( xInicio(ir), xFinal(ir), erroMaximo) % precisa de intervalo, converge sempre
	%[ raiz(ir) erro iter ] = fFalsaPos( xInicio(ir), xFinal(ir), erroMaximo) % precisa de intervalo converge sempre
   %[ raiz(ir) erro iter ] = fIterLin( xInicio(ir), xFinal(ir), erroMaximo) % 
   [ raiz(ir) erro iter ] = fNewRap( xInicio(ir), xFinal(ir), erroMaximo) % quando não converge, false posição, ou bisseção convergem
	%[ raiz(ir) erro iter ] = fNewRapNumerico( xInicio(ir), xFinal(ir), erroMaximo) % idem, porem quando não se tem acesso a função que realiza os calculos (código privado, encapsulado)
end
%desenha os pontos dos vetores x e y
plot( x, y )
grid on;
