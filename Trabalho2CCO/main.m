% Autores
% Joao Marcus Alves (07132067) 
% Evandro  Coan

%limpa o interpretador
clc
clear
format long

erroMax = 1.e-6;

%definimos regi�o os faremos o c�lculo das ra�zes
a = 0;
b = 50 * pi;

%tamanho do nosso vetor de ra�zes
n = 10;

%�ndice das ra�zes (o n�mero identificador da ra�z no vetor)
ir = 1;

[ xInicio, xFinal ] = localizaRaiz( a, b )

while( ir < n + 1 )
  raiz(ir) = newtonRaphson( xInicio(ir), xFinal(ir), erroMax );
  ir = ir + 1;
end

raiz
