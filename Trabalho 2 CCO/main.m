%limpa o interpretador
clc
clear
format long

erroMax = 1.e-6;

%definimos região os faremos o cálculo das raízes
a = 0;
b = 50 * pi;

%tamanho do nosso vetor de raízes
n = 10;

%índice das raízes (o número identificador da raíz no vetor)
ir = 1;

[ xInicio xFinal ] = localizaRaiz( a, b )

%while( ir < n )
  %raiz(ir) = newtonRaphson( xInicio, xFinal, erroMax ); 
%end
