
%Sistema Impossível
A = [ 1, 2, 4;
      1, 2, 1 ]

%Sistema Impossível e Indeterminável
A = [ 1, 2, 4;
      0, 0, 0 ]

%Dificilmente um numero real será igual a zero.
%Para isso fazemos abs( n ) < 1.e-15

% Pega o tamanho da primeira coluna
n = size( A, 1 )

% Utiliza o pivotamento_parcial(3), para cada uma das linhas e evitar zeros na diagonal principal
x = fgauss( A, n )

residuo_maximo = rmax( A, n, x )
