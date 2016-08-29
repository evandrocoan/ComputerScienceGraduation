OperatedMatrix = [
  pi       ,  e         ,  sqrt(2)       , 1; 
  sqrt( 3 ),  sqrt( 10 ), -1 / sqrt( 3 ) , 2; 
  e        , -sqrt( 2 ) ,  1 / sqrt( 10 ), 3; ];

OriginalInicialMatrix = OperatedMatrix

ImpossibleSystem = [ % SI (Sistema Impossível)
  1, 2, 4;
  1, 2, 1 ]

IndeterminateSystem = [ % SPI (Sistema Possível e Indeterminável)
  1, 2, 4;
  0, 0, 0 ]

n = size( ImpossibleSystem, 1 ) % Pega o tamanho da primeira coluna

% Utiliza o pivotamento_parcial(3), para cada uma das linhas e evitar zeros na diagonal principal
Solution = fgauss( ImpossibleSystem, n )

%Dificilmente um numero real será igual a zero. Para isso fazemos abs( n ) < 1.e-15
residuo_maximo = rmax( ImpossibleSystem, n, Solution )
