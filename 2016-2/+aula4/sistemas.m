

format short

ImpossibleSystem = [ % SI (Sistema Impossível)
  1, 2, 4;
  1, 2, 1 ];

IndeterminateSystem = [ % SPI (Sistema Possível e Indeterminável)
  1, 2, 4;
  0, 0, 0 ];

RandomSystemMatrix = [
  pi       ,  e         ,  sqrt(2)       , 1; 
  sqrt( 3 ),  sqrt( 10 ), -1 / sqrt( 3 ) , 2; 
  e        , -sqrt( 2 ) ,  1 / sqrt( 10 ), 3; ];
  
RandomSystemSolution = [ 1.137565113084189,  -0.239010641300073,  -1.360521473123446 ];

SystemMatrix = [ 
2  5 -1  4 0 ;  
1  1  1  1 0 ;  
4 -3  6  1 0 ; 
2 -5 -3 -1 7 ; ];

SystemSolution = [ 3.00000   1.00000  -1.00000  -3.00000 ];


SystemMatrix   = RandomSystemMatrix
SystemSolution = RandomSystemSolution

line_size   = size( SystemMatrix, 1 )
column_size = size( SystemMatrix, 2 ) % Pega o tamanho da primeira coluna


% Utiliza o pivotamento_parcial(3), para cada uma das linhas e evitar zeros na diagonal principal
MySolution = fgauss( SystemMatrix, line_size, column_size )

cd '../+aula3'
printf( 'Changing directory to ../+aula3\n\n\n\n\n\n' )
fgauss( SystemMatrix )

cd '../2_19.08'
printf( 'Changing directory to ../2_19.08\n\n\n\n\n\n' )

MySolution = fgauss( SystemMatrix, line_size )
SystemSolution

cd '../+aula4'

%Dificilmente um numero real será igual a zero. Para isso fazemos abs( n ) < 1.e-15
residuo_maximo = rmax( SystemMatrix, line_size, MySolution )







