
#{
Dado o sistema linear abaixo para n=50 equações:

Matrix Form
para i = 1;
A( i, i )     = 3;
A( i, i + 1 ) = 1;
A( i, n + 1 ) = 450;

para i = 2 : n / 2
A( i, i - 1 )     = 20;
A( i, i )         = 50;
A( i, i + 1 )     = 1;
A( i, i + n / 2 ) = 1;
A( i, n + 1 )     = 100;

para i = n / 2 + 1 : n - 1
A( i, i - n / 2 ) = 11;
A( i, i - 1 )     = 3;
A( i, i )         = 60;
A( i, i + 1 )     = 1;
A( i, n + 1 )     = 200;

para i = n;
A( i, i - 1 ) = 3;
A( i, i )     = 10;
A( i, n + 1 ) = 300;


Equations form:

para i = 1;
3 * x( i ) + x( i + 1 )                                          = 450;

para i = 2 : n / 2
20 * x( i - 1 ) + 50 * x( i ) + x( i + 1 ) + x( i + n / 2 )      = 100;

para i = n / 2 + 1 : n - 1
11 * x( i - n / 2 ) + 3 * x( i - 1 ) + 60 * x( i ) + x( i + 1 )  = 200;

para i = n;
3 * x( i - 1 ) + 10 * x( i )                                     = 300;

a). Determine a solução do sistema acima pelo método direto de Gauss SEM pivotação. 
Registre (via contador) o número total de operações em PONTO FLUTUANTE utilizadas. 
Calcule o resíduo máximo e o erro de Truncamento máximo na solução acima;

b). Determine a solução do sistema acima pelo método direto de Gauss COM pivotação.  
Registre (via contador) o número total de operações em PONTO FLUTUANTE utilizadas. 
Calcule o resíduo máximo na solução acima.

c). Compare o resíduo máximo das 2 soluções acima e defina qual é a mais exata; 

d). Determine a solução do sistema acima pelo método iterativo de Jacobi. 
Teste fatores de relaxação (sub ou sobre, entre 0<relax<2), determine e use o seu 
valor otimizado (aquele que permite a convergência com o menor número de iterações). 
Registre (via contador) o número total de operações em PONTO FLUTUANTE utilizadas, 
para critério de parada soma|(x-xi)|<1e-4;

e). Determine a solução do sistema acima pelo método iterativo de Gauss-Seidel. Teste 
fatores de relaxação (sub ou sobre, entre 0<relax<2), determine e use o seu valor otimizado. 
Registre (via contador) o número total de operações em PONTO FLUTUANTE utilizadas, para 
critério de parada soma|(x-xi)|<1e-4. Calcule erro de Truncamento máximo da solução 
aproximada obtida.
Lembre-se que o erro de Truncamento máximo de uma solução iterativa pode ser estimado 
por: max| x( aproximado, double, iter=n ) .- x( aproximado, double, iter=2n ) |

#}


n = 50;

format long
split_long_rows(0)
#output_precision(30)
#output_max_field_width(0)

#{
#########################################################################################################
############################################################################################################
printf( 'a) Determine a solução do sistema acima pelo método direto de Gauss SEM pivotação. \n' )
printf( '   Registre (via contador) o número total de operações em PONTO FLUTUANTE utilizadas. \n' )
printf( '   Calcule o resíduo máximo e o erro de Truncamento máximo na solução acima; \n' )

SingleMatrix               = create_single_matrix( n );
[ solucao, operacoes ]     = fgauss_sem_pivotacao( SingleMatrix, n, n + 1 );
[ max_residue, operacoes ] = rmax( SingleMatrix, n, solucao, operacoes );

printf( '\nO numero de operacoes de ponto flutuante foi: %d', operacoes );
printf( '\nO residuo maximo foi: %10.30f, e a solucao eh: \n', max_residue );
print_solution( solucao, n );


############################################################################################################
############################################################################################################
printf( '\nb) Determine a solução do sistema acima pelo método direto de Gauss COM pivotação.\n' );
printf( '   Registre (via contador) o número total de operações em PONTO FLUTUANTE utilizadas.\n' );
printf( '   Calcule o resíduo máximo na solução acima.\n' );

[ solucao, operacoes ]      = fgauss_com_pivotacao( SingleMatrix, n, n + 1 );
[ max_residue2, operacoes ] = rmax( SingleMatrix, n, solucao, operacoes );

printf( '\nO numero de operacoes de ponto flutuante foi: %d', operacoes );
printf( '\nO residuo maximo foi: %10.30f, e a solucao eh: \n', max_residue );
print_solution( solucao, n );


#########################################################################################################
############################################################################################################
printf( '\nc) Compare o resíduo máximo das 2 soluções acima e defina qual é a mais exata; \n' );

max_residue_diff = max_residue - max_residue2;
printf( '\nA segunda solucao eh mais exata por que apresenta um resíduo menor.\n' );
printf( 'Isso pode ser visto por que ao fazer max_residue - max_residue2, temos um numero\n' );
printf( 'positivo. Assim sabemos que o primeiro numero eh maior que o segundo.\n' );
#}

#########################################################################################################
############################################################################################################
printf( ' d). Determine a solução do sistema acima pelo método iterativo de Jacobi. \n' );
printf( ' Teste fatores de relaxação (sub ou sobre, entre 0<relax<2), determine e use o seu \n' );
printf( ' valor otimizado (aquele que permite a convergência com o menor número de iterações). \n' );
printf( ' Registre (via contador) o número total de operações em PONTO FLUTUANTE utilizadas, \n' );
printf( ' para critério de parada soma|(x-xi)|<1e-4;\n\n' );

for i = 1 : n
    
    xi( i ) = 1;
    
end

operacoes = 0;

currentStep  = 0;
maximumSteps = 1000;

currentError = 1;
desiredError = 1e-4;

while( ( currentStep < maximumSteps ) && ( currentError > desiredError ) )
    
    currentStep = currentStep + 1
    
    i = 1;
    x( i ) = ( - xi( i + 1 ) + 450 ) / 3;
    operacoes = operacoes + 1;
    
    for i = 2 : n / 2
        
        x( i ) = ( 100 + xi( i + 1 ) + xi( i + n / 2 ) + 20 * xi( i - 1 ) ) / 50;
        operacoes = operacoes + 2;
        
    end
    
    for i = n / 2 + 1 : n - 1
        
        x( i ) = ( 200 + 11 * xi( i - n / 2 ) + 3 * xi( i - 1 ) + xi( i + 1 ) ) / 60; 
        operacoes = operacoes + 3;
        
    end
    
    i = n;
    x( i ) = ( 300 + 3 * xi( i - 1 ) ) / 10;
    operacoes = operacoes + 2;
    
    errors       = xi .- x;
    currentError = 0;
    
    for i = 1 : n
        
        currentError = currentError + abs( errors( i ) );
        
    end
    
    currentError;
    xi = x;
    
end

operacoes
currentError
xi
x








