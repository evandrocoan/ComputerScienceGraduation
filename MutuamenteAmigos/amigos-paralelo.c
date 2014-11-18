#include <stdio.h>
#include <stdlib.h>
#include <math.h>       /* sqrt */

/**
 * Função principal que inicia a execução do programa que encotra os números
 * multuamente amigos.
 * 
 * @param argc o tamanho do vetor fornecido
 * @param argv um vetor de char's que deve conter 2 elementos que representam
 * os limites de busca para encontrar os números multuamente amigos. E o 
 * ternceiro elemento deve ser o número de threads que este programa tenrá 
 * ulizar caso estejam disponíveis recursos de hardware
 * @return o código que representa se a execução do programa ocorreu com 
 * sucesso.
 */
int main( int argc, char **argv )
{
    // pega o primeiro limite da busca
    int minimo = atoi( argv[ 1 ] );
    
    // pega o segundo limite da busca
    int maximo = atoi( argv[ 2 ] );
    
    // pega o número de threads que executaram o cálculo
    int numThreads = atoi( argv[ 3 ] );
    
    // calcula a amplitude da busca
    int range = maximo - minimo;
    
    int i, j;
    double amigos[ range ];
    double fracaoA;
    
    omp_set_num_threads( numThreads );
    for( i = minimo; i <= maximo; i++ )
    {
        fracaoA = (double) somaDivisores( i ) / i;
        amigos[ i - minimo ] = fracaoA;
    }
    
    for( j = 0; j <= range; j++ )
    {
        
#pragma omp parallel for schedule(static)
        for( i = j + 1; i <= range; i++ )
        {
            if( amigos[ j ] == amigos[ i ] )
            {
                printf( "Os numeros %d e %d são mutuamente amigos.\n",
                        ( minimo + i ), ( minimo + j ) );
            }
        }
        
    }
    return 0;
}

int somaDivisores( int valor )
{
    int extra = (int) sqrt( valor ), j, divid, soma = 0;
    for( j = 1; j <= extra; j++ )
    {
        if( valor % j == 0 )
        {
            divid = valor / j;
            //	printf("\nDivisores de %d: %d",valor, j);
            //printf("\nDivisores de %d: %d",valor, valor/j);
            if( ( valor / j ) == j )
                divid = 0;
            soma += j + divid;
            
        }
    }
    return soma;
}

