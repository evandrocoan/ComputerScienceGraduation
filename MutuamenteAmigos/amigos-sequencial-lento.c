#include <stdio.h>
#include <stdlib.h>
#include <math.h>       /* sqrt */

int main( int argc, char **argv )
{
    //valores maximos e minimos fornecidos pelo usuario
    int minimo = atoi( argv[ 1 ] );
    int maximo = atoi( argv[ 2 ] );

    // variáveis para utilizar em for's
    int i, j;
    double fracaoA, fracaoB;
    
    for( i = minimo; i <= maximo; i++ )
    {
        fracaoA = (double) calculaSomaDosDivisores( i ) / i;
        for( j = i + 1; j <= maximo; j++ )
        {
            fracaoB = (double) calculaSomaDosDivisores( j ) / j;
            if( fracaoA == fracaoB )
            {
                printf( "Os numeros %d e %d são mutuamente amigos.\n", ( i ),
                        ( j ) );
            }
        }
    }
    
    return -1;
}

int calculaSomaDosDivisores( int valor )
{
    int extra = (int) sqrt( valor ), j, soma = 0;
    for( j = 1; j <= extra; j++ )
    {
        if( valor % j == 0 )
        {
            //	printf("\nDivisores de %d: %d",valor, j);
            //printf("\nDivisores de %d: %d",valor, valor/j);
            soma += j + ( valor / j );
        }
    }
    return soma;
}

