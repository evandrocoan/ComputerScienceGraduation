#include <stdio.h>
#include <stdlib.h>
#include <math.h>       /* sqrt */

/**
 * Função principal que inicia a execução do programa que encotra os números
 * multuamente amigos.
 * 
 * @param argc o tamanho do vetor fornecido
 * @param argv um vetor de char's que deve conter 2 elementos que representam
 * os limites de busca para encontrar os números multuamente amigos
 * @return o código que representa se a execução do programa ocorreu com 
 * sucesso
 */
int main( int argc, char **argv )
{
    //valores maximos e minimos fornecidos pelo usuario
    int minimo = atoi( argv[ 1 ] );
    int maximo = atoi( argv[ 2 ] );

    // variáveis para utilizar em for's
    int i, j;
    
    // variaveis para auxiliar os calculos
    double fracaoA, fracaoB;
    
    // a cada interação dos for's, calcula as razões da soma dos divisores de 
    // todos os numeros do intervalo pelo próprio número e compara para ver 
    // se os resultados são iguais, caso sim, são mutuamente amigos
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

/** 
 * Método que soma os divisores de um número. Utiliza uma propriedade 
 * matemática para diminuir a complexidade do algoritmo
 * 
 * @param valor até qual será realizada a soma dos divisores
 * @return a soma dos divisores de um número
 */ 
int calculaSomaDosDivisores( int valor )
{
    int extra = (int) sqrt( valor ), j, soma = 0;
    for( j = 1; j <= extra; j++ )
    {
        if( valor % j == 0 )
        {
            soma += j + ( valor / j );
        }
    }
    return soma;
}

