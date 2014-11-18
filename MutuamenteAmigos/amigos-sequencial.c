#include <stdio.h>
#include <stdlib.h>
#include <math.h>       /* sqrt */

/**
 * Função principal que inicia a execução do programa que encotra os números
 * multuamente amigos.
 * 
 * @param argc o tamanho do vetor fornecido
 * @param argv um vetor de char's que deve conter 2 elementos que representam
 * os limites de busca para encontrar os números multuamente amigos.
 * @return o código que representa se a execução do programa ocorreu com 
 * sucesso.
 */
int main( int argc, char **argv )
{
    //valores maximos e minimos fornecidos pelo usuario
    int minimo = atoi( argv[ 1 ] );
    
    int maximo = atoi( argv[ 2 ] );
    
    // tamanho do intervalo
    int range = maximo - minimo;
    
    // array para armazenar o resultado da razão da soma dos divisores de 
    // um numero pelo numero
    double amigos[ range ];
    
    // variavel para auxiliar o calculo
    double fracaoA;
    
    // variáveis para utilizar em for's
    int j, i;
    
    // calcula a razao da soma dos divisores de todos os 
    // numeros do intervalo pelo proprio numero e armazena este 
    // numero no array amigos
    for( j = minimo; j <= maximo; j++ )
    {
        fracaoA = (double) somaDivisores( j ) / j;
        amigos[ j - minimo ] = fracaoA;
    }
    
    // percorre o array e compara para ver se os resultados são 
    // iguais, caso sim, são mutuamente amigos
    for( i = 0; i <= range; i++ )
    {
        for( j = i + 1; j <= range; j++ )
        {
            if( amigos[ i ] == amigos[ j ] )
            {
//                printf( "Os numeros %d e %d são mutuamente amigos.\n",
//                        ( minimo + j ), ( minimo + i ) );
            }
        }
    }
    return 0;
}

/** 
 * Método que soma os divisores de um numero. Utiliza uma propriedade 
 * matematica para diminuir a complexidade do algoritmo.
 * 
 * @param valor até qual será realizada a soma dos divisores
 * @return a soma dos divisores de um número
 */ 
int somaDivisores( int valor )
{
    int extra = (int) sqrt( valor ), j, divid, soma = 0;
    ;
    for( j = 1; j <= extra; j++ )
    {
        if( valor % j == 0 )
        {
            divid = valor / j;

            if( ( valor / j ) == j )
                divid = 0;
            soma += j + divid;
        }
    }
    return soma;
}

