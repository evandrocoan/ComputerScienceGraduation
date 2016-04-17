
/**
 * Compile and link with -pthread. Nome do aluno:
 *
 * @author Evandro  Coan
 * @author Wagner Fernando Gascho
 *
 */

#include <iostream>
#include <pthread.h>
#include <stdlib.h>
#include <vector>
#include <fstream>


typedef vector<vector<int> > Campo;


// Global variables
Campo campo;

bool works = true;


// Functions prototypes
Campo ini();
Campo solved();
void* verify( void* );

using namespace std;


int main( int argc, char* argv[] )
{
    if ( argc == 1 ) { campo = solved(); }
    else {
        ifstream file ( argv[1] );
        if ( file.is_open() ) {
            campo.resize ( 9 );
            for ( int i = 0; i < 9; i++ ) {
                campo[i].resize ( 9 );
                for ( int j = 0; j < 9; j++ ) {
                    file >>  campo[i][j];

                }
            }
        } else {
            cout << "unable to open argv[1]";
        }
    }
    // campo= ini();
    int n=9;
    pthread_t t[n];
    int a[n];
    for ( int i = 0; i < n; i++ ) {
        std::cout << "creating thread "<< i << std::endl;
        //  t[i] = thread(verify,i); tchau thread boa, ola gambis
        a[i]=i;
        if( pthread_create(&t[i], NULL, verify, &a[i]) !=0){
            std::cout<< "failed to create thread "<<i <<std::endl;
        };

    }
    for ( int i = 0; i < n; i++ ) {
        // t[i].join();
        pthread_join ( t[i], NULL );
        std::cout << "thread "<< i << "has joined"<< std::endl;
    }

    std::cout << "" << std::endl;
    if ( works ) {
        std::cout << "solucao valida"<< std::endl;
    } else {
        std::cout << "solucao invalida"<< std::endl;
    }

    return 0;
}


void* verify ( void* nm )
{
    int n = ( * ( int* ) nm );
    int sum = 0;
    ///verificar linha n;
    for ( int i = 0; i < 9; i++ ) {
        if ( !works ) { return 0; } //alguem ja falhou o campo
        sum += campo[n][i];
    }
    if ( sum != 45 ) {
        works = false;
    }
    ///verificar coluna n
    sum = 0;
    for ( int i = 0; i < 9; i++ ) {
        if ( !works ) { return 0; } //alguem ja falhou o campo
        sum += campo[i][n];
    }
    if ( sum != 45 ) {
        works = false;
    }
    ///verificar quadrante n
    sum = 0;
    int x = n / 3;
    int y = n % 3;

    for ( int i = 0; i < 3; i++ ) {
        for ( int j = 0; j < 3; j++ ) {
            if ( !works ) { return 0; } //alguem ja falhou o campo
            sum += campo[x * 3 + i][y * 3 + j];
        }
    }
    if ( sum != 45 ) {
        works = false;
    }
}


Campo solved()
{
    Campo campo 
    {
        {8, 2, 7,     1, 5, 4,     3, 9, 6},
        {9, 6, 5,     3, 2, 7,     1, 4, 8},
        {3, 4, 1,     6, 8, 9,     7, 5, 2},
        
        {5, 9, 3,     4, 6, 8,     2, 7, 1},
        {4, 7, 2,     5, 1, 3,     6, 8, 9},
        {6, 1, 8,     9, 7, 2,     4, 3, 5},
        
        {7, 8, 6,     2, 3, 5,     9, 1, 4},
        {1, 5, 4,     7, 9, 6,     8, 2, 3},
        {2, 3, 9,     8, 4, 1,     5, 6, 7},
    };
    
    return campo;
}


Campo ini()
{
    Campo campo;
    campo.resize ( 9 );
    for ( int i = 0; i < 9; i++ ) {
        campo[i].resize ( 9 );
        for ( int j = 0; j < 9; j++ ) {
            campo[i][j] = rand() % 9 + 1;
        }

    }
    return campo;
}


