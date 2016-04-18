
/**
 * Compile and link with "-std=c++11 -pthread". Nome do aluno:
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
#include <string>
#include <sstream>


using namespace std;

typedef vector< vector< int > > Campo;


// Global variables
Campo campo;

bool works = true;


// Functions prototypes
Campo ini();
Campo solved();
void* verify( void* );


/**
 * Start the program execution and read the program argument list passed to it. This program
 * accept none or one command line argument. If passed, it must be an sudoku file path. This
 * file must to follow this structure:
 * 
 * @param argumentsCount         one plus the argument counting passed to the program command line.
 * @param argumentsStringList    an argument list passed the program command line, where its first
 *                               string is current program execution path.
 * 
 * @return the <cstdlib> EXIT_SUCCESS on success, or EXIT_FAILURE on fail.
 */
int main( int argumentsCount, char* argumentsStringList[] )
{
    if( argumentsCount == 1 )
    {
        campo = solved();
    }
    else
    {
        std::ifstream file( argumentsStringList[ 1 ] );
        
        if( file.is_open() )
        {
            char currentChar   = '\n';
            int  currentLine   = -1;
            int  currentCollun = 0;
            
            campo.resize( 9 );
            
            while( file.good() )
            {
                if( currentChar == '\n' )
                {
                    ++currentLine;
                    
                    currentCollun = 0;
                    
                    campo[ currentLine ].resize( 9 );
                    
                    {
                        cout << endl;
                    }
                    while( file.good()
                            && ( currentChar = file.get() ) == '\n' );
                }
                
                if( file.good() 
                    && currentChar == ' ' )
                {
                    {
                        cout << " ";
                    }
                    while( file.good()
                           && ( currentChar = file.get() ) == ' ' );
                    
                    continue;
                }
                
                campo[ currentLine ][ currentCollun ] = currentChar - '0';
                
                cout << "currentChar: " << currentChar - '0' << " ";
                cout << "currentLine: " << currentLine << " ";
                cout << "currentCollun: " << currentCollun << " ";
                cout << "campo[ currentLine ][ currentCollun ]: " << campo[ currentLine ][ currentCollun ] << endl;
                
                ++currentCollun;
                
                if( file.good() )
                {
                    currentChar = file.get();
                }
            }
            
            cout << endl;
            
            // campo.resize( 9 );
            
            // for( int i = 0; i < 9; i++ )
            // {
                // campo[ i ].resize( 9 );
                
                // for( int j = 0; j < 9; j++ )
                // {
                    // file >> campo[ i ][ j ];
                    // cout << campo[ i ][ j ];
                // }
                // cout << endl;
            // }
        }
        else
        {
            cout << "unable to open argumentsStringList[1]";
        }
    }
    
    // campo= ini();
    int       n = 9;
    pthread_t t[ n ];
    int       a[ n ];
    
    for( int i = 0; i < n; i++ )
    {
        cout << "creating thread " << i << endl;
        
        a[ i ] = i;
        
        if( pthread_create( &t[ i ], NULL, verify, &a[ i ] ) != 0 )
        {
            cout << "failed to create thread " << i << endl;
        }
    }
    
    for( int i = 0; i < n; i++ )
    {
        // t[i].join();
        pthread_join( t[ i ], NULL );
        
        cout << "thread " << i << "has joined" << endl;
    }
    
    cout << "" << endl;
    
    if( works )
    {
        cout << "solucao valida" << endl;
    }
    else
    {
        cout << "solucao invalida" << endl;
    }
    
    return EXIT_SUCCESS;
}

void* verify( void* nm )
{
    int n   = *( ( int* ) nm );
    int sum = 0;
    
    // verificar linha n;
    for( int i = 0; i < 9; i++ )
    {
        //alguem ja falhou o campo
        if( !works )
        {
            return 0;
        }
        
        sum += campo[ n ][ i ];
    }
    
    if( sum != 45 )
    {
        works = false;
    }
    
    // verificar coluna n
    sum = 0;
    
    for( int i = 0; i < 9; i++ )
    {
        //alguem ja falhou o campo
        if( !works )
        {
            return 0;
        }
        
        sum += campo[ i ][ n ];
    }
    
    if( sum != 45 )
    {
        works = false;
    }
    
    // verificar quadrante n
    sum = 0;
    
    int x = n / 3;
    int y = n % 3;
    
    for( int i = 0; i < 3; i++ )
    {
        for( int j = 0; j < 3; j++ )
        {
            //alguem ja falhou o campo
            if( !works )
            {
                return 0;
            }
            
            sum += campo[ x * 3 + i ][ y * 3 + j ];
        }
    }
    
    if( sum != 45 )
    {
        works = false;
    }
}


Campo solved()
{
    Campo campo
    {
        { 8, 2, 7,     1, 5, 4,     3, 9, 6 },
        { 9, 6, 5,     3, 2, 7,     1, 4, 8 },
        { 3, 4, 1,     6, 8, 9,     7, 5, 2 },
        
        { 5, 9, 3,     4, 6, 8,     2, 7, 1 },
        { 4, 7, 2,     5, 1, 3,     6, 8, 9 },
        { 6, 1, 8,     9, 7, 2,     4, 3, 5 },
        
        { 7, 8, 6,     2, 3, 5,     9, 1, 4 },
        { 1, 5, 4,     7, 9, 6,     8, 2, 3 },
        { 2, 3, 9,     8, 4, 1,     5, 6, 7 },
    };
    
    return campo;
}


Campo ini()
{
    Campo campo;
    campo.resize( 9 );
    
    for( int i = 0; i < 9; i++ )
    {
        campo[ i ].resize( 9 );
        
        for( int j = 0; j < 9; j++ )
        {
            campo[ i ][ j ] = rand() % 9 + 1;
        }
    
    }
    
    return campo;
}













