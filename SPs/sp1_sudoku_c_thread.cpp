
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
Campo g_sudokuVectorMatrix;

bool works = true;


// Functions prototypes
Campo ini();
Campo solved();
void* verify( void* );
void toCreateASolvedSudoku();
void processInputSudoku( char *sudokuFileAddress );

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
        toCreateASolvedSudoku();
    }
    else
    {
        processInputSudoku( argumentsStringList[ 1 ] );
        
        cout << '\n' << endl;
        
        // g_sudokuVectorMatrix.resize( 9 );
        
        // for( int i = 0; i < 9; i++ )
        // {
            // g_sudokuVectorMatrix[ i ].resize( 9 );
            
            // for( int j = 0; j < 9; j++ )
            // {
                // sudokuFileInput >> g_sudokuVectorMatrix[ i ][ j ];
                // cout << g_sudokuVectorMatrix[ i ][ j ];
            // }
            // cout << endl;
        // }
    }
    
    // g_sudokuVectorMatrix= ini();
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

/**
 * 
 * @param *sudokuFileAddress    an char pointer to the 
 */
void processInputSudoku( char *sudokuFileAddress )
{
    std::ifstream sudokuFileInput( sudokuFileAddress );
    
    if( sudokuFileInput.is_open() )
    {
        char currentChar   = '\n';
        int  currentLine   = -1;
        int  currentColumn = 0;
        
        g_sudokuVectorMatrix.resize( 9 );
        
        while( sudokuFileInput.good() )
        {
            if( isdigit( currentChar ) && currentColumn < 9 )
            {
                g_sudokuVectorMatrix[ currentLine ][ currentColumn ] = currentChar - '0';
                
                printf( "[%i,%i]%i", currentLine, currentColumn, g_sudokuVectorMatrix[ currentLine ][ currentColumn ] );
                
                if( sudokuFileInput.good() )
                {
                    currentChar = sudokuFileInput.get();
                }
                
                ++currentColumn;
            }
            else
            {
                // handle any CR/LF zoo
                if( currentChar == '\n'
                    || currentChar == '\r' )
                {
                    do
                    {
                        cout << endl;
                    }
                    while( sudokuFileInput.good()
                           && ( ( currentChar = sudokuFileInput.get() ) == '\n'
                                 || currentChar == '\r' ) );
                    
                    if( isdigit( currentChar ) )
                    {
                        ++currentLine;
                        
                        currentColumn = 0;
                        
                        g_sudokuVectorMatrix[ currentLine ].resize( 9 );
                    }
                    
                    continue;
                }
                
                // ignore unrecognized character
                cout << currentChar;
                currentChar = sudokuFileInput.get();
            }
        }
        
        sudokuFileInput.close();
    }
    else
    {
        cout << "unable to open argumentsStringList[1]";
    }
}

void* verify( void* nm )
{
    int n   = *( ( int* ) nm );
    int sum = 0;
    
    // verificar linha n;
    for( int i = 0; i < 9; i++ )
    {
        //alguem ja falhou o g_sudokuVectorMatrix
        if( !works )
        {
            return 0;
        }
        
        sum += g_sudokuVectorMatrix[ n ][ i ];
    }
    
    if( sum != 45 )
    {
        works = false;
    }
    
    // verificar coluna n
    sum = 0;
    
    for( int i = 0; i < 9; i++ )
    {
        //alguem ja falhou o g_sudokuVectorMatrix
        if( !works )
        {
            return 0;
        }
        
        sum += g_sudokuVectorMatrix[ i ][ n ];
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
            //alguem ja falhou o g_sudokuVectorMatrix
            if( !works )
            {
                return 0;
            }
            
            sum += g_sudokuVectorMatrix[ x * 3 + i ][ y * 3 + j ];
        }
    }
    
    if( sum != 45 )
    {
        works = false;
    }
}


void toCreateASolvedSudoku()
{
    Campo g_sudokuVectorMatrix
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
    
    ::g_sudokuVectorMatrix = g_sudokuVectorMatrix;
}


Campo ini()
{
    Campo g_sudokuVectorMatrix;
    
    g_sudokuVectorMatrix.resize( 9 );
    
    for( int i = 0; i < 9; i++ )
    {
        g_sudokuVectorMatrix[ i ].resize( 9 );
        
        for( int j = 0; j < 9; j++ )
        {
            g_sudokuVectorMatrix[ i ][ j ] = rand() % 9 + 1;
        }
    
    }
    
    return g_sudokuVectorMatrix;
}













