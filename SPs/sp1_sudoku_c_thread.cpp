
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


class Sudoku
{
public:
    
    Sudoku()
    {
    }
    
    Sudoku( char *sudokuFileAddress )
    {
        processInputSudoku( sudokuFileAddress );
    }
    
    bool computeSudoku()
    {
        parameters *data = (parameters *) malloc(sizeof(parameters));
        data->row = 1;
        data->column = 1;
        data->a[0]   = 0;
        data->sudoku = this;
        /* Now create the thread passing it data as a parameter */
        
        // g_sudokuVectorMatrix= createRandomSudoku();
        int       n = 9;
        pthread_t t[ n ];
        
        for( int i = 0; i < n; i++ )
        {
            std::cout << "creating thread " << i << std::endl;
            
            //a[ i ] = i;
            data->a[ i ] = i;
            
            if( pthread_create( &t[ i ], NULL, startThread, data ) != 0 )
            {
                std::cout << "failed to create thread " << i << std::endl;
            }
        }
        
        for( int i = 0; i < n; i++ )
        {
            // t[i].join();
            pthread_join( t[ i ], NULL );
            
            std::cout << "thread " << i << "has joined" << std::endl;
        }
        
        return this->works;
    }


private:
    
    bool works = true;
    
    /**
     * Structure for passing data to threads.
     */
    struct parameters
    {
        int    row;
        int    column;
        int    a[9];
        Sudoku *sudoku;
    };
    
    std::vector< std::vector< int > > g_sudokuVectorMatrix
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
    
    static void* startThread( void* arg )
    {
        parameters* data = static_cast< parameters* >( arg );
        
        data->sudoku->verify( *( data->a ) );
        
        return 0;
    }
    
    /**
     * 
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
            
            while( sudokuFileInput.good() )
            {
                if( isdigit( currentChar )
                    && currentLine < 9
                    && currentColumn < 9 )
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
                            std::cout << std::endl;
                        }
                        while( sudokuFileInput.good()
                               && ( ( currentChar = sudokuFileInput.get() ) == '\n'
                                     || currentChar == '\r' ) );
                        
                        if( isdigit( currentChar ) )
                        {
                            ++currentLine;
                            
                            currentColumn = 0;
                        }
                        
                        continue;
                    }
                    
                    // ignore unrecognized character
                    std::cout << currentChar;
                    
                    currentChar = sudokuFileInput.get();
                }
            }
            
            sudokuFileInput.close();
        }
        else
        {
            std::cout << "unable to open argumentsStringList[1]";
        }
    }
    
    void verify( int n )
    {
        int sum = 0;
        
        // verificar linha n;
        for( int i = 0; i < 9; i++ )
        {
            //alguem ja falhou o g_sudokuVectorMatrix
            if( !works )
            {
                return;
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
                return;
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
                    return;
                }
                
                sum += g_sudokuVectorMatrix[ x * 3 + i ][ y * 3 + j ];
            }
        }
        
        if( sum != 45 )
        {
            works = false;
        }
    }
    
    /**
     * 
     */
    void createRandomSudoku()
    {
        g_sudokuVectorMatrix.resize( 9 );
        
        for( int i = 0; i < 9; i++ )
        {
            g_sudokuVectorMatrix[ i ].resize( 9 );
            
            for( int j = 0; j < 9; j++ )
            {
                g_sudokuVectorMatrix[ i ][ j ] = rand() % 9 + 1;
            }
        
        }
    }
};


/**
 * Start the program execution and read the program argument list passed to it. This program
 * accept none or one command line argument. If passed, it must be an sudoku file path. The
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
    Sudoku *sudoku;
    
    if( argumentsCount == 2 )
    {
        sudoku = new Sudoku( argumentsStringList[ 1 ] );
        
        // g_sudokuVectorMatrix.resize( 9 );
        
        // for( int i = 0; i < 9; i++ )
        // {
            // g_sudokuVectorMatrix[ i ].resize( 9 );
            
            // for( int j = 0; j < 9; j++ )
            // {
                // sudokuFileInput >> g_sudokuVectorMatrix[ i ][ j ];
                // std::cout << g_sudokuVectorMatrix[ i ][ j ];
            // }
            // std::cout << std::endl;
        // }
    }
    else
    {
        std::cout << "" << std::endl;
    }
    
    if( sudoku->computeSudoku() )
    {
        std::cout << "solucao valida" << std::endl;
    }
    else
    {
        std::cout << "solucao invalida" << std::endl;
    }
    
    return EXIT_SUCCESS;
}





















