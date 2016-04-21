
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


/**
 * This is an abstract class to represent a complete sudoku input and offer an basic structure
 * to offer how to verify whether it is a valid sudoku. It is processed any way to input a complete
 * sudoku and let to the concrete class which implements this to handle the sudoku solution
 * verification.
 */
class SudokuStrategy
{
public:
    
    /**
     * To creates a default sudoku, which is a valid (solved) soduku.
     */
    SudokuStrategy();
    
    /**
     * To creates a sudoku object given an input sudoku text file properly formatted.
     * 
     * @param sudokuFileAddress                an char pointer to the sudoku's file path.
     * 
     * @see SudokuStrategy::processInputSudoku( char* ) member function declaration for the input text file
     *      format.
     */
    SudokuStrategy( char *sudokuFileAddress );
    
    /**
     * Free the heap dynamic allocated memory on object destruction.
     */
    ~SudokuStrategy();
    
    /**
     * To erases the current loaded sudoku and to creates a new random sudoku using random valid
     * values.
     */
    void createRandomSudoku();
    
    /**
     * Verifies the current loaded sudoku solution the desired strategy.
     */
    virtual bool computeSudoku() = 0;
    
    
protected:
    
    /**
     * An matrix to store the inputed sudoku values.
     */
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
    
    
private:
    
    /**
     * To creates an sudoku accordantly by the input file passed. The Sudoku's text file must 
     * to follow this structure:
     * 
     * Any text without numbers, on any line. The next line has the sudoku numbers:
     * 8 2 7,  some space  1 5 4,    3 9 6 after all nine sudoku's digits, you can place other
     * numbers.
     * You can also skip lines and put any other non-digit characters between the sudoku's numbers.
     * 
     * 9 6 5,         3 2 7,         1 4 8 Huehuehue
     * 3 4 1,         6 8 9,         7 5 2  Huehuehue
     * 
     * 5 9 3, ||||||  4 6 8, ||||||  2 7 11337
     * 4 7 2, ||||||  5 1 3, ||||||  6 8 9 1337
     * 6 1 8, ||||||  9 7 2, ||||||  4 3 5  1337
     * 
     * 7 8 6, %%%%%%% 2 3 5, &&&&&&& 9 1 4 |
     * 1 5 4, %%%%%%% 7 9 6, &&&&&&& 8 2 3 |
     * 2 3 9, %%%%%%% 8 4 1, &&&&&&& 5 6 7 |
     * 
     * This example is also an valid sudoku input!
     * Just remember, once you to start putting numbers on a line they will be the sudoku's
     * numbers, and must be at least nine numbers.
     * 
     * 
     * @param *sudokuFileAddress    an char pointer to the 
     */
    void processInputSudoku( char* sudokuFileAddress );
    
};


/**
 * @see SudokuStrategy::SudokuStrategy() member class declaration.
 */
SudokuStrategy::SudokuStrategy()
{
}

/**
 * @see SudokuStrategy::SudokuStrategy( char* ) member class declaration.
 */
SudokuStrategy::SudokuStrategy( char* sudokuFileAddress )
{
    processInputSudoku( sudokuFileAddress );
}

/**
 * @see SudokuStrategy::~SudokuStrategy() member class declaration.
 */
SudokuStrategy::~SudokuStrategy()
{
}

/**
 * @see SudokuStrategy::processInputSudoku() member class declaration.
 */
void SudokuStrategy::processInputSudoku( char *sudokuFileAddress )
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

/**
 * @see SudokuStrategy::createRandomSudoku() member class declaration.
 */
void SudokuStrategy::createRandomSudoku()
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



/**
 * Implements the abstract class SudokuStrategy sudoku solution algorithm using POSIX 9 threads.
 */
class SudokuStrategyWith9Threads : public SudokuStrategy
{
public:
    /**
     * Inherits the superclass constructor.
     */
    using SudokuStrategy::SudokuStrategy;
    
    /**
     * Implements the abstract class SudokuStrategy method. It verifies the current loaded sudoku
     * solution using the Wagner's method.
     * 
     * @see SudokuStrategy::computeSudoku()
     */
    bool computeSudoku();
    
    
private:
    
    /**
     * An boolean value used by the Wagner's method to verifies the sudoku's solution.
     */
    bool works = true;
    
    /**
     * Structure for passing data to threads.
     */
    struct parameters
    {
        int                         currentElement;
        int                         indexesArray[9];
        SudokuStrategyWith9Threads* currentSudoku;
    };
    
    /**
     * This is a bridge for the C POSIX thread to run from an C++ member class function. The POSIX
     * thread cannot run properly an C++ member function due all C++ member's function to receive
     * an additional hidden object parameter this, to reference the current object within its
     * functions. And the POSIX thread requires the forwarding calling function to receive only
     * a void pointer parameter.
     * 
     * @param voidArgumentPointer      a parameter's data struct void pointer.
     */
    static void* startThread( void* voidArgumentPointer );
    
    /**
     * Performs the current sodoku solutions check
     */
    void verify( int n );
    
};


/**
 * @see SudokuStrategyWith9Threads::computeSudoku()
 */
bool SudokuStrategyWith9Threads::computeSudoku()
{
    parameters *data = (parameters *) malloc( sizeof( parameters ) );
    
    // Now create the thread passing it data as a parameter
    data->currentElement  = 0;
    data->indexesArray[0] = 0;
    data->currentSudoku   = this;
    
    int n = 9;
    
    pthread_t t[ n ];
    
    for( int i = 0; i < n; i++ )
    {
        std::cout << "creating thread " << i << std::endl;
        
        data->currentElement    = i;
        data->indexesArray[ i ] = i;
        
        if( pthread_create( &t[ i ], NULL, startThread, data ) != 0 )
        {
            std::cout << "failed to create thread " << i << std::endl;
        }
    }
    
    for( int i = 0; i < n; i++ )
    {
        pthread_join( t[ i ], NULL );
        
        std::cout << "thread " << i << " has joined" << std::endl;
    }
    
    return this->works;
}

/**
 * @see SudokuStrategyWith9Threads::startThread( void* ) member class declaration.
 */
void* SudokuStrategyWith9Threads::startThread( void* voidArgumentPointer )
{
    parameters* data          = static_cast< parameters* >( voidArgumentPointer );
    int         wagnersNumber = data->indexesArray[ data->currentElement ];
    
    data->currentSudoku->verify( wagnersNumber );
    
    return NULL;
}

/**
 * @see SudokuStrategyWith9Threads::verify() member class declaration.
 */
void SudokuStrategyWith9Threads::verify( int n )
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
 * Start the program execution and read the program argument list passed to it. This program
 * accept none or one command line argument. If passed, it must be an sudoku file path. See the
 * SudokuStrategy class documentation for the sudoku's text file structure.
 * 
 * @param argumentsCount         one plus the argument counting passed to the program command line.
 * @param argumentsStringList    an argument list passed the program command line, where its first
 *                               string is current program execution path.
 * 
 * @return the <cstdlib> EXIT_SUCCESS on success, or EXIT_FAILURE on fail.
 */
int main( int argumentsCount, char* argumentsStringList[] )
{
    SudokuStrategy *sudoku;
    
    if( argumentsCount == 2 )
    {
        sudoku = new SudokuStrategyWith9Threads( argumentsStringList[ 1 ] );
        
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
        sudoku = new SudokuStrategyWith9Threads();
        
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
    
    delete sudoku;
    
    return EXIT_SUCCESS;
}






















