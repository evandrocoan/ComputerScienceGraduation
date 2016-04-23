
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
#include <string.h>
#include <sstream>
#include <unistd.h>
#include <errno.h>


/** This is to view internal program data while execution. Default value: 0
 *
 * 0   - Disables this feature.
 * 1   - Normal debug.
 */
#if !defined DEBUG_LEVEL
    #define DEBUG_LEVEL 1


#if DEBUG_LEVEL > 0
    #define DEBUG


pthread_mutex_t g_fprintf_mutex;


/**
 * To convert a macro argument into a string constant.
 */
#define STRINGIFY( a ) #a


/**
 * Print like function for logging putting a new line at the end of string. It does uses mutex
 * due the doubt to know whether 'fprintf' is thread safe of not over every/any platforms, since
 * could not be found anything concrete. Following its explanations:
 *
 * // Lock the mutex.
 * pthread_mutex_lock( g_fprintf_mutex );
 * 
 * // Print to the specified output stream the formatting args. If the variable argument is left
 * // out when the DEBUGGER macro is used, then the comma before the ‘##’ will be deleted.
 * fprintf( stream, ##__VA_ARGS__ ); 
 * 
 * // Print a new line.
 * fprintf( stream, "\n" );
 * 
 * // Flushes the output stream to avoid double output over '>'. Example: './main > results.txt'
 * // would get doubled/... print.
 * fflush( stream );
 * 
 * // Unlock the shared memory mutex.
 * pthread_mutex_unlock( g_fprintf_mutex );  
 * 
 * // To allow to use ';' semicolon over the macro statement use and still to be able to use it
 * // within an unbraced if statement.
 * while( 0 )
 */
#define DEBUGGER( stream, ... ) \
do \
{ \
    pthread_mutex_lock( &g_fprintf_mutex ); \
    \
    fprintf( stream, ##__VA_ARGS__ ); \
    fprintf( stream, "\n" ); \
    fflush( stream ); \
    \
    pthread_mutex_unlock( &g_fprintf_mutex ); \
} \
while( 0 )

#define DEBUGGERN( stream, ... ) \
do \
{ \
    pthread_mutex_lock( &g_fprintf_mutex ); \
    \
    fprintf( stream, ##__VA_ARGS__ ); \
    fflush( stream ); \
    \
    pthread_mutex_unlock( &g_fprintf_mutex ); \
} \
while( 0 )


#else
    #define DEBUGGER( stream, ... )
    #define DEBUGGERN( stream, ... )


#endif


#endif



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
     * To creates a sudoku object given an text input sudoku text file properly formatted.
     * 
     * @param sudokuFileAddress                an std::string properly formatted.
     * 
     * @see SudokuStrategy::processInputSudoku( processInputSudoku ) member function declaration
     *      for the input text file format.
     */
    SudokuStrategy( std::string );
    
    /**
     * To creates a sudoku object given an input sudoku text file properly formatted.
     * 
     * @param sudokuFileAddress                an char pointer to the sudoku's file path.
     * 
     * @see SudokuStrategy::processInputSudoku( std::string ) member function declaration for the
     *      input text file format.
     */
    SudokuStrategy( char* );
    
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
     * Creates a string representation of the current loaded sudoku.
     * 
     * @return an std::string object.
     */
    std::string toString();
    
    /**
     * Verifies the current loaded sudoku solution the desired strategy.
     */
    virtual bool computeSudoku() = 0;
    
    
protected:
    
    /**
     * An matrix to store the inputed sudoku values.
     */
    int g_sudokuVectorMatrix[9][9] =
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
     * 8 2 7 ,  some space  1 5 4,    3 9 6
     * You can also skip lines and put any other non-digit characters between the sudoku's numbers.
     * 
     * 9 6 5,         3 2 7,         1 4 8 Huehuehue
     * 3 4 1,         6 8 9,         7 5 2  Huehuehue
     * more non-digit characters
     * 5 9 3, ||||||  4 6 8, ||||||  2 7 1
     * 4 7 2, ||||||  5 1 3, ||||||  6 8 9
     * 6 1 8, ||||||  9 7 2, ||||||  4 3 5
     * You can also put all or any of the number on just one line. 7 8 6, 2 3 5, 9 1 4    1 5 4, 
     * 7 9 6, &&&&&&& 8 2 3 |
     * 2 3 9, %%%%%%% 8 4 1, &&&&&&& 5 6 7 |
     * 
     * This example is also an valid sudoku input!
     * 
     * 
     * 
     * @param *sudokuFileAddress    an char pointer to the 
     */
    void processInputSudoku( std::string sudokuFileAddress );
    
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
SudokuStrategy::SudokuStrategy( std::string sudokuText )
{
    this->createRandomSudoku();
    this->processInputSudoku( sudokuText );
}

/**
 * @see SudokuStrategy::SudokuStrategy( char* ) member class declaration.
 */
SudokuStrategy::SudokuStrategy( char* sudokuFileAddress )
{
    std::ifstream sudokuFileInput( sudokuFileAddress );
    
    if( sudokuFileInput.is_open() )
    {
        std::stringstream inputedPipeLineSudoku;
        
        // converts the ifstream "std::cin" to "std::stringstream" which natively supports
        // conversion to string.
        inputedPipeLineSudoku << inputedPipeLineSudoku.rdbuf();
        
        this->createRandomSudoku();
        this->processInputSudoku( inputedPipeLineSudoku.str() );
        
        sudokuFileInput.close();
    }
    else
    {
        DEBUGGER( stderr, "ERROR: %s! While opening the file: %s", strerror( errno ), sudokuFileAddress );
    }
}

/**
 * @see SudokuStrategy::~SudokuStrategy() member class declaration.
 */
SudokuStrategy::~SudokuStrategy()
{
}

/**
 * @see SudokuStrategy::createRandomSudoku() member class declaration.
 */
void SudokuStrategy::createRandomSudoku()
{
    // To give a different seed for rand().
    srand ( time(NULL) );
    
    for( int i = 0; i < 9; i++ )
    {
        for( int j = 0; j < 9; j++ )
        {
            g_sudokuVectorMatrix[ i ][ j ] = rand() % 9 + 1;
        }
    }
}

/**
 * @see SudokuStrategy::toString() member class declaration.
 */
std::string SudokuStrategy::toString()
{
    std::stringstream sudokuText;
    
    for( int i = 0; i < 9; i++ )
    {
        for( int j = 0; j < 9; j++ )
        {
            sudokuText << std::to_string( g_sudokuVectorMatrix[i][j] ) << ' ';
        }
        
        sudokuText << '\n';
    }
    
    return sudokuText.str();
}

/**
 * @see SudokuStrategy::processInputSudoku() member class declaration.
 */
void SudokuStrategy::processInputSudoku( std::string sudokuText )
{
    int currentLine    = 0;
    int currentColumn  = 0;
    
    for( char currentChar : sudokuText )
    {
        if( isdigit( currentChar )
            && currentLine < 9
            && currentColumn < 9 )
        {
            g_sudokuVectorMatrix[ currentLine ][ currentColumn ] = currentChar - '0';
            
            DEBUGGERN( stdout, "[%i,%i]%i", currentLine, currentColumn,
                    g_sudokuVectorMatrix[ currentLine ][ currentColumn ] );
            
            ++currentColumn;
            
            if( currentColumn > 8 )
            {
                ++currentLine;
                
                currentColumn = 0;
            }
        }
        else
        {
            // ignore unrecognized character
            DEBUGGERN( stdout, "%c", currentChar );
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
    bool computeSudoku() override;
    
    
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
        DEBUGGER( stdout, "Creating thread %d...", i );
        
        data->currentElement    = i;
        data->indexesArray[ i ] = i;
        
        if( pthread_create( &t[ i ], NULL, startThread, data ) != 0 )
        {
            DEBUGGER( stderr, "Failed to create thread %d! %s", i, strerror( errno ) );
        }
    }
    
    for( int i = 0; i < n; i++ )
    {
        pthread_join( t[ i ], NULL );
        
        DEBUGGER( stdout, "Thread %d has joined.", i );
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
 * Calculates a static array size.
 */
#if !defined STATIC_ARRAY_SIZE
    #define STATIC_ARRAY_SIZE( array ) ( sizeof( ( array ) ) / sizeof( ( array )[0] ) )
    
#endif


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
    // Uninitialized pointers cannot be deleted, but NULL pointers can. Then initializes it to be
    // safely deleted latter.
    SudokuStrategy* sudokus[ 3 ] = { NULL };
    
    // If it is passed input throw the terminal pipe line, get it.
    if( !isatty( fileno( stdin ) ) )
    {
        std::stringstream inputedPipeLineSudoku;
        
        // converts the ifstream "std::cin" to "std::stringstream" which natively supports
        // conversion to string.
        inputedPipeLineSudoku << std::cin.rdbuf();
        
        sudokus[ 0 ] = new SudokuStrategyWith9Threads( inputedPipeLineSudoku.str() );
    }
    else
    {
        sudokus[ 0 ] = new SudokuStrategyWith9Threads();
    }
    
    if( argumentsCount == 2 )
    {
        sudokus[ 1 ] = new SudokuStrategyWith9Threads( argumentsStringList[ 1 ] );
    }
    else
    {
        sudokus[ 1 ] = new SudokuStrategyWith9Threads();
        
        std::cout << std::endl;
    }
    
    sudokus[ 2 ] = new SudokuStrategyWith9Threads();
    
    sudokus[ 2 ]->createRandomSudoku();
    
    for( auto sudoku : sudokus )
    {
        if( sudoku->computeSudoku() )
        {
            std::cout << std::endl << sudoku->toString();
            std::cout << "This sudoku is a valid solution!" << std::endl;
        }
        else
        {
            std::cout << std::endl << sudoku->toString();
            std::cout << "This sudoku is NOT a valid solution!" << std::endl;
        }
        
        std::cout << std::endl;
    }
    
    for( int currentPointer = 0; currentPointer < STATIC_ARRAY_SIZE( sudokus ); ++currentPointer )
    {
        DEBUGGER( stdout, "Deleting currentPointer: %d", currentPointer );
        
        delete sudokus[ currentPointer ];
    }
    
    return EXIT_SUCCESS;
}






















