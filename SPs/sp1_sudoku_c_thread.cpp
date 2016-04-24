
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



/**
 * Preprocessor directive designed to cause the current source file to be included only once in a
 * single compilation. Thus, serves the same purpose as #include guards, but with several
 * advantages, including: less code, avoidance of name clashes, and sometimes improvement in
 * compilation speed. In main file this is enabled by default.
 */
// #pragma once


/**
 * Calculates a static array size.
 */
#if !defined STATIC_ARRAY_SIZE
    #define STATIC_ARRAY_SIZE( array ) ( sizeof( ( array ) ) / sizeof( ( array )[0] ) )
    
#endif


/** This is to view internal program data while execution. Default value: 0
 *
 * 0   - Disables this feature.
 * 1   - Normal debug.
 */
#define DEBUG_LEVEL 0


#if DEBUG_LEVEL > 0
    #define DEBUG


/**
 * 0   - Disabled all debug.
 * 1   - Basic debug messages.
 * 2   - Thread creation messages.
 * 4   - Functions entrances.
 * 8   - Thread internal processing.
 * 
 * 15  - Enables all debugging levels (1111).
 */
const int g_debugLevel = 1 + 4;

/**
 * Mutex used by the DEBUGGER print function for synchronized print from multi-threading.
 */
pthread_mutex_t g_fprintf_mutex;


/**
 * The same as DEBUGGERLN(...) just below, but do not put automatically a new line.
 */
#define DEBUGGER( level, ... ) \
do \
{ \
    pthread_mutex_lock( &g_fprintf_mutex ); \
    \
    if( level & g_debugLevel ) \
    { \
        fprintf( stdout, ##__VA_ARGS__ ); \
        fflush( stdout ); \
    } \
    \
    pthread_mutex_unlock( &g_fprintf_mutex ); \
} \
while( 0 )

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
#define DEBUGGERLN( level, ... ) \
do \
{ \
    pthread_mutex_lock( &g_fprintf_mutex ); \
    \
    if( level & g_debugLevel ) \
    { \
        fprintf( stdout, ##__VA_ARGS__ ); \
        fprintf( stdout, "\n" ); \
        fflush( stdout ); \
    } \
    \
    pthread_mutex_unlock( &g_fprintf_mutex ); \
} \
while( 0 )

/**
 * The same as DEBUGGER(...), but it is for standard program output.
 */
#define FPRINT( stream, ... ) \
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

/**
 * The same as DEBUGGERLN(...), but it is for standard program output.
 */
#define FPRINTLN( stream, ... ) \
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


#else
    #define DEBUGGER( stream, ... )
    #define DEBUGGERLN( stream, ... )

/**
 * The same as DEBUGGER(...), but it is for standard program output and is not multi-thread safe.
 */
#define FPRINT( stream, ... ) \
do \
{ \
    fprintf( stream, ##__VA_ARGS__ ); \
    fflush( stream ); \
} \
while( 0 )

/**
 * The same as DEBUGGERLN(...), but it is for standard program output and is not multi-thread safe.
 */
#define FPRINTLN( stream, ... ) \
do \
{ \
    fprintf( stream, ##__VA_ARGS__ ); \
    fprintf( stream, "\n" ); \
    fflush( stream ); \
} \
while( 0 )


#endif



/**
 * Preprocessor directive designed to cause the current source file to be included only once in a
 * single compilation. Thus, serves the same purpose as #include guards, but with several
 * advantages, including: less code, avoidance of name clashes, and sometimes improvement in
 * compilation speed. In main file this is enabled by default.
 */
// #pragma once


/**
 * This is an abstract class to represent a complete sudoku input and offer an basic structure
 * to verify whether it is a valid sudoku. It is processed any way to input a complete sudoku and
 * let to the concrete class which implements this to handle the sudoku solution verification.
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
     * To erases the current loaded sudoku and to creates a new zeroed sudoku.
     */
    void emptyTheSudoku();
    
    /**
     * Creates a string representation of the current loaded sudoku.
     * 
     * @return an std::string object.
     */
    std::string toString();
    
    /**
     * Verifies the current loaded sudoku solution the desired strategy.
     * 
     * @return true if the current loaded sudoku is a valid solution, false otherwise.
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
     * 8 2 7 ,  some space  1 5 4,    3 9 (this sudoku is solved, if you change this 0 by six).
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
    DEBUGGERLN( 3, "Calling the SudokuStrategy() constructor." );
}

/**
 * @see SudokuStrategy::SudokuStrategy( char* ) member class declaration.
 */
SudokuStrategy::SudokuStrategy( std::string sudokuText )
{
    DEBUGGERLN( 3, "Calling the SudokuStrategy( std::string sudokuText ) constructor." );
    
    // Clear the default sudoku and process the new one.
    this->emptyTheSudoku();
    this->processInputSudoku( sudokuText );
}

/**
 * @see SudokuStrategy::SudokuStrategy( char* ) member class declaration.
 */
SudokuStrategy::SudokuStrategy( char* sudokuFileAddress )
{
    DEBUGGERLN( 3, "Calling the SudokuStrategy( char* sudokuFileAddress ) constructor." );
    
    std::ifstream sudokuFileInput( sudokuFileAddress );
    
    if( sudokuFileInput.is_open() )
    {
        std::stringstream inputedPipeLineSudoku;
        
        // Converts the std::fstream "std::cin" to std::stringstream which natively supports
        // conversion to string.
        inputedPipeLineSudoku << sudokuFileInput.rdbuf();
        
        // Clear the default sudoku and process the new one.
        this->emptyTheSudoku();
        this->processInputSudoku( inputedPipeLineSudoku.str() );
        
        sudokuFileInput.close();
    }
    else
    {
        FPRINTLN( stderr, "ERROR: %s! While opening the file: %s", strerror( errno ), sudokuFileAddress );
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
 * @see SudokuStrategy::createRandomSudoku() member class declaration.
 */
void SudokuStrategy::emptyTheSudoku()
{
    for( int i = 0; i < 9; i++ )
    {
        for( int j = 0; j < 9; j++ )
        {
            g_sudokuVectorMatrix[ i ][ j ] = 0;
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
    int lineIndex    = 0;
    int columnIndex  = 0;
    
    for( char currentChar : sudokuText )
    {
        if( isdigit( currentChar )
            && lineIndex < 9
            && columnIndex < 9 )
        {
            g_sudokuVectorMatrix[ lineIndex ][ columnIndex ] = currentChar - '0';
            
            DEBUGGER( 1, "[%i,%i]%i", lineIndex, columnIndex,
                    g_sudokuVectorMatrix[ lineIndex ][ columnIndex ] );
            
            ++columnIndex;
            
            if( columnIndex > 8 )
            {
                ++lineIndex;
                
                columnIndex = 0;
            }
        }
        else
        {
            // ignore unrecognized character
            DEBUGGER( 1, "%c", currentChar );
        }
    }
}



/**
 * Preprocessor directive designed to cause the current source file to be included only once in a
 * single compilation. Thus, serves the same purpose as #include guards, but with several
 * advantages, including: less code, avoidance of name clashes, and sometimes improvement in
 * compilation speed. In main file this is enabled by default.
 */
// #pragma once


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
        int                         threadIndex;
        SudokuStrategyWith9Threads* thisSudoku;
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
    // Now create the thread passing it data as a parameter
    const int   n          = 9;
    parameters* datas[ n ] = { NULL };
    
    pthread_t t[ n ];
    
    DEBUGGER( 2, "\n" );
    
    for( int i = 0; i < n; i++ )
    {
        DEBUGGERLN( 2, "Creating thread %d...", i );
        
        datas[ i ]              = (parameters *) malloc( sizeof( parameters ) );
        datas[ i ]->thisSudoku  = this;
        datas[ i ]->threadIndex = i;
        
        if( pthread_create( &t[ i ], NULL, startThread, datas[ i ] ) != 0 )
        {
            FPRINTLN( stderr, "Failed to create thread %d! %s", i, strerror( errno ) );
        }
    }
    
    for( int i = 0; i < n; i++ )
    {
        pthread_join( t[ i ], NULL );
        
        DEBUGGERLN( 2, "Thread %d has joined.", i );
    }
    
    return this->works;
}

/**
 * @see SudokuStrategyWith9Threads::startThread( void* ) member class declaration.
 */
void* SudokuStrategyWith9Threads::startThread( void* voidArgumentPointer )
{
    parameters* data = static_cast< parameters* >( voidArgumentPointer );
    
    data->thisSudoku->verify( data->threadIndex );
    
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
 * Preprocessor directive designed to cause the current source file to be included only once in a
 * single compilation. Thus, serves the same purpose as #include guards, but with several
 * advantages, including: less code, avoidance of name clashes, and sometimes improvement in
 * compilation speed. In main file this is enabled by default.
 */
// #pragma once

/**
 * Constants used.
 */
#define NUMBER_OF_THRHEADS 27


/**
 * Implements the abstract class SudokuStrategy sudoku solution algorithm using POSIX 9 threads.
 */
class SudokuStrategyWith27Threads : public SudokuStrategy
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
    bool g_isValid[ NUMBER_OF_THRHEADS ] = { false };
    
    /**
     * Structure for passing data to threads.
     */
    struct parameters
    {
        int                          threadIndex;
        SudokuStrategyWith27Threads* thisSudoku;
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
    static void* startThread( void* );
    
    /**
     * Performs the current sodoku solutions check
     * 
     * @param threadIndex              the current thread index on the boolean array g_isValid.
     */
    void verify( int );
    
};


/**
 * @see SudokuStrategyWith27Threads::computeSudoku()
 */
bool SudokuStrategyWith27Threads::computeSudoku()
{
    pthread_t workersThreads[ NUMBER_OF_THRHEADS ];
    
    // Now create the thread passing it data as a parameter
    bool        isValidSudoku               = true;
    parameters* datas[ NUMBER_OF_THRHEADS ] = { NULL };
    
    DEBUGGER( 2, "\n" );
    
    // To create the workers threads.
    for( int threadIndex = 0; threadIndex < NUMBER_OF_THRHEADS; threadIndex++ )
    {
        DEBUGGERLN( 2, "Creating thread %d...", threadIndex );
        
        datas[ threadIndex ]              = (parameters *) malloc( sizeof( parameters ) );
        datas[ threadIndex ]->thisSudoku  = this;
        datas[ threadIndex ]->threadIndex = threadIndex;
        
        if( pthread_create( &workersThreads[ threadIndex ], NULL, startThread, datas[ threadIndex ] ) != 0 )
        {
            FPRINTLN( stderr, "Failed to create thread %d! %s", threadIndex, strerror( errno ) );
        }
    }
    
    // Wait for all thread to finish.
    for( int threadIndex = 0; threadIndex < NUMBER_OF_THRHEADS; threadIndex++ )
    {
        pthread_join( workersThreads[ threadIndex ], NULL );
        
        DEBUGGERLN( 2, "Thread %d has joined.", threadIndex );
    }
    
    DEBUGGER( 1, "\n" );
    
    // Compute the workers threads result.
    for( int resultIndex = 0; resultIndex < STATIC_ARRAY_SIZE( g_isValid ); ++resultIndex )
    {
        DEBUGGER( 1, "%d", g_isValid[ resultIndex ] );
        
        if( !g_isValid[ resultIndex ] )
        {
            isValidSudoku = false;
        }
    }
    
    return isValidSudoku;
}

/**
 * @see SudokuStrategyWith27Threads::startThread( void* ) member class declaration.
 */
void* SudokuStrategyWith27Threads::startThread( void* voidArgumentPointer )
{
    parameters* data = static_cast< parameters* >( voidArgumentPointer );
    
    data->thisSudoku->verify( data->threadIndex );
    
    return NULL;
}

/**
 * @see SudokuStrategyWith27Threads::verify() member class declaration.
 */
void SudokuStrategyWith27Threads::verify( int threadIndex )
{
    int sum = 0;
    
    DEBUGGERLN( 8, "Calling SudokuStrategyWith27Threads::verify, threadIndex: %d", threadIndex );
    
    /**
     * Threads to check each line.
     */
    if( threadIndex < 9 )
    {
        int lineIndex = threadIndex % 9;
        
        for( int columnIndex = 0; columnIndex < 9; columnIndex++ )
        {
            sum += g_sudokuVectorMatrix[ lineIndex ][ columnIndex ];
        }
        
        DEBUGGERLN( 8, "threadIndex: %d, lineIndex: %d, sum: %d ", threadIndex, lineIndex, sum );
        
        if( sum == 45 )
        {
            g_isValid[ threadIndex ] = true;
        }
    }
    /**
     * Threads to check each column.
     */
    else if( 8 < threadIndex < 18 )
    {
        int columnIndex = threadIndex % 9;
        
        for( int lineIndex = 0; lineIndex < 9; lineIndex++ )
        {
            sum += g_sudokuVectorMatrix[ lineIndex ][ columnIndex ];
        }
        
        DEBUGGERLN( 8, "threadIndex: %d, columnIndex: %d, sum: %d ", threadIndex, columnIndex, sum );
        
        if( sum == 45 )
        {
            g_isValid[ threadIndex ] = true;
        }
    }
    /**
     * Threads to check each quadrant.
     */
    else if( 17 < threadIndex < 27 )
    {
        int quadrantIndex      = threadIndex % 9;
        int horizontalQuadrant = quadrantIndex / 3;
        int verticalQuadrant   = quadrantIndex % 3;
        
        for( int quadrantLine = 0; quadrantLine < 3; quadrantLine++ )
        {
            for( int quadrantColumn = 0; quadrantColumn < 3; quadrantColumn++ )
            {
                sum += g_sudokuVectorMatrix[ horizontalQuadrant * 3 + quadrantLine ][ verticalQuadrant * 3 + quadrantColumn ];
            }
        }
        
        DEBUGGERLN( 8, "threadIndex: %d, quadrantIndex: %d, sum: %d ", threadIndex, quadrantIndex, sum );
        
        if( sum == 45 )
        {
            g_isValid[ threadIndex ] = true;
        }
    }
}



/**
 * To create a basic testing for different sudoku solution checker. This accept reading from the
 * terminal pipe line and form an argument line passed sudoku file name.
 * 
 * @param sodokus             a sudoku class "SudokuStrategy" pointer.
 * @param constructorName     the desired sudokus strategy constructor. Ex: SudokuStrategyWith27Threads
 */
#define createBasicSodukusTest( sudokus, constructorName ) \
do \
{ \
    /* If it is passed input throw the terminal pipe line, get it.*/ \
    if( isatty( fileno( stdin ) ) ) \
    { \
        sudokus[ 0 ] = new constructorName(); \
    } \
    else \
    { \
        /* Converts the std::fstream "std::cin" to std::stringstream which natively supports \
         * conversion to string.
         */ \
        inputedPipeLineSudoku << std::cin.rdbuf(); \
        \
        sudokus[ 0 ] = new constructorName( inputedPipeLineSudoku.str() ); \
    } \
    \
    if( argumentsCount >= 2 ) \
    { \
        sudokus[ 1 ] = new constructorName( argumentsStringList[ 1 ] ); \
    } \
    else \
    { \
        sudokus[ 1 ] = new constructorName(); \
    } \
    \
    sudokus[ 2 ] = new constructorName(); \
    \
    sudokus[ 2 ]->createRandomSudoku(); \
} \
while( 0 )

/**
 * To print the basic sudoku testing result to the standard output stream.
 * 
 * @param sudokus          a array of SudokuStrategy pointers.
 */
#define printBasicSudokuTestResults( sudokus ) \
do \
{ \
    for( auto sudoku : sudokus ) \
    { \
        if( sudoku->computeSudoku() ) \
        { \
            FPRINTLN( stdout, "\n%s" "This sudoku is a valid solution!", sudoku->toString().c_str() ); \
        } \
        else \
        { \
            FPRINTLN( stdout, "\n%s" "This sudoku is NOT a valid solution!", sudoku->toString().c_str() ); \
        } \
    } \
    \
    FPRINT( stdout, "\n" ); \
    \
    for( int pointerIndex = 0; pointerIndex < STATIC_ARRAY_SIZE( sudokus ); ++pointerIndex ) \
    { \
        DEBUGGERLN( 1, "Deleting pointerIndex: %d", pointerIndex ); \
        \
        delete sudokus[ pointerIndex ]; \
    } \
} \
while( 0 )


/**
 * Start the program execution and read the program argument list passed to it. This program
 * accept none or one command line argument. If passed, it must be an sudoku file path. See the
 * SudokuStrategy class documentation for the sudoku's text file structure. An example call to
 * this program could be:
 * cat sudoku_unsolved.txt | ./main.o sudoku_solved.txt
 * 
 * @param argumentsCount         one plus the argument counting passed to the program command line.
 * @param argumentsStringList    an argument list passed the program command line, where its first
 *                               string is current program execution path.
 * 
 * @return the <cstdlib> EXIT_SUCCESS on success, or EXIT_FAILURE on fail.
 */
int main( int argumentsCount, char* argumentsStringList[] )
{
    FPRINTLN( stdout, "Starting the SudokuStrategyWith9Threads tests!" );
    
    // Uninitialized pointers cannot be deleted, but NULL pointers can. Then initializes it to be
    // safely deleted latter.
    SudokuStrategy* sudokus[ 3 ] = { NULL };
    
    // As we only can catch from the terminal pipe to the std::cin its first time, declare it here
    // so it can be re-used latter.
    std::stringstream inputedPipeLineSudoku;
    
    // Use the SudokuStrategy with 9 threads to test.
    createBasicSodukusTest( sudokus, SudokuStrategyWith9Threads );
    printBasicSudokuTestResults( sudokus );
    
    // To add some spacing between the tests
    FPRINTLN( stdout, "\n\n\n\n\n\n\nStarting the SudokuStrategyWith27Threads tests!" );
    
    // Use the SudokuStrategy with 27 threads to test.
    createBasicSodukusTest( sudokus, SudokuStrategyWith27Threads );
    printBasicSudokuTestResults( sudokus );
    
    return EXIT_SUCCESS;
}























