/**
 * Compile and link with -pthread.
 */


#include <iostream>
#include <pthread.h>
#include <cstdlib>
#include <errno.h>


/** This is to view internal program data while execution. Default value: 0
 *
 * 0   - Disables this feature.
 * 1   - Normal debug.
 */
#define DEBUG_LEVEL 0


/** Print like function for logging putting a new line at the end of string. Following explanations:
 * 
 * fprintf( stream, __VA_ARGS__ ); Print to the specified output stream the formatting args.
 * fprintf( stream, "\n" );        Print a new line.
 * fflush( stream );               Flushes the output stream to avoid double output over '>'.
 *                                   Example: './main > results.txt' would get doubled/... print.
 * } while( 0 )                    To allow to use ';' semicolon over the macro statement use and
 *                                   still to be able to use it within an unbraced if statement.
 */
#define DEBUGGER( stream, ... ) \
{ \
    fprintf( stream, __VA_ARGS__ ); \
    fprintf( stream, "\n" ); \
    fflush( stream ); \
} while( 0 )

#else
    #define DEBUGGER( stream, ... )
    
#endif


// Declare a mutex pointer.
pthread_mutex_t *xGlobalVariableMutexLocker;

// The x global variable requested by the teacher.
int xGlobalVariable = 0;


using namespace std;

/** Increment the xGlobalVariable to 100.
 * 
 * @param xGlobalVariableVoidPointer      the variable to increment until 100.
 * @return 
 */
void *toIncrementTheGlobalVariable(void *xGlobalVariableVoidPointer)
{
	int *xGlobalVariableIntegerPointer = (int *) xGlobalVariableVoidPointer;
    
	for( int currentForIndex = 0; currentForIndex<100; currentForIndex++) 
	{
		// Enter critical region
        pthread_mutex_lock( xGlobalVariableMutexLocker );
        
		++( *xGlobalVariableIntegerPointer );
        
        // Leave critical region
        pthread_mutex_unlock( xGlobalVariableMutexLocker );
	}
    
	cout << "increment finished" << endl;
    
	return NULL;
}

void *toDecrementTheGlobalVariable(void *xGlobalVariableVoidPointer)
{
    /* decrement x to 100 */
    int *xGlobalVariableIntegerPointer = (int *)xGlobalVariableVoidPointer;
    
	for( int currentForIndex = 0; currentForIndex<100; currentForIndex++ )
	{
		/* enter critical region */
		--(*ptr);
		/* leave critical region */
	}
    
    cout << "decrement finished" << endl;
    return NULL;
}

/// Para pensar: Na implementação computacional 1 (IC 1) o que acontecia com a variável global 
/// count quando ela era incrementada pelos diferentes processos?
/// 
/// 
/// O mesmo vai acontecer agora quando a variável global x for incrementada e decrementada pelos
/// diferentes threads?
/// 
/// 
/// Qual é o valor esperado para a variável x após o término do aplicativo?
/// 
/// 
/// Se não houver mutex, qual será o valor final da variável global x (ou sua distribuição de
/// probabilidade)?
/// 
/// 
/// 
int main()
{
    int errno;
    
    // Print to the standard output stream
	cout << "x: " << x << endl;
    
	// Declare threads
    pthread_t *threadToImcremmentTheGlobalVariable;
    pthread_t *threadToDecremmentTheGlobalVariable;
    
    // Init mutex. This function shall return zero; otherwise, an error number shall be returned to
    // indicate the error.
    //
    // 'xGlobalVariableMutexLocker'
    // It is the mutex address to initialize.
    // 
    // 'NULL'
    // To use the default mutex attributes where upon successful initialization, the state of the 
    // mutex becomes initialized and unlocked.
    // 
    if( errno = pthread_mutex_init( xGlobalVariableMutexLocker, NULL ) != 0 )
    {
        // Print like function for logging used when the DEBUG_LEVEL is set to greater than 0.
        DEBUGGER( stdout, "ERROR! Could not initialize the mutex! Error code: %d", errno );
        
        // Exits the program using a platform portable failure exit status.
        return EXIT_FAILURE;
    }
    
    // Create a first thread which executes 'toIncrementTheGlobalVariable'. On success, returns 0; 
    // on error, it returns an error number, and the contents of 'threadToImcremmentTheGlobalVariable'
    // are undefined.
    // 
    // 'threadToImcremmentTheGlobalVariable'
    // The pointer to the ID of the new thread created. This identifier is used to refer to the
    // thread in subsequent calls to other pthreads functions.
    // 
    // 'NULL'
    // The thread is created with default attributes. 
    // 
    // 
    // 
    if( errno = pthread_create( threadToImcremmentTheGlobalVariable, NULL, toIncrementTheGlobalVariable, NULL ) )
    {
        // Print like function for logging used when the DEBUG_LEVEL is set to greater than 0.
        DEBUGGER( stdout, "ERROR! Could not to create the thread! Error code: %d", errno );
        
        // Exits the program using a platform portable failure exit status.
        return EXIT_FAILURE;
    }
    
	/* create a second thread which executes dec_(&x) */
    
    /* wait for the first thread to finish */
    
	/* wait for the second thread to finish */
    
	// Destroy mutex. Function shall return zero; otherwise, an error number shall be returned to
    // indicate the error.
    // 
    // 'xGlobalVariableMutexLocker'
    // It Is the mutex address to destroy.
    // 
    if( pthread_mutex_destroy( xGlobalVariableMutexLocker ) != 0 )
    {
        // Exits the program using a platform portable failure exit status.
        return EXIT_FAILURE;
    }
    
    // Print to the standard output stream
	cout << "x: " << x << endl;
    
    // Exits the program using a platform portable successful exit status.
	return EXIT_SUCCESS;
}
