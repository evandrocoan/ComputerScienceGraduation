/**
 * Compile and link with -pthread.
 */


#include <iostream>
#include <pthread.h>
#include <cstdlib>
#include <stdio.h>
#include <string.h>


/** This is to view internal program data while execution. Default value: 0
 *
 * 0   - Disables this feature.
 * 1   - Normal debug.
 */
#define DEBUG_LEVEL 0

#define MAX_FOR_LOOPS_TO_INCREMENT_THE_GLOBAL_VARIABLE 100


#if DEBUG_LEVEL > 0
    #define DEBUG

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
pthread_mutex_t xGlobalVariableMutex;

// The x global variable requested by the teacher.
int xGlobalVariableInteger = 0;

// Functions prototypes
void *incrementTheGlobalVariable(void *);
void *decrementTheGlobalVariable(void *);

using namespace std;


/** 
 * Para pensar:
 * 
 * Na implementação computacional 1 (IC 1) o que acontecia com a variável global 
 * count quando ela era incrementada pelos diferentes processos?
 *     Bem, com qual delas? Cada processo tinha sua própria variável 'count' copiada a partir da
 *     original no momento do fork. Cada um incrementava sua própria cópia da variable count, o que
 *     não afeta a variável original do processo pai.
 * 
 * O mesmo vai acontecer agora quando a variável global x for incrementada e decrementada pelos
 * diferentes threads?
 *     Ela vai incrementada e decrementada como se tudo fosse um único programa, desde que sejam
 *     evitados as condições de corrida.
 * 
 * Qual é o valor esperado para a variável x após o término do aplicativo?
 *     É esperado que ela seja 0.
 * 
 * Se não houver mutex, qual será o valor final da variável global x (ou sua distribuição de
 * probabilidade)?
 *     Ela pode estar entre -100 e +100, dependendo da operação do escalonador de processos do sistema
 *     operacional.
 *     Por exemplo, se toda vez que thread que decrementa ler 0, for escrever -1, mas
 *     no meio do processo ser colocada em espera, e a thread the incrementa, for colocada para
 *     executar e ler 0, e escreve 1, e então é colocada em espera, e a thread que decrementa voltar
 *     e continuar a operação de onde parou, e então escreve -1, lê ele novamente, e é mais uma
 *     vez colocar em espera pelo escalonador, e a thread que incrementa é colocada para executar
 *     mais uma vez, e agora lê -1, e escreve 0 e então é colocada em espera, e a thread que
 *     decrementa volta a executar mais uma vez e continua de onde parou, isto é, a escrever o
 *     valor de -2.
 *     Continuando assim sem exclusão mútua, podemos chegar a ter -100 ou +100 ou qualquer valor
 *     entre essa faixa. Claro, que tal chance real de acontecer algo assim na vida real é muito
 *     pequena. E mais precisamente, precisaria-se de um escalonador muito sacana.
 */
int main()
{
    int errno;
    
    // Print to the standard output stream
	cout << "x: " << xGlobalVariableInteger << endl;
    
	// Declare threads
    pthread_t imcrementTheGlobalVariableThread;
    pthread_t decremmentTheGlobalVariableThread;
    
    // Print like function for logging used when the DEBUG_LEVEL is set to greater than 0.
    DEBUGGER( stdout, "We are about to initialize the mutex." );
    
    // Init mutex. This function shall return zero; otherwise, an error number shall be returned to
    // indicate the error.
    //
    // '&xGlobalVariableMutex'
    // It is the mutex address to initialize.
    // 
    // 'NULL'
    // To use the default mutex attributes where upon successful initialization, the state of the 
    // mutex becomes initialized and unlocked.
    // 
    if( ( errno = pthread_mutex_init( &xGlobalVariableMutex, NULL ) ) != 0 )
    {
        // Print like function for logging used when the DEBUG_LEVEL is set to greater than 0.
        DEBUGGER( stderr, "ERROR! Could not initialize the mutex! %s", strerror( errno ) );
        
        // Exits the program using a platform portable failure exit status.
        return EXIT_FAILURE;
    }
    
    // Print like function for logging used when the DEBUG_LEVEL is set to greater than 0.
    DEBUGGER( stdout, "We are about to create a first thread which executes "
            "'incrementTheGlobalVariable'." );
    
    // Create a first thread which executes 'incrementTheGlobalVariable'. On success, returns 0; 
    // on error, it returns an error number, and the contents of 'imcrementTheGlobalVariableThread'
    // are undefined.
    // 
    // 'imcrementTheGlobalVariableThread'
    // The pointer to the ID of the new thread created. This identifier is used to refer to the
    // thread in subsequent calls to other pthreads functions.
    // 
    // 'NULL'
    // The thread is created with default attributes. Attributes are specified only at thread 
    // creation time; they cannot be altered while the thread is being used. Where the attibute 
    // initialisation -- pthread_attr_init() create a default 'pthread_attr_t' attr. Example:
    // PTHREAD_CREATE_JOINABLE, Exit status and thread are preserved after the thread terminates.
    // 
    // 'incrementTheGlobalVariable'
    // This is a pointer to the function to call when the thread starts running.
    // 
    // 'xGlobalVariableInteger'
    // This is the pointer to argument to be passed to the function to call.
    // 
    if( errno = pthread_create( &imcrementTheGlobalVariableThread, NULL, incrementTheGlobalVariable,
            &xGlobalVariableInteger ) )
    {
        // Print like function for logging used when the DEBUG_LEVEL is set to greater than 0.
        DEBUGGER( stderr, "ERROR! Could not to create the thread! %s", strerror( errno ) );
        
        // Exits the program using a platform portable failure exit status.
        return EXIT_FAILURE;
    }
    
    // Print like function for logging used when the DEBUG_LEVEL is set to greater than 0.
    DEBUGGER( stdout, "We are about to create a second thread which executes "
            "'decrementTheGlobalVariable'." );
    
    // Create a second thread which executes 'decrementTheGlobalVariable'. On success, returns 0; 
    // on error, it returns an error number, and the contents of 'decremmentTheGlobalVariableThread'
    // are undefined.
    // 
    // 'decremmentTheGlobalVariableThread'
    // The pointer to the ID of the new thread created. This identifier is used to refer to the
    // thread in subsequent calls to other pthreads functions.
    // 
    // 'NULL'
    // The thread is created with default attributes. Attributes are specified only at thread 
    // creation time; they cannot be altered while the thread is being used. Where the attribute 
    // initialisation -- pthread_attr_init() create a default 'pthread_attr_t' attr. Example:
    // PTHREAD_CREATE_JOINABLE, Exit status and thread are preserved after the thread terminates.
    // 
    // 'decrementTheGlobalVariable'
    // This is a pointer to the function to call when the thread starts running.
    // 
    // 'xGlobalVariableInteger'
    // This is the pointer to argument to be passed to the function to call.
    // 
    if( errno = pthread_create( &decremmentTheGlobalVariableThread, NULL, decrementTheGlobalVariable,
            &xGlobalVariableInteger ) )
    {
        // Print like function for logging used when the DEBUG_LEVEL is set to greater than 0.
        DEBUGGER( stderr, "ERROR! Could not to create the thread! %s", strerror( errno ) );
        
        // Exits the program using a platform portable failure exit status.
        return EXIT_FAILURE;
    }
    
    // Print like function for logging used when the DEBUG_LEVEL is set to greater than 0.
    DEBUGGER( stdout, "We are about to wait for the first thread to finish." );
    
    // Wait for the first thread to finish. The this function waits for the thread specified by
    // thread to terminate. If that thread has already terminated, then pthread_join() returns
    // immediately. On success, pthread_join() returns 0; on error, it returns an error number.
    // 
    // 'imcrementTheGlobalVariableThread'
    // This is the thread id to wait.
    // 
    // 'NULL'
    // If is not NULL, then pthread_join() copies the exit status of the target thread
    // (i.e., the value that the target thread supplied to pthread_exit(3)) into the location
    // pointed to by. If the target thread was canceled, then PTHREAD_CANCELED is placed in.
    // 
    if( ( errno = pthread_join( imcrementTheGlobalVariableThread, NULL ) ) != 0 )
    {
        // Print like function for logging used when the DEBUG_LEVEL is set to greater than 0.
        DEBUGGER( stderr, "ERROR! Could not wait the thread %lu to exit! %s",
                imcrementTheGlobalVariableThread, strerror( errno ) );
        
        // Exits the program using a platform portable failure exit status.
        return EXIT_FAILURE;
    }
    
    // Print like function for logging used when the DEBUG_LEVEL is set to greater than 0.
    DEBUGGER( stdout, "We are about to wait for the second thread to finish." );
    
    // Wait for the second thread to finish. The this function waits for the thread specified by
    // thread to terminate. If that thread has already terminated, then pthread_join() returns
    // immediately. On success, pthread_join() returns 0; on error, it returns an error number.
    // 
    // 'decremmentTheGlobalVariableThread'
    // This is the thread id to wait.
    // 
    // 'NULL'
    // If is not NULL, then pthread_join() copies the exit status of the target thread
    // (i.e., the value that the target thread supplied to pthread_exit(3)) into the location
    // pointed to by. If the target thread was canceled, then PTHREAD_CANCELED is placed in.
    // 
    if( ( errno = pthread_join( decremmentTheGlobalVariableThread, NULL ) ) != 0 )
    {
        // Print like function for logging used when the DEBUG_LEVEL is set to greater than 0.
        DEBUGGER( stderr, "ERROR! Could not wait the thread %lu to exit! %s",
                decremmentTheGlobalVariableThread, strerror( errno ) );
        
        // Exits the program using a platform portable failure exit status.
        return EXIT_FAILURE;
    }
    
	// Destroy mutex. Function shall return zero; otherwise, an error number shall be returned to
    // indicate the error.
    // 
    // '&xGlobalVariableMutex'
    // It Is the mutex address to destroy.
    // 
    if( ( errno =  pthread_mutex_destroy( &xGlobalVariableMutex ) ) != 0 )
    {
        // Print like function for logging used when the DEBUG_LEVEL is set to greater than 0.
        DEBUGGER( stderr, "ERROR! Could not destroy the mutex! %s", strerror( errno ) );
        
        // Exits the program using a platform portable failure exit status.
        return EXIT_FAILURE;
    }
    
    // Print to the standard output stream
	cout << "x: " << xGlobalVariableInteger << endl;
    
    // Print like function for logging used when the DEBUG_LEVEL is set to greater than 0.
    DEBUGGER( stdout, "Exits the program using a platform portable successful exit status." );
    
    // Exits the program using a platform portable successful exit status.
	return EXIT_SUCCESS;
}

/** Increment the xGlobalVariableInteger to MAX_FOR_LOOPS_TO_INCREMENT_THE_GLOBAL_VARIABLE.
 * 
 * @param xGlobalVariableVoidPointer      a void pointer to the variable to increment until
 *                                        MAX_FOR_LOOPS_TO_INCREMENT_THE_GLOBAL_VARIABLE.
 * 
 * @return a void pointer to a zero int value.
 */
void *incrementTheGlobalVariable(void *xGlobalVariableVoidPointer)
{
	int *xGlobalVariableIntegerPointer = (int *) xGlobalVariableVoidPointer;
    
	for( int currentForIndex = 0; 
         currentForIndex < MAX_FOR_LOOPS_TO_INCREMENT_THE_GLOBAL_VARIABLE; ++currentForIndex )
	{
		// Enter critical region. xGlobalVariableMutex is the mutex.
        pthread_mutex_lock( &xGlobalVariableMutex );
        
        ++( *xGlobalVariableIntegerPointer );
        
		DEBUGGER( stdout, "Incrementing: %d", *xGlobalVariableIntegerPointer );
        
        // Leave critical region. xGlobalVariableMutex is the mutex.
        pthread_mutex_unlock( &xGlobalVariableMutex );
	}
    
	cout << "increment finished" << endl;
    
    return NULL;
}

/** Increment the xGlobalVariableInteger to MAX_FOR_LOOPS_TO_INCREMENT_THE_GLOBAL_VARIABLE.
 * 
 * @param xGlobalVariableVoidPointer      a void pointer to the variable to increment until
 *                                        MAX_FOR_LOOPS_TO_INCREMENT_THE_GLOBAL_VARIABLE.
 * 
 * @return a void pointer to a zero int value.
 */
void *decrementTheGlobalVariable(void *xGlobalVariableVoidPointer)
{
    /* decrement x to MAX_FOR_LOOPS_TO_INCREMENT_THE_GLOBAL_VARIABLE */
    int *xGlobalVariableIntegerPointer = (int *) xGlobalVariableVoidPointer;
    
	for( int currentForIndex = 0;
         currentForIndex < MAX_FOR_LOOPS_TO_INCREMENT_THE_GLOBAL_VARIABLE; ++currentForIndex )
	{
		// Enter critical region. xGlobalVariableMutex is the mutex.
        pthread_mutex_lock( &xGlobalVariableMutex );
        
        --( *xGlobalVariableIntegerPointer );
        
		DEBUGGER( stdout, "Decrementing: %d", *xGlobalVariableIntegerPointer );
        
        // Leave critical region. xGlobalVariableMutex is the mutex.
        pthread_mutex_unlock( &xGlobalVariableMutex );
	}
    
    cout << "decrement finished" << endl;
    
    return NULL;
}
