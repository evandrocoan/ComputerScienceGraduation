
/**
 * Compile and link with -pthread. Nome do aluno:
 *
 * @author Evandro  Coan
 *
 */


#include <iostream>
#include <pthread.h>
#include <cstdlib>
#include <stdio.h>

// ... other includes ...
#include <string.h>
#include <stdlib.h>
#include <semaphore.h>
#include <unistd.h>


/** This is to view internal program data while execution. Default value: 0
 *
 * 0   - Disables this feature.
 * 1   - Normal debug.
 */
#define DEBUG_LEVEL 0


#if DEBUG_LEVEL > 0
    #define DEBUG

pthread_mutex_t g_fprintf_mutex;


/**
 * Print like function for logging putting a new line at the end of string. It does uses mutex
 * due the my doubt to know whether 'fprintf' is thread safe of not over every/any platforms, since
 * I could not find anything concrete. Following explanations:
 *
 * pthread_mutex_lock( g_fprintf_mutex );   Lock the mutex.
 * fprintf( stream, __VA_ARGS__ );          Print to the specified output stream the formatting args.
 * fprintf( stream, "\n" );                 Print a new line.
 * fflush( stream );                        Flushes the output stream to avoid double output over '>'.
 *                                           Example: './main > results.txt' would get doubled/... print.
 * pthread_mutex_unlock( g_fprintf_mutex ); Unlock the shared memory mutex.
 * } while( 0 )                             To allow to use ';' semicolon over the macro statement use and
 *                                           still to be able to use it within an unbraced if statement.
 */
#define DEBUGGER( stream, ... ) \
{ \
    pthread_mutex_lock( &g_fprintf_mutex ); \
    fprintf( stream, __VA_ARGS__ ); \
    fprintf( stream, "\n" ); \
    fflush( stream ); \
    pthread_mutex_unlock( &g_fprintf_mutex ); \
} while( 0 )

#else
    #define DEBUGGER( stream, ... )

#endif


// declare whenever global variables you need to synchronize pthreads using posix semaphores
#define MAX_BALLS_PER_CHILD                                  1
#define MAX_BALLS_THE_BASKET_SUPPORT                         3
#define MAX_BALLS_TO_INITIALLY_GIVE_TO_THE_CHILDREN          3
#define MAX_CHILD_THREADS_TO_PLAY                            7
#define MAX_TIMES_THE_CHILD_IS_ALLOWED_TO_PLAY_WITH_THE_BALL 5

int   errno;
sem_t g_emptyBasketBallPlaces;
sem_t g_filledBasketBallPlaces;

int g_childNumbers            [ MAX_CHILD_THREADS_TO_PLAY ];
int g_howManyBallsEachChildHas[ MAX_CHILD_THREADS_TO_PLAY ];


// Functions prototypes
void *childSimulatorFunction( void* );
void initializeTheSemaphores();
void toCreateTheThreadsToExecute( pthread_t* );
void waitTheThreadToExecute( pthread_t* );
void closesTheChildsGargen();

void takeABallFromTheBasketBall( unsigned short, unsigned short );
void dropABallInTheBasketBall( unsigned short );

using namespace std;


/**
 * Para pensar e para responder no codigo
 *
 *
 * Os semaforos podem ser variaveis locais?
 *         Variaveis locais de quem?
 *         1) Caso fossem do metodo main, poderia-se passar elas como parametro para as threads 
 *     (um ponteiro, obviamente), se e somente se o metodo main terminar depois que todas as
 *     threads terminarem tambem. Para passar esse parametro extra seria necessario emcapsular
 *     esse semafore junto do numero da crianca a executar, como em uma struct, e passar o /
 *     ponteiro dessa struct, que deve permanecer nao-destruida ate todas as thread terminarem
 *     suas execucoes.
 *         2) Caso fossem variabeis locacais de cada uma dessas threads, eles seriam completamente
 *     inuteis. Agora cada thread teria uma propria copia do semaforo, pois estao em enderecos
 *     de memoria diferentes e assim uma thread jamis poderia saber sobre a outra.
 *
 * Todas as threads "child" terminam? Justifique.
 *         Depende. Da forma que o problema esta apresentado sim. Por que as criancas (threads) tem 
 *     3 bolas e para terminarem precisam se livrar destas bolas colocando do cesto, que
 *     comporta 3 bolas.
 *         Agora, se voce dar mais bolas para as criancas do que o cesto comporta estas thread nao
 *     terminarao por que quando todas terminarem de brincar e forem guardar as bolas no cesto,
 *     nao havera vagas disponiveis, e estas criancas (thread) vao ficar esperando eternamente
 *     por uma vaga o cesto para terminarem.
 *
 * E se no cesto couber uma unica bola, como na versao original do problema?
 *         Caso caiba somente uma bola, as criancas somente podem ganhar uma bola, ou o programa
 *     nunca ira terminar por que no final da brincadeira, nao havera vagas para guardar as
 *     bolas e as criancas (threads) ficaram eternamente esperando para guardar o bola,
 *     quando todas elas terminarem de brincar.
 *         Mas caso elas tambem ganharem somente uma bola, as criancas (o programa e suas threads)
 *     irao demorar mais para terminar a execucao, por que agora somente uma crianca (de 7 crincas)
 *     brinca por vez, 5 vezes. 75 = 35 segundos com cada crianca brincando 1 segundo com a
 *     bola e desprezando o tempo de troca de bolas (colocar no cesto e pegar), o compartilhamento
 *     de tempo entre os outros processos do computador, e o overhead gasto com a sincrinizacao
 *     entre as diferentes threads (criancas).
 *
 * Ha algum erro de programacao para que as threads nao terminem?
 *         Nao, este codigo esta bem descrito para o caso descrito no problema apresentado, com a
 *     configuracao apresentada. Mas caso se abuse das contantes MAX_BALLS_THE_BASKET_SUPPORT e
 *     MAX_BALLS_TO_INITIALLY_GIVE_TO_THE_CHILDREN vai dar problema. O que se pode esperar ao colocar-se
 *     MAX_BALLS_THE_BASKET_SUPPORT = 1 e MAX_BALLS_TO_INITIALLY_GIVE_TO_THE_CHILDREN = 2?
 *     Que o programa execute e chegue a finalizar? Depende, atualmente nao faz-se o tratamento de tal
 *     caso, mas um simples linha de codigo com a seguir resolve esse problema:
 *         initialSemaphoreValue = (
 *                  MAX_BALLS_TO_INITIALLY_GIVE_TO_THE_CHILDREN > MAX_BALLS_THE_BASKET_SUPPORT 
 *                  ? MAX_BALLS_THE_BASKET_SUPPORT : initialSemaphoreValue );
 *         Pois caso o valor calculado para ser o valor inicial do semaforo ultrapasse o numero de
 *     de bolas que o sistema pode desfazer-se no fim, limitamos ele ao numero maximo de bolas
 *     da cesta. Isso nao esta implementado por que nao foi pedido nenhum controle, e com isso,
 *     nao poderiamos ver o sistema travar colocando valores absurdos como explicados acima.
 *
 * O que poderia ser feito para detectar que um evento nunca ocorrera e fazer com que o programa
 * finalize com sucesso, encerrando todas suas threads?
 *         De um timeout razoavel/suficiente para cada thread esperar enquanto ela aguarda a 
 *     liberacao do semaforo, e caso ultrapasse esse tempo limite, ela finaliza.
 *         Esse sistema de controle nao possui implementado por que nao foi pedido para esta
 *     solucao de problema.
 *
 */
int main()
{
    cout << "The kindengarten is open" << endl;
    
    // declare local variable, such as threads
    pthread_t childSimulatorThreads[ MAX_CHILD_THREADS_TO_PLAY ];
    
    initializeTheSemaphores();
    toCreateTheThreadsToExecute( childSimulatorThreads );
    waitTheThreadToExecute( childSimulatorThreads );
    closesTheChildsGargen();
    
    // Print like function for logging used when the DEBUG_LEVEL is set to greater than 0.
    DEBUGGER( stdout, "Exits the program using a platform portable successful exit status." );
    
    // Exits the program using a platform portable successful exit status.
    return EXIT_SUCCESS;
    
    /** Respostas das perguntas "para pensar":
     *
     * @see function main() documentation.
     */
}

/**
 * Initializes the semaphores 'g_emptyBasketBallPlaces' and
 * 'g_filledBasketBallPlaces' to be used over the child's ball problem, accordingly
 * with 'MAX_BALLS_TO_INITIALLY_GIVE_TO_THE_CHILDREN' and 'MAX_CHILD_THREADS_TO_PLAY'.
 */
void initializeTheSemaphores()
{
#if defined DEBUG
    
    // Initializes the printf mutex for use. Specifies NULL to use the default mutex attributes.
    if( errno = pthread_mutex_init( &g_fprintf_mutex, NULL ) )
    {
        // Print to the standard output stream
        DEBUGGER( stderr, "\nERROR! Could initialize the mutex! %s", strerror( errno ) );
        
        exit( EXIT_FAILURE );
    }
#endif
    
    // Print like function for logging used when the DEBUG_LEVEL is set to greater than 0.
    DEBUGGER( stdout, "We are about to initialize the semaphores to synchronization." );
    
    // init semaphores to synchronize the threads
    //
    // 'g_emptyBasketBallPlaces'
    // The semaphore to initialize.
    //
    // int '0'
    // Indicates whether this semaphore is to be shared between the threads of a process, or
    // between processes. If 0, then the semaphore is shared between the threads of a process,
    // and should be located at some address that is visible to all threads (e.g., a global
    // variable, or a variable allocated dynamically on the heap).
    //
    // 'initialSemaphoreValue'
    // The value argument specifies the initial value for the semaphore. The balls are initially
    // taken by some children, but if there are more balls than children initializes with how
    // many balls there are available.
    //
    bool         areThereEmptyPlaces;
    bool         areThereRemainningBalls;
    int          remainingBalls;
    unsigned int initialSemaphoreValue;
    
    remainingBalls          = MAX_BALLS_TO_INITIALLY_GIVE_TO_THE_CHILDREN - MAX_CHILD_THREADS_TO_PLAY;
    areThereRemainningBalls = remainingBalls > 0;
    initialSemaphoreValue   = ( areThereRemainningBalls ? remainingBalls : 0 );
    
    if( sem_init( &g_emptyBasketBallPlaces, 0, initialSemaphoreValue ) != 0 )
    {
        // Print like function for logging used when the DEBUG_LEVEL is set to greater than 0.
        DEBUGGER( stderr, "ERROR! Could not to initialize the semaphore! %s", strerror( errno ) );
        
        // Exits the program using a platform portable failure exit status.
        exit( EXIT_FAILURE );
    }
    
    //
    // This specifies how many ball there are missing from the basket. When there are more balls
    // than children, we need to set the used balls to accordingly.
    //
    initialSemaphoreValue = ( areThereRemainningBalls ? MAX_BALLS_THE_BASKET_SUPPORT - remainingBalls : MAX_BALLS_THE_BASKET_SUPPORT );
    areThereEmptyPlaces   = initialSemaphoreValue > 0;
    initialSemaphoreValue = ( areThereEmptyPlaces ? initialSemaphoreValue : 0 );
    
    if( sem_init( &g_filledBasketBallPlaces, 0, initialSemaphoreValue ) != 0 )
    {
        // Print like function for logging used when the DEBUG_LEVEL is set to greater than 0.
        DEBUGGER( stderr, "ERROR! Could not to initialize the semaphore! %s", strerror( errno ) );
        
        // Exits the program using a platform portable failure exit status.
        exit( EXIT_FAILURE );
    }
}

/**
 * To create this program child's play simulator thread, accordingly with
 * 'MAX_BALLS_TO_INITIALLY_GIVE_TO_THE_CHILDREN' and 'MAX_CHILD_THREADS_TO_PLAY'.
 * 
 * @param childSimulatorThreads       an thread array pointer to the threads to initialize.
 */
void toCreateTheThreadsToExecute( pthread_t *childSimulatorThreads )
{
    // Print like function for logging used when the DEBUG_LEVEL is set to greater than 0.
    DEBUGGER( stdout, "We are about to create the threads to execute. sizeof childSimulatorThreads %lu",
            ( (long unsigned int) sizeof childSimulatorThreads ) );
    
    // create 7 threads for the children, passing to each one a different number (child 0 to 6)
    for( int currentChild = 0; currentChild < MAX_CHILD_THREADS_TO_PLAY; ++currentChild )
    {
        // These are the children ids to be used as identifiers to them while they are running.
        g_childNumbers[ currentChild ] = currentChild;
        
        // Give initially some balls to some children.
        if( currentChild < MAX_BALLS_TO_INITIALLY_GIVE_TO_THE_CHILDREN )
        {
            g_howManyBallsEachChildHas[ currentChild ] = 1;
        }
        else
        {
            g_howManyBallsEachChildHas[ currentChild ] = 0;
        }
        
        // Create a second thread which executes 'childSimulatorFunction'. On success, returns 0;
        // on error, it returns an error number, and the contents of 'childSimulatorThreads[ currentChild ]'
        // are undefined.
        //
        // 'childSimulatorThreads[ currentChild ]'
        // The pointer to the ID of the new thread created. This identifier is used to refer to the
        // thread in subsequent calls to other pthreads functions.
        //
        // 'NULL'
        // The thread is created with default attributes. Attributes are specified only at thread
        // creation time; they cannot be altered while the thread is being used. Where the attribute
        // initialisation -- pthread_attr_init() create a default 'pthread_attr_t' attr. Example:
        // PTHREAD_CREATE_JOINABLE, Exit status and thread are preserved after the thread terminates.
        //
        // 'childSimulatorFunction'
        // This is a pointer to the function to call when the thread starts running.
        //
        // 'currentChild'
        // This is the pointer to argument to be passed to the function to call.
        //
        if( errno = pthread_create( &childSimulatorThreads[ currentChild ], NULL, childSimulatorFunction, &g_childNumbers[ currentChild ] ) )
        {
            // Print like function for logging used when the DEBUG_LEVEL is set to greater than 0.
            DEBUGGER( stderr, "ERROR! Could not to create the thread! %s", strerror( errno ) );
            
            // Exits the program using a platform portable failure exit status.
            exit( EXIT_FAILURE );
        }
        
        // Print like function for logging used when the DEBUG_LEVEL is set to greater than 0.
        DEBUGGER( stdout, "We just created the thread %lu (child %d) to execute.",
                childSimulatorThreads[ currentChild ], g_childNumbers[ currentChild ] );
    }
}

/**
 * Waits all this program's threads to finish before exit.
 * 
 * @param childSimulatorThreads       an thread array pointer to the threads to initialize.
 */
void waitTheThreadToExecute( pthread_t *childSimulatorThreads )
{
    // Print like function for logging used when the DEBUG_LEVEL is set to greater than 0.
    DEBUGGER( stdout, "We are about to wait for the threads to finish." );
    
    // wait for all children to finish
    for( int currentChild = 0; currentChild < MAX_CHILD_THREADS_TO_PLAY; ++currentChild )
    {
        // Print like function for logging used when the DEBUG_LEVEL is set to greater than 0.
        DEBUGGER( stdout, "We are about to wait for the thread %lu (child %d) to finish.",
                childSimulatorThreads[ currentChild ], g_childNumbers[ currentChild ] );
        
        // The this function waits for the thread specified to terminate. If that thread has
        // already terminated, then pthread_join() returns immediately. On success, pthread_join()
        // returns 0; on error, it returns an error number.
        //
        // 'childSimulatorThreads[ currentChild ]'
        // This is the thread id to wait.
        //
        // 'NULL'
        // If is not NULL, then pthread_join() copies the exit status of the target thread
        // (i.e., the value that the target thread supplied to pthread_exit(3)) into the location
        // pointed to by. If the target thread was canceled, then PTHREAD_CANCELED is placed in.
        //
        if( ( errno = pthread_join( childSimulatorThreads[ currentChild ], NULL ) ) != 0 )
        {
            // Print like function for logging used when the DEBUG_LEVEL is set to greater than 0.
            DEBUGGER( stderr, "ERROR! Could not wait the thread %lu to exit! %s",
                    childSimulatorThreads[ currentChild ], strerror( errno ) );
            
            // Exits the program using a platform portable failure exit status.
            exit( EXIT_FAILURE );
        }
    }
}

/**
 * Clean the used memory freeing its program's semaphores.
 */
void closesTheChildsGargen()
{
#if defined DEBUG
    
    // Destroy mutex. Function shall return zero; otherwise, an error number shall be returned to
    // indicate the error.
    //
    // '&g_fprintf_mutex'
    // It Is the mutex address to destroy.
    //
    if( ( errno =  pthread_mutex_destroy( &g_fprintf_mutex ) ) != 0 )
    {
        // Print like function for logging used when the DEBUG_LEVEL is set to greater than 0.
        DEBUGGER( stderr, "ERROR! Could not destroy the mutex! %s", strerror( errno ) );
        
        // Exits the program using a platform portable failure exit status.
        exit( EXIT_FAILURE );
    }
    
    DEBUGGER( stdout, "The kindengarten is closed" );
#else
    
    cout << "The kindengarten is closed" << endl;
#endif
    
    // Print like function for logging used when the DEBUG_LEVEL is set to greater than 0.
    DEBUGGER( stdout, "We are about to destroy the semaphores." );
    
    // Destroy semaphore 'g_emptyBasketBallPlaces' used to synchronize the threads.
    //
    if( sem_destroy( &g_emptyBasketBallPlaces ) != 0 )
    {
        // Print like function for logging used when the DEBUG_LEVEL is set to greater than 0.
        DEBUGGER( stderr, "ERROR! Could not destroy the semaphore g_emptyBasketBallPlaces! %s",
                strerror( errno ) );
        
        // Exits the program using a platform portable failure exit status.
        exit( EXIT_FAILURE );
    }
    
    // Destroy semaphore 'g_filledBasketBallPlaces' used to synchronize the threads.
    //
    if( sem_destroy( &g_filledBasketBallPlaces ) != 0 )
    {
        // Print like function for logging used when the DEBUG_LEVEL is set to greater than 0.
        DEBUGGER( stderr, "ERROR! Could not destroy the semaphore g_filledBasketBallPlaces! %s",
                strerror( errno ) );
        
        // Exits the program using a platform portable failure exit status.
        exit( EXIT_FAILURE );
    }
}

/**
 * This simulates a child playing/trying to play MAX_TIMES_THE_CHILD_IS_ALLOWED_TO_PLAY_WITH_THE_BALL
 * times, with only one of MAX_BALLS_THE_BASKET_SUPPORT ball(s) available to play with.
 *
 * @param void_ptr     an unsigned short to indicates the current child which will be playing/trying to play.
 *
 * @return             a void pointer to zero value.
 */
void *childSimulatorFunction( void *void_ptr )
{
    unsigned short *childNum = (unsigned short *) void_ptr;
    
    // Print like function for logging used when the DEBUG_LEVEL is set to greater than 0.
    DEBUGGER( stdout, "We are about to put the child %d (thread %lu) to play with the ball.",
            *childNum, pthread_self() );
    
    for( unsigned short currentPlayTime = 1;
         currentPlayTime <= MAX_TIMES_THE_CHILD_IS_ALLOWED_TO_PLAY_WITH_THE_BALL; ++currentPlayTime )
    {
        takeABallFromTheBasketBall( *childNum, currentPlayTime );
        dropABallInTheBasketBall( *childNum );
    }


#if defined DEBUG
    
    DEBUGGER( stdout, "Child %d will no longer play", *childNum );
#else
    
    cout << "Child " << *childNum << " will no longer play" << endl;
#endif
    
    // exit the thread
    // Terminates the calling thread and returns a value via parameter that (if the thread is joinable)
    // is available to another thread in the same process that calls pthread_join(3).
    //
    // 'NULL'
    // Pass a pointer to the zero value.
    //
    pthread_exit( NULL );
}

/**
 * Takes a ball from the basket ball, if the child 'childNum' has not already an ball to play.
 * Later let the kid to play with his ball for 1 second.
 *
 * @param childNum           the current running child number.
 * @param currentPlayTime    the current time where this child is playing.
 *
 * @note This detach this thread if the child has more than MAX_BALLS_PER_CHILD, or the
 *       'g_emptyBasketBallPlaces' semaphore is not properly initialized.
 */
void takeABallFromTheBasketBall( unsigned short childNum, unsigned short currentPlayTime )
{
    int errno;
    int howManyBallsThisChildHas;

#if defined DEBUG
    
    DEBUGGER( stdout, "Child %d wants to play with the ball for the %dth time", childNum, currentPlayTime );
#else
    
    cout << "Child " << childNum << " wants to play with the ball for the " << currentPlayTime << "th time" << endl;
#endif
    
    // if the child has no ball, need to take one from the basket if there is one, or will wait until there is a ball in the basket
    howManyBallsThisChildHas = g_howManyBallsEachChildHas[ childNum ];
    
    if( howManyBallsThisChildHas < MAX_BALLS_PER_CHILD )
    {
    #if defined DEBUG
        
        DEBUGGER( stdout, "Child %d wants to take a ball from the basket", childNum );
    #else
        
        cout << " Child " << childNum << " wants to take a ball from the basket" << endl;
    #endif
        
        // Decrements (locks) the semaphore pointed to by g_emptyBasketBallPlaces. If the
        // semaphore's value is greater than zero, then the decrement proceeds, and the
        // function returns, immediately.  If the semaphore currently has the value zero,
        // then the call blocks until either it becomes possible to perform the decrement
        // (i.e., the semaphore value rises above zero), or a signal handler interrupts the
        // call.
        //
        if( sem_wait( &g_emptyBasketBallPlaces ) != 0 )
        {
            // Print like function for logging used when the DEBUG_LEVEL is set to greater than 0.
            DEBUGGER( stderr, "ERROR! Could not to wait the semaphore g_emptyBasketBallPlaces! %s",
                    strerror( errno ) );
            
            // Exits the program using a platform portable failure exit status.
            exit( EXIT_FAILURE );
        }
        
        // Each child only access its own array position, hence there are no race conditions.
        g_howManyBallsEachChildHas[ childNum ]++;
        
        // Increments (unlocks) the semaphore pointed to by 'g_filledBasketBallPlaces'.
        // If the semaphore's value consequently becomes greater than zero, then another
        // process or thread blocked in a sem_wait(3) call will be woken up and proceed
        // to lock the semaphore.
        //
        if( sem_post( &g_filledBasketBallPlaces ) != 0 )
        {
            // Print like function for logging used when the DEBUG_LEVEL is set to greater than 0.
            DEBUGGER( stderr, "ERROR! Could not to free the semaphore g_filledBasketBallPlaces! %s",
                    strerror( errno ) );
            
            // Exits the program using a platform portable failure exit status.
            exit( EXIT_FAILURE );
        }
    }
    else if( howManyBallsThisChildHas > MAX_BALLS_PER_CHILD )
    {
        // Print like function for logging used when the DEBUG_LEVEL is set to greater than 0.
        DEBUGGER( stderr, "ERROR! This child has %d balls! It is more balls than %d balls"
                " allowed!", howManyBallsThisChildHas, MAX_BALLS_PER_CHILD );
        
        // Exits the program using a platform portable failure exit status.
        exit( EXIT_FAILURE );
    }

#if defined DEBUG
    
    DEBUGGER( stdout, "Child %d is playing with the ball", childNum );
#else
    
    // once the child has a ball, he/she starts to play
    cout << "  Child " << childNum << " is playing with the ball" << endl;
#endif
    
    // play with the ball for 1 second
    sleep( 1 );
}

/**
 * Drops a ball to the basket ball, if the child 'childNum' has an ball, and if the basket is
 * not already full (more than MAX_BALLS_THE_BASKET_SUPPORT). If the basket is full
 * wait until it has a free space, and then and only then, let the child to place its ball in the
 * basket.
 *
 * @param childNum           the current running child number.
 *
 * @note This exits the program if the child has lass ball than 0, or the 'g_emptyBasketBallPlaces'
 *       semaphore is not properly initialized.
 */
void dropABallInTheBasketBall( unsigned short childNum )
{
    int errno;
    int howManyBallsThisChildHas;
    
#if defined DEBUG
    
    DEBUGGER( stdout, "Child %d wants to leave the ball in the basket", childNum );
#else
    
    cout << "  Child " << childNum << " wants to leave the ball in the basket" << endl;
#endif
    
    howManyBallsThisChildHas = g_howManyBallsEachChildHas[ childNum ];
    
    if( howManyBallsThisChildHas > 0 )
    {
        // when the child is tired of playing, he/she has to drop the ball into the basket, if
        // there is room for it (basket holds only 3 balls), or will wait until another child to
        // take a ball.
        //
        // Decrements (locks) the semaphore pointed to by g_filledBasketBallPlaces. If the
        // semaphore's value is greater than zero, then the decrement proceeds, and the
        // function returns, immediately.  If the semaphore currently has the value zero,
        // then the call blocks until either it becomes possible to perform the decrement
        // (i.e., the semaphore value rises above zero), or a signal handler interrupts the
        // call.
        //
        if( sem_wait( &g_filledBasketBallPlaces ) != 0 )
        {
            // Print like function for logging used when the DEBUG_LEVEL is set to greater than 0.
            DEBUGGER( stderr, "ERROR! Could not to wait the semaphore g_filledBasketBallPlaces! %s",
                    strerror( errno ) );
            
            // Exits the program using a platform portable failure exit status.
            exit( EXIT_FAILURE );
        }
        
        g_howManyBallsEachChildHas[ childNum ]--;
        
        // Increments (unlocks) the semaphore pointed to by 'g_emptyBasketBallPlaces'.
        // If the semaphore's value consequently becomes greater than zero, then another
        // process or thread blocked in a sem_wait(3) call will be woken up and proceed
        // to lock the semaphore.
        //
        if( sem_post( &g_emptyBasketBallPlaces ) != 0 )
        {
            // Print like function for logging used when the DEBUG_LEVEL is set to greater than 0.
            DEBUGGER( stderr, "ERROR! Could not to free the semaphore g_emptyBasketBallPlaces! %s",
                    strerror( errno ) );
            
            // Exits the program using a platform portable failure exit status.
            exit( EXIT_FAILURE );
        }

    #if defined DEBUG
        
        DEBUGGER( stdout, "Child %d has dropped the ball in the basket", childNum );
    #else
        
        cout << " Child " << childNum << " has droped the ball in the basket" << endl;
    #endif
    }
    else
    {
        // Print like function for logging used when the DEBUG_LEVEL is set to greater than 0.
        DEBUGGER( stderr, "Error! This child has %d balls! He cannot drop ball in the basket.",
                howManyBallsThisChildHas );
        
        // Exits the program using a platform portable failure exit status.
        exit( EXIT_FAILURE );
    }
}







