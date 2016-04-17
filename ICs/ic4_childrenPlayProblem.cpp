/*********************** Licensing *******************************************************
*
*  Copyright 2016 @ Evandro  Coan
*
*  This program is free software; you can redistribute it and/or modify it
*  under the terms of the GNU General Public License as published by the
*  Free Software Foundation; either version 3 of the License, or ( at
*  your option ) any later version.
*
*  This program is distributed in the hope that it will be useful, but
*  WITHOUT ANY WARRANTY; without even the implied warranty of
*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
*  General Public License for more details.
*
*  You should have received a copy of the GNU General Public License
*  along with this program.  If not, see <http://www.gnu.org/licenses/>.
*
*****************************************************************************************
*/

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

/* ... other includes ... */
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


/** Print like function for logging putting a new line at the end of string. It does uses mutex
 * due the my doubt to know whether 'fprintf' is thread safe of not over every/any platforms, since
 * I could not find anything concrete. Following explanations:
 * 
 * pthread_mutex_lock( g_fprintf_mutex );   Lock the mutex.
 * fprintf( stream, __VA_ARGS__ );        Print to the specified output stream the formatting args.
 * fprintf( stream, "\n" );               Print a new line.
 * fflush( stream );                      Flushes the output stream to avoid double output over '>'.
 *                                          Example: './main > results.txt' would get doubled/... print.
 * pthread_mutex_unlock( g_fprintf_mutex ); Unlock the shared memory mutex.
 * } while( 0 )                           To allow to use ';' semicolon over the macro statement use and
 *                                          still to be able to use it within an unbraced if statement.
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
sem_t g_howManyBallsToCompletyFillTheBasket;
sem_t g_howManyBallsToCompletyEmptyTheBasket;

int g_childNumbers            [ MAX_CHILD_THREADS_TO_PLAY ];
int g_howManyBallsEachChildHas[ MAX_CHILD_THREADS_TO_PLAY ];


// Functions prototypes
void *childSimulatorFunction(void *);
void initializeTheSemaphores();
void toCreateTheThreadsToExecute( pthread_t *);
void waitTheThreadToExecute( pthread_t *);
void closesTheChildsGargen();

void lockTheBasketBall( unsigned short, unsigned short );
void unlockTheBasketBall( unsigned short );

using namespace std;


/** 
 * Para pensar e para responder no código
 *     
 * 
 * Os semáforos podem ser variáveis locais?
 *     
 * 
 * Todas as threads "child" terminam? Justifique.
 *     
 * 
 * E se no cesto couber uma única bola, como na versão original do problema?
 *     
 * 
 * Há algum erro de programação para que as threads não terminem?
 *     
 * 
 * O que poderia ser feito para detectar que um evento nunca ocorrerá e fazer com que o programa
 * finalize com sucesso, encerrando todas suas threads?
 *     
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
 * Initializes the semaphores 'g_howManyBallsToCompletyEmptyTheBasket' and 
 * 'g_howManyBallsToCompletyFillTheBasket' to be used over the child's ball problem, accordingly
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
    // 'g_howManyBallsToCompletyEmptyTheBasket'
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
    // taken by some children, but if there are more ball than children initializes with how
    // many balls there are available.
    //
    int          remainingBalls          = MAX_BALLS_TO_INITIALLY_GIVE_TO_THE_CHILDREN - MAX_CHILD_THREADS_TO_PLAY;
    bool         areThereRemainningBalls = remainingBalls > 0;
    unsigned int initialSemaphoreValue   = ( areThereRemainningBalls ? remainingBalls : 0 );
    
    if( sem_init( &g_howManyBallsToCompletyEmptyTheBasket, 0, initialSemaphoreValue ) != 0 )
    {
        // Print like function for logging used when the DEBUG_LEVEL is set to greater than 0.
        DEBUGGER( stderr, "ERROR! Could not to initialize the semaphore! %s", strerror( errno ) );
        
        // Exits the program using a platform portable failure exit status.
        exit( EXIT_FAILURE );
    }
    
    //
    // This specifies how many ball there are missing from the basket. When there are more balls
    // than children, we need to set the used balls to MAX_CHILD_THREADS_TO_PLAY.
    // 
    initialSemaphoreValue = ( areThereRemainningBalls ? MAX_CHILD_THREADS_TO_PLAY : MAX_BALLS_TO_INITIALLY_GIVE_TO_THE_CHILDREN );
    
    if( sem_init( &g_howManyBallsToCompletyFillTheBasket, 0, initialSemaphoreValue ) != 0 )
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
 */
void toCreateTheThreadsToExecute( pthread_t *childSimulatorThreads )
{
    // Print like function for logging used when the DEBUG_LEVEL is set to greater than 0.
    DEBUGGER( stdout, "We are about to create the threads to execute. sizeof childSimulatorThreads %d",
            sizeof childSimulatorThreads );
    
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
    
    // Destroy semaphore 'g_howManyBallsToCompletyEmptyTheBasket' used to synchronize the threads.
    //
    if( sem_destroy( &g_howManyBallsToCompletyEmptyTheBasket ) != 0 )
    {
        // Print like function for logging used when the DEBUG_LEVEL is set to greater than 0.
        DEBUGGER( stderr, "ERROR! Could not destroy the semaphore g_howManyBallsToCompletyEmptyTheBasket! %s",
                strerror( errno ) );
        
        // Exits the program using a platform portable failure exit status.
        exit( EXIT_FAILURE );
    }
    
    // Destroy semaphore 'g_howManyBallsToCompletyFillTheBasket' used to synchronize the threads.
    //
    if( sem_destroy( &g_howManyBallsToCompletyFillTheBasket ) != 0 )
    {
        // Print like function for logging used when the DEBUG_LEVEL is set to greater than 0.
        DEBUGGER( stderr, "ERROR! Could not destroy the semaphore g_howManyBallsToCompletyFillTheBasket! %s",
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
void *childSimulatorFunction(void *void_ptr)
{
    unsigned short *childNum  = (unsigned short *)void_ptr;
    
    // Print like function for logging used when the DEBUG_LEVEL is set to greater than 0.
    DEBUGGER( stdout, "We are about to put the child %d (thread %lu) to play with the ball.",
            *childNum, pthread_self() );
    
    for( unsigned short currentPlayTime = 1;
         currentPlayTime <= MAX_TIMES_THE_CHILD_IS_ALLOWED_TO_PLAY_WITH_THE_BALL; ++currentPlayTime )
    {
        lockTheBasketBall( *childNum, currentPlayTime );
        unlockTheBasketBall( *childNum );
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
 *       'g_howManyBallsToCompletyEmptyTheBasket' semaphore is not properly initialized.
 */
void lockTheBasketBall( unsigned short childNum, unsigned short currentPlayTime )
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
        
        // Decrements (locks) the semaphore pointed to by g_howManyBallsToCompletyEmptyTheBasket. If the 
        // semaphore's value is greater than zero, then the decrement proceeds, and the
        // function returns, immediately.  If the semaphore currently has the value zero,
        // then the call blocks until either it becomes possible to perform the decrement
        // (i.e., the semaphore value rises above zero), or a signal handler interrupts the
        // call.
        // 
        if( sem_wait( &g_howManyBallsToCompletyEmptyTheBasket ) != 0 )
        {
            // Print like function for logging used when the DEBUG_LEVEL is set to greater than 0.
            DEBUGGER( stderr, "ERROR! Could not to wait the semaphore g_howManyBallsToCompletyEmptyTheBasket! %s",
                    strerror( errno ) );
            
            // Exits the program using a platform portable failure exit status.
            pthread_detach( pthread_self() );
        }
        
        // Each child only access its own array position, hence there are no race conditions.
        g_howManyBallsEachChildHas[ childNum ]++;
        
        // Increments (unlocks) the semaphore pointed to by 'g_howManyBallsToCompletyFillTheBasket'. 
        // If the semaphore's value consequently becomes greater than zero, then another 
        // process or thread blocked in a sem_wait(3) call will be woken up and proceed 
        // to lock the semaphore.
        // 
        if( sem_post( &g_howManyBallsToCompletyFillTheBasket ) != 0 )
        {
            // Print like function for logging used when the DEBUG_LEVEL is set to greater than 0.
            DEBUGGER( stderr, "ERROR! Could not to free the semaphore g_howManyBallsToCompletyFillTheBasket! %s",
                    strerror( errno ) );
            
            // Exits the program using a platform portable failure exit status.
            pthread_detach( pthread_self() );
        }
    }
    else if( howManyBallsThisChildHas > MAX_BALLS_PER_CHILD )
    {
        // Print like function for logging used when the DEBUG_LEVEL is set to greater than 0.
        DEBUGGER( stderr, "ERROR! This child has %d balls! It is more balls than %d balls"
                " allowed!", howManyBallsThisChildHas, MAX_BALLS_PER_CHILD );
        
        // Exits the program using a platform portable failure exit status.
        pthread_detach( pthread_self() );
    }
    
#if defined DEBUG
    
    DEBUGGER( stdout, "Child %d is playing with the ball", childNum );
#else
    
    // once the child has a ball, he/she starts to play
    cout << "  Child " << childNum << " is playing with the ball" << endl;
#endif
    
    // play with the ball for 1 second */
    sleep( 1 );
}

/**
 * Places a ball to the basket ball, if the child 'childNum' has an ball, and if the basket is
 * not already full (more than MAX_BALLS_THE_BASKET_SUPPORT). If the basket is full
 * wait until it has a free space, and then and only then, let the child to place its ball in the
 * basket.
 * 
 * @param childNum           the current running child number.
 * 
 * @note This detach this thread if the child has more than MAX_BALLS_PER_CHILD, or the
 *       'g_howManyBallsToCompletyEmptyTheBasket' semaphore is not properly initialized.
 */
void unlockTheBasketBall( unsigned short childNum )
{
    int errno;
    
#if defined DEBUG
    
    DEBUGGER( stdout, "Child %d wants to leave the ball in the basket", childNum );
#else
    
    cout << "  Child " << childNum << " wants to leave the ball in the basket" << endl;
#endif
    
    // when the child is tired of playing, he/she has to drop the ball into the basket, if
    // there is room for it (basket holds only 3 balls), or will wait until another child to
    // take a ball.
    //
    // Decrements (locks) the semaphore pointed to by g_howManyBallsToCompletyFillTheBasket. If the 
    // semaphore's value is greater than zero, then the decrement proceeds, and the
    // function returns, immediately.  If the semaphore currently has the value zero,
    // then the call blocks until either it becomes possible to perform the decrement
    // (i.e., the semaphore value rises above zero), or a signal handler interrupts the
    // call.
    // 
    if( sem_wait( &g_howManyBallsToCompletyFillTheBasket ) != 0 )
    {
        // Print like function for logging used when the DEBUG_LEVEL is set to greater than 0.
        DEBUGGER( stderr, "ERROR! Could not to wait the semaphore g_howManyBallsToCompletyFillTheBasket! %s",
                strerror( errno ) );
        
        // Exits the program using a platform portable failure exit status.
        pthread_detach( pthread_self() );
    }
    
    g_howManyBallsEachChildHas[ childNum ]--;
    
    // Increments (unlocks) the semaphore pointed to by 'g_howManyBallsToCompletyEmptyTheBasket'. 
    // If the semaphore's value consequently becomes greater than zero, then another 
    // process or thread blocked in a sem_wait(3) call will be woken up and proceed 
    // to lock the semaphore.
    // 
    if( sem_post( &g_howManyBallsToCompletyEmptyTheBasket ) != 0 )
    {
        // Print like function for logging used when the DEBUG_LEVEL is set to greater than 0.
        DEBUGGER( stderr, "ERROR! Could not to free the semaphore g_howManyBallsToCompletyEmptyTheBasket! %s",
                strerror( errno ) );
        
        // Exits the program using a platform portable failure exit status.
        pthread_detach( pthread_self() );
    }
    
#if defined DEBUG
    
    DEBUGGER( stdout, "Child %d has dropped the ball in the basket", childNum );
#else
        
    cout << " Child " << childNum << " has droped the ball in the basket" << endl;
#endif
}






















