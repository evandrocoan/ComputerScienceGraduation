#include <stdio.h>
#include <string>
#include <iostream>
#include <unistd.h>
#include <errno.h>
#include <stdarg.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <sys/mman.h>
#include <cstdlib>
#include <pthread.h>

#define MAX_CHILD_PROCESS_TO_CREATE                       5
#define REQUIRED_THREAD_SLEEP_TIME_SECONDS_BY_THE_TEACHER 1

using namespace std;

static pid_t           *childProcessPids;
static pthread_mutex_t *fprintf_mutex;

/** Print like function for logging putting a new line at the end of string. It does uses mutex
 * due the my doubt to know whether 'fprintf' is thread safe of not over every/any platforms, since
 * I could not find anything concrete. Following explanations:
 * 
 * pthread_mutex_lock( fprintf_mutex );   Lock the mutex.
 * fprintf( stream, __VA_ARGS__ );        Print to the specified output stream the formatting args.
 * fprintf( stream, "\n" );               Print a new line.
 * fflush( stream );                      Flushes the output stream to avoid double output over '>'.
 *                                          Example: './main > results.txt' would get doubled/triple/...
 *                                          print.
 * pthread_mutex_unlock( fprintf_mutex ); Unlock the shared memory mutex.
 * } while( 0 )                           To allow to use ';' semicolon over the macro statement use and
 *                                          still to be able to use it within an unbraced if statement.
 */
#define FPRINTFLN( stream, ... ) \
{ \
    pthread_mutex_lock( fprintf_mutex ); \
    fprintf( stream, __VA_ARGS__ ); \
    fprintf( stream, "\n" ); \
    fflush( stream ); \
    pthread_mutex_unlock( fprintf_mutex ); \
} while( 0 )


/**
 * Para pensar: Há uma variável global count acessível a todos os processos e que é inicializada 
 * com 0. Cinco processos são criados e cada um incrementa o valor atual dessa variável, 
 * retornando o valor incrementado. Se somarmos todos os valores retornados, quanto você acha 
 * que será apresentado? Existe possibilidade de condição de corrida?
 * 
 * Não por que o fork duplica os endereços de memória de processo pai e filho, portando ambos
 * executam em locais diferentes da memória com suas próprias cópias de variáveis. Exceto quando
 * se é utilizado uma memória compartilhada em um local diferente da memória definido pelo sistema
 * operational como nosso caso e feito nesse projeto:
 * 1) Para o pai e ambos os filhos terem acesso ao mutex utilizado.
 * 2) Para o pai poder ter acesso ao pid dos filhos.
 */
int main ()
{
    int   errno;
    int   returnStatusCached;
    int   returnStatus;
    pid_t currentProcessPid;
    
    int   count            = 0;
    int   sum              = 0;
    int   tries            = 0;
    pid_t parentProcessPid = getpid();
    
    // 'NULL'
    //  indicates to the kernel chooses the address at which to create the mapping.
    //
    // 'sizeof childProcessPids * MAX_CHILD_PROCESS_TO_CREATE'
    //  specifies the length of the mapping.
    //
    // 'PROT_READ | PROT_WRITE'
    //  describes the desired writing and reading memory protection of the mapping to avoid race
    //  condition problems.
    //
    // 'MAP_SHARED | MAP_ANONYMOUS'
    //  MAP_SHARED, determine that the updates to the mapping are visible to other processes
    //  mapping the same region. MAP_ANONYMOUS, the mapping is not backed by any file, then its
    //  contents are initialized to zero. Also, the use of MAP_ANONYMOUS in conjunction with 
    //  MAP_SHARED is supported on Linux only since kernel v2.4.
    //
    // '-1'
    //  This and next arguments are ignored when using 'MAP_ANONYMOUS', however, some
    //  implementations require this to be '-1' if MAP_ANONYMOUS is specified, hence portable
    //  applications should ensure this. This is the file descriptor used to initialize the 
    //  contents of the file mapping using the length bytes of the mapping and offset specified
    //  at the next parameter bellow.
    //
    // '0'
    //  This is a must be a multiple of the page size as returned by sysconf(_SC_PAGE_SIZE).
    //  However here, it is unused due the 'MAP_ANONYMOUS' being specified.
    //
    // It returns a void pointer to the shared memory and here it is saved on the variable sharedMemory.
    //
    int protection          = PROT_READ | PROT_WRITE;
    int memory_pid_size     = sizeof *childProcessPids * MAX_CHILD_PROCESS_TO_CREATE;
    int visibility          = MAP_SHARED | MAP_ANONYMOUS;
    void *sharedMemoryPids  = mmap( NULL, memory_pid_size, protection, visibility, -1, 0 );
    void *sharedMemoryMutex = mmap( NULL, sizeof *fprintf_mutex, protection, visibility, -1, 0 );
    
    // verifies whether the shared memory was created of not
    if( sharedMemoryPids == MAP_FAILED
        || sharedMemoryMutex == MAP_FAILED )
    {
        // Print to the standard output stream 
        FPRINTFLN( stderr, "\nERROR! The shared memory could not to be created.\n" );
        
        // Exits the program with failure status
        return EXIT_FAILURE;
    }
    
    // Gets the child's pid shared memory array from the void pointer sharedMemoryPids.
    childProcessPids = ( pid_t * ) sharedMemoryPids;
    
    // Converts the void pointer sharedMemoryMutex to a pthread_mutex_t pointer.
    fprintf_mutex    = ( pthread_mutex_t * ) sharedMemoryMutex;
    
    // Initializes the mutex for use. Specifies NULL to use the default mutex attributes.
    if( errno = pthread_mutex_init( fprintf_mutex, NULL ) )
    {
        // Print to the standard output stream
        FPRINTFLN( stderr, "\nERROR! Could initialize the mutex! Erro code: %d\n", errno );
    }
    
    // Put a empty line to clear console view.
    FPRINTFLN( stdout, "\n" );
    
    // Repeat 5 times and to create 5 concurrent threads
    for( int current_child_process_number = 0; 
          current_child_process_number < MAX_CHILD_PROCESS_TO_CREATE; current_child_process_number++ )
    {
        // Print to the standard output stream /* PID of parent-process */
        FPRINTFLN( stdout, "Parent process %i: Creating child...", parentProcessPid );
        
        // Duplicate this process and saves the current process pid
        currentProcessPid = fork();
        
        // If there was an error on duplication then
        if( currentProcessPid < 0 )
        {
            // Print to the standard output stream
            FPRINTFLN( stderr, "\nError on fork()\n" );
            
            // Exits the program with portable error state
            return EXIT_FAILURE;
        }
        else if( currentProcessPid == 0 ) // If child-process is running then
        {
            // Put a empty line to clear console view
            FPRINTFLN( stdout, "\n" );
            
            // Save the current child pid process to wait for it later
            currentProcessPid                                = getpid();
            childProcessPids[ current_child_process_number ] = currentProcessPid;
            
            // Print to the standard output stream /* PID of child-process */ 
            FPRINTFLN( stdout, "Child process %i : Running...", currentProcessPid );
            
            // Increment the conter required by the teacher
            count++;
            
            // Sleep for the REQUIRED_THREAD_SLEEP_TIME_SECONDS_BY_THE_TEACHER second(s)
            sleep( REQUIRED_THREAD_SLEEP_TIME_SECONDS_BY_THE_TEACHER );
            
            // Print to the standard output stream /* PID of child-process */
            FPRINTFLN( stdout, "Child process %i: Exiting with status %i", currentProcessPid, count );
            
            // Exits the fork to stop the children creating grandchildren using the required value
            // by the teacher.
            exit( count );
        }
        else // pid is greater than 0, so we are the parent process, then to create the next thread
        {
            continue;
        }
    }
    
    // Wait all created threads to exit. Here is not treated the cases where there are threads
    // slower than others. It is just checked sequentially for the threads to run and return its
    // answer, i.e., if a thread 2 take 4 seconds but the thread 1 takes 10 seconds, the thread 2
    // will wait 10 seconds before being validated because it is after the thread 1 on this queue.
    //
    // 'waitpid( -1, &returnStatus, 0 ) < 0 && errno == ECHILD )'
    //  Was not used here to determine when there are not any more thread waiting to exit, as we
    //  keep a list of all child threads over the childProcessPids' array. However, it is used to
    //  to determinate if the current child process is does not waiting, then just proceed to the
    //  next thread, if there is any.
    //
    for( int current_child_process_number = 0;
          current_child_process_number < MAX_CHILD_PROCESS_TO_CREATE; ++current_child_process_number )
    {
        // Caches the current array position process pid
        currentProcessPid = childProcessPids[ current_child_process_number ];
        
        // If the pid is null, wait until it is initialized by its thread.
        if( currentProcessPid == 0 )
        {
            // If the maximum tries exceed 50 * REQUIRED_THREAD_SLEEP_TIME_SECONDS_BY_THE_TEACHER,
            // there is a bug.
            if( tries++ > 50 * REQUIRED_THREAD_SLEEP_TIME_SECONDS_BY_THE_TEACHER )
            {
                // Print to the standard output stream
                FPRINTFLN( stderr, "\nError! The maximum tries exceed %i! \
                        The current child process number is: %i\n",
                        50 * REQUIRED_THREAD_SLEEP_TIME_SECONDS_BY_THE_TEACHER,
                        current_child_process_number );
                
                // Exits the program with portable error state
                return EXIT_FAILURE;
            }
            
            // Gives 0.1 * REQUIRED_THREAD_SLEEP_TIME_SECONDS_BY_THE_TEACHER seconds to wait for
            // the current process child.
            usleep( 100000 * REQUIRED_THREAD_SLEEP_TIME_SECONDS_BY_THE_TEACHER );
            
            // Return to the current child to try to process its pid again.
            --current_child_process_number;
            continue;
        }
        
        // If this is the parent-process then /* PID of parent-process */
        FPRINTFLN( stdout, "Parent process %i: Waiting children %i to exit.", parentProcessPid,
                currentProcessPid );
        
        // Waits the children to exits.
        //
        // 'currentProcessPid'
        //  This is child we are waiting to exit and catch its return value.
        //
        // 'returnStatus'
        //  This is the address to the return value by the child using exit( status )
        //
        // '0'
        //  This is to wait to any children to exit before to go on.
        //
        // 'errno'
        //  This implementation is provided <errno.h> and indicates the failure error code, it only
        //  must be checked over an failure exit status checked by
        //  'waitpid( currentProcessPid, &returnStatus, 0 ) < 0'.
        //
        if( waitpid( currentProcessPid, &returnStatus, 0 ) < 0
            && errno == ECHILD )
        {
            // Print to the standard output stream
            FPRINTFLN( stderr, "\nThe calling process %i does not have any unwaited-for children.\n",
                    currentProcessPid );
            
            // Go to the another child process, if there is any
            continue;
        }
        
        // Verifies whether the child terminated normally, that is, by calling 'exit'.
        if( WIFEXITED( returnStatus ) )
        {
            // caches the return status value
            returnStatusCached = WEXITSTATUS( returnStatus );
        }
        else
        {
            // Print to the standard output stream
            FPRINTFLN( stderr, "\nERROR! The child process %i terminated abnormally! Exit code: %i\n",
                currentProcessPid, returnStatus );
            
            // Go to the another child process, if there is any
            continue;
        }
        
         // Print to the standard output stream
        FPRINTFLN( stdout, "Current child process %i return status: %i", currentProcessPid,
                returnStatusCached );
        
        // Accumulates the return status value
        sum += returnStatusCached;
        
        // Parent-process waits for all children to exit, adding each status to the sum variable
        /* PID of parent-process */
        FPRINTFLN( stdout, "Parent process %i: Accumulated sum %i", parentProcessPid, sum );
    }
    
    // Print to the standard output stream
    FPRINTFLN( stdout, "Exiting the father process with sum: %i", sum );
    
    // free memory, childProcessPids is the address to free and memory_pid_size is the address size.
    if( munmap( childProcessPids, memory_pid_size ) < 0
        || munmap( sharedMemoryMutex, sizeof *fprintf_mutex ) < 0 )
    {
        // Print to the standard output stream
        FPRINTFLN( stderr, "\nERROR! Could not free the shared memory! Error code: %d\n", errno );
    }
    
    // Destroy the mutex
    if( errno = pthread_mutex_destroy( fprintf_mutex ) )
    {
        // Print to the standard output stream
        FPRINTFLN( stderr, "\nERROR! Could destroy the mutex!\n Error code: %d", errno );
    }
    
    // Return the required value by the teacher
    return sum;
}

