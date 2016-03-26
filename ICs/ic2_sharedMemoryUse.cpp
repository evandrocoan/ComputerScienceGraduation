
#include <iostream>
#include <unistd.h>
#include <fcntl.h>
#include <cstring>
#include <cstdlib>
#include <errno.h>
#include <stdio.h>
#include <sys/mman.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/wait.h>


/** This is to view internal program data while execution. Default value: 0
 *
 * 0   - Disables this feature.
 * 1   - Normal debug.
 */
#define DEBUG_LEVEL 1


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


/**
 * For portable use, a shared memory object should be identified by a name of the form /somename;
 * that is, a null-terminated string of up to NAME_MAX (i.e., 255) characters consisting of an 
 * initial slash, followed by one or more characters, none of which are slashes.
 */
const char *sharedMemoryObjectFileDescriptorName = "/oioioi1234";
#define MAX_MESSAGE_LENGTH 100


struct structMessage
{
    pid_t sender;
    int   type;
    
    char content[ MAX_MESSAGE_LENGTH ];
};

using namespace std;


/**
 * Para pensar:
 * 
 * As funções usadas criam um segmento de memória de tamanho especificado (truncado) e depois há
 * proteção de páginas. Qual é o modelo de gerenciamento de memória usado?
 * 
 * 
 * Esse mecanismo pode ser usado para implementar sincronização de processos, como uma alternativa
 * aos semáforos?
 * 
 * 
 * Os processos que se comunicam via memória compartilhada precisam estar no mesmo arquivo executável
 * como nesse exemplo ou eles podem estar em arquivos separados?
 * 
 * 
 * Eles precisam estar executando no mesmo processador ou podem estar executando em processadores
 * diferentes?
 * 
 * 
 * Eles precisam estar executando no mesmo computador ou podem estar executando em computadores
 * diferentes?
 * 
 * 
 * Por fim, se o objetivo é comunicar dois processos no mesmo computador, tente fazer o mesmo
 * usando pipes.
 * 
 * 
 */
int main() 
{
    pid_t                currentProcessPid;
    int                  errno;
    int                  sharedMemoryObjectFileDescriptor;
    int                  returnStatus;
    int                  returnStatusCached;
    struct structMessage *sharedMemoryMessage;
    
    int sharedMemorySegmentSize = sizeof( struct structMessage );
    pid_t parentProcessPid      = getpid();
    
    // Print to the standard output stream /* Process ID */
    cout << "Parent process " << parentProcessPid << " is creating a child process" << endl;
    
    // Duplicate process
    currentProcessPid = fork();
    
    // if error in duplicating
    if( currentProcessPid < 0 )
    {
        // Print to the standard output stream
        DEBUGGER( stderr, "Error on fork()" );
        
        // Exits the program using a platform portable failure exit status.
        return EXIT_FAILURE;
    }
    else if( currentProcessPid == 0 ) // if child-process running then
    {
        currentProcessPid = getpid();
        
        // Print to the standard output stream /* Process ID */
        cout << "Child process " << currentProcessPid;
        cout << " is creating a shared memory object to write a message in" << endl;
        
        /**
         * Creates a new, or opens an existing, POSIX shared memory object. A POSIX
         * shared memory object is in effect a handle which can be used by unrelated processes to
         * mmap(2) the same region of shared memory.
         * 
         * 'const char *sharedMemoryObjectFileDescriptorName'
         * specifies the shared memory object name to be created or opened.
         * 
         * 'int O_RDWR | O_CREAT'
         * 'O_RDWR' is a bit mask created to open the object for read-write access.
         * 'O_CREAT' create the shared memory object if it does not exist.
         * 
         * 'mode_t S_IRWXU'
         * 00700 user (file owner) has read, write, and execute permission.
         * 
         * @return on success, returns a nonnegative file descriptor. On failure, returns -1.
         */
        int    creation_flags            = O_RDWR | O_CREAT;
        mode_t creation_mode             = S_IRWXU;
        sharedMemoryObjectFileDescriptor = shm_open( sharedMemoryObjectFileDescriptorName, creation_flags, creation_mode );
        
        // If error creating shared object
        if( sharedMemoryObjectFileDescriptor < 0 )
        {
            // Print to the standard output stream
            cout << "Error "<< sharedMemoryObjectFileDescriptor << " creating shared object" << endl;
            
            // Exits the program using a platform portable failure exit status.
            return EXIT_FAILURE;
        }
        
        DEBUGGER( stdout, "Child process created a new, or opened an existing, POSIX shared memory with shm_open()." );
        
        /**
         * Make room for the shared object to fit a message. On ftruncate success, zero is returned.
         * On error, -1 is returned, and errno is set appropriately.
         * 
         * 'sharedMemoryObjectFileDescriptor'
         * reference to be truncated to a size of precisely length bytes 'sizeof struct structMessage'.
         */
        if( ftruncate( sharedMemoryObjectFileDescriptor, sharedMemorySegmentSize ) < 0 )
        {
            // Print to the standard output stream
            DEBUGGER( stderr, "ERROR! Could not cause the regular file named by path or referenced \
                    to be truncated to a size of precisely length of %i bytes.", sharedMemorySegmentSize );
            
            // Exits the program using a platform portable failure exit status.
            return EXIT_FAILURE;
        }
        
        DEBUGGER( stdout, "Child process make a room for the shared object to fit a message." );
        
        /** 
         * Creates a shared memory.
         * 
         * 'NULL'
         *  indicates to the kernel chooses the address at which to create the mapping.
         *
         * 'sharedMemorySegmentSize'
         *  specifies the length of the mapping.
         *
         * 'PROT_READ | PROT_WRITE'
         *  describes the desired writing and reading memory protection of the mapping to avoid race
         *  condition problems.
         *
         * 'MAP_SHARED | MAP_ANONYMOUS'
         *  MAP_SHARED, determine that the updates to the mapping are visible to other processes
         *  mapping the same region. MAP_ANONYMOUS, the mapping is not backed by any file, then its
         *  contents are initialized to zero. Also, the use of MAP_ANONYMOUS in conjunction with 
         *  MAP_SHARED is supported on Linux only since kernel v2.4.
         *
         * '-1'
         *  This and next arguments are ignored when using 'MAP_ANONYMOUS', however, some
         *  implementations require this to be '-1' if MAP_ANONYMOUS is specified, hence portable
         *  applications should ensure this. This is the file descriptor used to initialize the 
         *  contents of the file mapping using the length bytes of the mapping and offset specified
         *  at the next parameter bellow.
         *
         * '0'
         *  This is a must be a multiple of the page size as returned by sysconf(_SC_PAGE_SIZE).
         *  However here, it is unused due the 'MAP_ANONYMOUS' being specified.
         *
         * It returns a void pointer to the shared memory and here it is saved on the variable sharedMemory.
         */
        int protection            = PROT_READ | PROT_WRITE;
        int visibility            = MAP_SHARED;
        void *sharedMemoryMapping = mmap( NULL, sharedMemorySegmentSize, protection, visibility,
                sharedMemoryObjectFileDescriptor, 0 );
        
        // verifies whether the shared memory was created of not
        if( sharedMemoryMapping == MAP_FAILED )
        {
            // Print to the standard output stream 
            DEBUGGER( stderr, "ERROR! The shared memory could not to be created." );
            
            // Exits the program with failure status
            return EXIT_FAILURE;
        }
        
        DEBUGGER( stdout, "Child process created a shared memory with mmap()." );
        
        // Map the shared object to memory. Now we can refer to mapped region using fields of
        // sharedMemoryMessage. For example, sharedMemoryMessage->len
        sharedMemoryMessage = (struct structMessage *) sharedMemoryMapping;
        
        // If error in memory map
        if( sharedMemoryMessage == NULL )
        {
            // Print to the standard output stream
            cout << "Error in memory map" << endl;
            
            // Exits the program using a platform portable failure exit status.
            return EXIT_FAILURE;
        }
        
        DEBUGGER( stdout, "Child process mapped the shared object to memory. Usage example, sharedMemoryMessage->len." );
        
        // Producing a message on the shared segment
        sharedMemoryMessage->type   = 1;
	    sharedMemoryMessage->sender = currentProcessPid; // Process ID
        
	    strcpy( sharedMemoryMessage->content, "Message to be sent" );
        
        // Print to the standard output stream /* Process ID */
        cout << "Child process " << currentProcessPid << " wrote message '";
        cout << sharedMemoryMessage->content << "' in memory" << endl;
        
        DEBUGGER( stdout, "Child process exits using a platform portable successful exit status." );
        
        // Exits the child process returning to the parent the shared memory pointer.
        exit( EXIT_SUCCESS );
    }
    else // Pid is greater than 0, so we are the parent process
    {
        // Print to the standard output stream /* process ID */ 
        cout << "Parent process " << parentProcessPid << " is waiting for child to exit" << endl;
        
        // Wait for child process to exit and get its status
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
        //  'waitpid( -1, &returnStatus, 0 ) < 0'.
        //
        if( waitpid( currentProcessPid, &returnStatus, 0 ) < 0
            && errno == ECHILD )
        {
            // Print to the standard output stream
            DEBUGGER( stderr, "The calling process %i does not have any unwaited-for children.",
                    parentProcessPid );
        }
	    
        if( WIFEXITED( returnStatus ) )
        {
            // caches the return status value
            returnStatusCached = WEXITSTATUS( returnStatus );
        }
        else // status is not success
        {
            // Print to the standard output stream /* Process ID */
	        cout << "Parent process " << parentProcessPid;
            cout << " is exiting since child could not write message in memory" << endl;
            
            // Print to the standard output stream
            DEBUGGER( stderr, "ERROR! The child process %i terminated abnormally! Exit code: %i",
                    currentProcessPid, returnStatus );
            
            // Exits the program using a platform portable failure exit status.
	        return EXIT_FAILURE;
        }
        
        // Print to the standard output stream /* Process ID */
        cout << "Parent process " << parentProcessPid << " will read message from process ";
        
        // Print to the standard output stream /* Child process ID */
        cout << sharedMemoryMessage->sender << " finished with status ";
        
        // Print to the standard output stream /* Status of finished child process */
        cout << returnStatus << endl;
        
        /**
         * Creates and opens a new, or opens an existing, POSIX shared memory object. A POSIX
         * shared memory object is in effect a handle which can be used by unrelated processes to
         * mmap(2) the same region of shared memory.
         * 
         * 'const char *sharedMemoryObjectFileDescriptorName'
         * specifies the shared memory object name to be created or opened.
         * 
         * 'int O_RDWR | O_CREAT'
         * 'O_RDWR' is a bit mask created to open the object for read-write access.
         * 'O_CREAT' create the shared memory object if it does not exist.
         * 
         * 'mode_t S_IRWXU'
         * 00700 user (file owner) has read, write, and execute permission.
         * 
         * @return on success, returns a nonnegative file descriptor. On failure, returns -1.
         */
        int    creation_flags            = O_RDWR | O_CREAT;
        mode_t creation_mode             = S_IRWXU;
        sharedMemoryObjectFileDescriptor = shm_open( sharedMemoryObjectFileDescriptorName, creation_flags, creation_mode );
        
        if( sharedMemoryObjectFileDescriptor < 0 )
        {
            // Print to the standard output stream
            cout << "Error in shm_open()" << endl;
            
            // Exits the program using a platform portable failure exit status.
            return EXIT_FAILURE;
        }
        
        DEBUGGER( stdout, "Parent process created a new, or opened an existing, POSIX shared memory with shm_open()." );
        
        /**
         * Make room for the shared object to fit a message. On ftruncate success, zero is returned.
         * On error, -1 is returned, and errno is set appropriately.
         * 
         * 'sharedMemoryObjectFileDescriptor'
         * reference to be truncated to a size of precisely length bytes 'sizeof struct structMessage'.
         */
        if( ftruncate( sharedMemoryObjectFileDescriptor, sharedMemorySegmentSize ) < 0 )
        {
            // Print to the standard output stream
            DEBUGGER( stderr, "ERROR! Could not cause the regular file named by path or referenced \
                    to be truncated to a size of precisely length of %i bytes.", sharedMemorySegmentSize );
            
            // Exits the program using a platform portable failure exit status.
            return EXIT_FAILURE;
        }
        
        DEBUGGER( stdout, "Parent process make a room for the shared object to fit a message." );
        
        /** 'NULL'
         *  indicates to the kernel chooses the address at which to create the mapping.
         *
         * 'sharedMemorySegmentSize'
         *  specifies the length of the mapping.
         *
         * 'PROT_READ | PROT_WRITE'
         *  describes the desired writing and reading memory protection of the mapping to avoid race
         *  condition problems.
         *
         * 'MAP_SHARED | MAP_ANONYMOUS'
         *  MAP_SHARED, determine that the updates to the mapping are visible to other processes
         *  mapping the same region. MAP_ANONYMOUS, the mapping is not backed by any file, then its
         *  contents are initialized to zero. Also, the use of MAP_ANONYMOUS in conjunction with 
         *  MAP_SHARED is supported on Linux only since kernel v2.4.
         *
         * '-1'
         *  This and next arguments are ignored when using 'MAP_ANONYMOUS', however, some
         *  implementations require this to be '-1' if MAP_ANONYMOUS is specified, hence portable
         *  applications should ensure this. This is the file descriptor used to initialize the 
         *  contents of the file mapping using the length bytes of the mapping and offset specified
         *  at the next parameter bellow.
         *
         * '0'
         *  This is a must be a multiple of the page size as returned by sysconf(_SC_PAGE_SIZE).
         *  However here, it is unused due the 'MAP_ANONYMOUS' being specified.
         *
         * It returns a void pointer to the shared memory and here it is saved on the variable sharedMemory.
         */
        int protection            = PROT_READ | PROT_WRITE;
        int visibility            = MAP_SHARED;
        void *sharedMemoryMapping = mmap( NULL, sharedMemorySegmentSize, protection, visibility,
                sharedMemoryObjectFileDescriptor, 0 );
        
        // verifies whether the shared memory was created of not
        if( sharedMemoryMapping == MAP_FAILED )
        {
            // Print to the standard output stream 
            DEBUGGER( stderr, "\nERROR! The shared memory could not to be created.\n" );
            
            // Exits the program with failure status
            return EXIT_FAILURE;
        }
        
        DEBUGGER( stdout, "Parent process created a shared memory with mmap()." );
        
        // Map the shared object to memory. Now we can refer to mapped region using fields of
        // sharedMemoryMessage. For example, sharedMemoryMessage->len
        sharedMemoryMessage = (struct structMessage *) sharedMemoryMapping;
        
        // If error in memory map
        if( sharedMemoryMessage == NULL )
        {
            // Print to the standard output stream
            cout << "Error in memory map" << endl;
            
            // Exits the program using a platform portable failure exit status.
            return EXIT_FAILURE;
        }
        
        DEBUGGER( stdout, "Parent process mapped the shared object to memory. Usage example, sharedMemoryMessage->len." );
        
        // Print to the standard output stream/* process ID */
        cout << "Parent process " << parentProcessPid << " read the message '";
        cout << sharedMemoryMessage->content << "' from sender ";
        cout << sharedMemoryMessage->sender << " in memory " << endl;
        
        /**
         * Remove the object previously created by shm_open().
         * 
         * @param name   specifies the shared memory object to be created or opened.
         * @return       returns 0 on success, or -1 on error.
         */
        int removed = shm_unlink( sharedMemoryObjectFileDescriptorName );
        
        if( removed != 0 )
        {
            // Print to the standard output stream
            cout << "Error removing the shared object" << endl;
            
            // Exits the program using a platform portable failure exit status.
            return EXIT_FAILURE;
        }
    }
    
    DEBUGGER( stdout, "Parent process exits using a platform portable successful exit status." );
    
    // Exits the program using a platform portable successful exit status.
    return EXIT_SUCCESS;
}

