
/** This is to view internal program data while execution. Default value: 0
 *
 * 0   - Disables this feature.
 * 1   - Normal debug.
 */
#define DEBUG_LEVEL 1

#include <iostream>
#include <unistd.h>
#include <fcntl.h>
#include <cstring>
#include <sys/mman.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/wait.h>

#define MAX_MESSAGE_LENGTH            /* max length */
#define SHARED_MEMORY_OBJECT_NAME     "/oioioi1234"


#if DEBUG_LEVEL > 0
    #defined DEBUG

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
    int                   sharedMemoryObject;
    struct structMessage *sharedMemoryMessage;
    
    int sharedMemorySegmentSize = sizeof struct structMessage;
    
    // Print to the standard output stream
    cout << "Parent process " << getpid() /* Process ID */ << " is creating a child process" << endl;
    
    // Duplicate process
    
    // if error in duplicating
    {
        // Exits the program using a platform portable failure exit status.
        return EXIT_FAILURE;
    }
    
    // if child-process running then
    {
        // Print to the standard output stream
        cout << "Child process " << getpid(); // Process ID
        cout << " is creating a shared memory object to write a message in" << endl;
        
        sharedMemoryObject = ; // Create shared object
        
        if( sharedMemoryObject < 0 )
        {
            // Print to the standard output stream
            cout << "Error "<< sharedMemoryObject << " creating shared object" << endl;
            
            // Exits the program using a platform portable failure exit status.
            return EXIT_FAILURE;
        }
        
        // Make room for the shared object to fit a message
        //...
        sharedMemoryMessage = (struct structMessage *) // Map the shared object to memory
        
        if( sharedMemoryMessage == NULL )
        {
            // Print to the standard output stream
            cout << "Error in memory map" << endl;
            
            // Exits the program using a platform portable failure exit status.
            return EXIT_FAILURE;
        }
        
        // Producing a message on the shared segment
        sharedMemoryMessage->type   = 1;
	    sharedMemoryMessage->sender = ; // Process ID
        
	    strcpy( sharedMemoryMessage->content, /* Message to be sent */ );
        
        // Print to the standard output stream
        cout << "Child process " << getpid() /* Process ID */ << " wrote message '";
        cout << sharedMemoryMessage->content << "' in memory" << endl;
        
        return 0;
    }
      
    // If parent-process running then
    {
        // Print to the standard output stream
        cout << "Parent process " << /* process ID */ << " is waiting for child to exit" << endl;
        
        // Wait for child process to exit and get its status
        
        
	    // If status is not success
        {
            // Print to the standard output stream
	        cout << "Parent process " << /* Process ID */;
            cout << " is exiting since child could not write message in memory" << endl;
            
            // Exits the program using a platform portable failure exit status.
	        return EXIT_FAILURE;
        }
        
        // Print to the standard output stream
        cout << "Parent process " << /* Process ID */ << " will read message from process ";
        cout << /* Child process ID */ << " finished with status ";
        cout << /* Status of finished child process */ << endl;
        
        sharedMemoryObject = // Create the shared object to read from
        
        if( sharedMemoryObject < 0 )
        {
            // Print to the standard output stream
            cout << "Error in shm_open()" << endl;
            
            // Exits the program using a platform portable failure exit status.
            return EXIT_FAILURE;
        }
        
        sharedMemoryMessage = (struct structMessage *) // Map the shared object to memory
        
        if( sharedMemoryMessage == NULL )
        {
            // Print to the standard output stream
            cout << "Error in memory map" << endl;
            
            // Exits the program using a platform portable failure exit status.
            return EXIT_FAILURE;
        }
        
        // Print to the standard output stream
        cout << "Parent process " << getpid() /* process ID */ << " read the message '";
        cout << sharedMemoryMessage->content << "' from sender ";
        cout << sharedMemoryMessage->sender << " in memory " << endl;
        
        int removed = ;// Remove the shared object
        
        if( removed != 0 )
        {
            // Print to the standard output stream
            cout << "Error removing the shared object" << endl;
            
            // Exits the program using a platform portable failure exit status.
            return EXIT_FAILURE;
        }
    }
    
    // Exits the program using a platform portable successful exit status.
    return EXIT_SUCCESS;
}

