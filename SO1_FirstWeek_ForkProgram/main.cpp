#include <stdio.h>
#include <string>
#include <iostream>
#include <unistd.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <sys/mman.h>
#include <cstdlib>

#define MAX_CHILD_PROCESS_TO_CREATE 5

static pid_t *childProcessPids = new pid_t[ MAX_CHILD_PROCESS_TO_CREATE ];

using namespace std;

int main ()
{
    int errno;
    int returnStatusCached;
    int returnStatus;
    pid_t currentProcessPid;
    
    int count              = 0;
    int sum                = 0;
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
    void *sharedMemory = mmap( NULL, sizeof childProcessPids * MAX_CHILD_PROCESS_TO_CREATE,
            PROT_READ | PROT_WRITE, MAP_SHARED | MAP_ANONYMOUS, -1, 0 );
    
    // verifies whether the shared memory was created of not
    if( sharedMemory == MAP_FAILED )
    {
        // Print to the standard output stream 
        cout << "\nERROR! The shared memory could not to be created.\n" << endl;
        
        // Exits the program with failure status
        return EXIT_FAILURE;
    }
    
    //childProcessPids = (pid_t *) sharedMemory;
    
    // Put a empty line to clear console view.
    cout << endl;
    
    // Repeat 5 times and to create 5 concurrent threads
    for( int current_child_process_number = 0; 
          current_child_process_number < MAX_CHILD_PROCESS_TO_CREATE; current_child_process_number++ )
    {
        // Print to the standard output stream /* PID of parent-process */
        cout << "Parent process " << parentProcessPid << ": Creating child..." << endl;
        
        // Duplicate this process and saves the current process pid
        currentProcessPid = fork();
        
        // If there was an error on duplication then
        if( currentProcessPid < 0 )
        {
          cout << "\nError on fork()\n" << endl;
          
          return -1;
        }
        else if( currentProcessPid == 0 ) // If child-process is running then
        {
            // Put a empty line to clear console view
            cout << endl;
            
            // Save the current child pid process to wait for it later
            currentProcessPid                                = getpid();
            childProcessPids[ current_child_process_number ] = currentProcessPid;
            
            // Print to the standard output stream /* PID of child-process */ 
            cout << "Child process " << currentProcessPid << ": Running..." << endl;
            
            // Increment the conter required by the teacher
            count++;
            
            // Sleep for 1 second
            sleep( 1 );
            
            // Print to the standard output stream /* PID of child-process */
            cout << "Child process " << currentProcessPid << ": Exiting with status " << count << endl;
            
            // Exits the fork to stop the children creating grandchildren using the required value
            // by the teacher.
            exit( count );
        }
        else // pid is greater than 0, so we are the parent process, then to create the next thread
        {
            continue;
        }
    }
    
    // Wait all created threads to exit
    for( int current_child_process_number = 0;
          current_child_process_number < MAX_CHILD_PROCESS_TO_CREATE; ++current_child_process_number )
    {
        // Caches the current array position process pid
        currentProcessPid = childProcessPids[ current_child_process_number ];
        
        // If this is the parent-process then /* PID of parent-process */
        cout << "Parent process " << parentProcessPid << ": Waiting children ";
        cout << currentProcessPid << " to exit.\n" << endl;
        
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
            cout << "The calling process does not have any unwaited-for children." << endl;
            
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
            cout << "The child process terminated abnormally! Exit code: " << returnStatus << endl;
            
            // Go to the another child process, if there is any
            continue;
        }
        
        cout << "Current child process return status: " << returnStatusCached << endl;
        
        // Accumulates the return status value
        sum += returnStatusCached;
        
        // Parent-process waits for all children to exit, adding each status to the sum variable
        /* PID of parent-process */
        cout << "Parent process " << parentProcessPid << ": Exiting with sum " << sum << endl;
    }
    
    // Print to the standard output stream
    cout << "Exiting the father process with sum: " << sum << endl;
    
    // Free the dynamic allocated memory to avoid memory leak
    delete childProcessPids;
    
    // Return the required value by the teacher
    return sum;
}

