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
        // If this is the parent-process then /* PID of parent-process */
        cout << "Parent process " << parentProcessPid << ": Waiting children to exit.\n" << endl;
        
        // Waits the children to exits.
        //
        // '-1'
        //  This catches a child we are waiting to exit and catch its return value.
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
        if( waitpid( -1, &returnStatus, 0 ) < 0
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
            cout << "\nThe child process terminated abnormally! Exit code: \n" << returnStatus << endl;
            
            // Go to the another child process, if there is any
            continue;
        }
        
        cout << "Current child process return status: " << returnStatusCached << endl;
        
        // Accumulates the return status value
        sum += returnStatusCached;
        
        // Parent-process waits for all children to exit, adding each status to the sum variable
        /* PID of parent-process */
        cout << "Parent process " << parentProcessPid << ": Accumulated sum " << sum << endl;
    }
    
    // Print to the standard output stream
    cout << "Exiting the father process with sum: " << sum << endl;
    
    // Return the required value by the teacher
    return sum;
}

