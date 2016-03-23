#include <iostream>
#include <unistd.h>
#include <fcntl.h>
#include <cstring>
#include <sys/mman.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/wait.h>

#define SHARED_OBJ_NAME     "/oioioi1234"
#define MAX_MSG_LENGTH      /* max length */

struct message_struct {
    pid_t sender;
    int type;
    char content[MAX_MSG_LENGTH];
};

using namespace std;
int main() {
    int shared_object;
    int shared_seg_size = sizeof(struct message_struct);   
    struct message_struct *shared_message;  

    cout << "Parent process " << /* process ID */ << " is creating a child process" << endl;
    
    /* duplicate process */
    
    /* if error in duplicating */
        return -1;
        
    /* if child-process running then */
        cout << "Child process " << /* process ID */ << " is creating a shared memory object to write a message in" << endl;
        shared_object = /* create shared object */
        if (shared_object < 0) {
            cout << "Error "<< shared_object << " creating shared object" << endl;
            return -1;
        }
        /*  make room for the shared object to fit a message*/
        //...
        shared_message = (struct message_struct *) /* map the shared onject to memory */
        if (shared_message == NULL) {
            cout << "Error in memory map" << endl;
            return -1;
        }
        /* producing a message on the shared segment */
        shared_message->type = 1;
	    shared_message->sender = /* process ID */
	    strcpy(shared_message->content, /* message to be sent */);
        cout << "Child process " << /* process ID */ << " wrote message '" << shared_message->content << "' in memory" << endl;
        return 0;
      
    /* if parent-process running then */
        cout << "Parent process " << /* process ID */ << " is waiting for child to exit" << endl;
        /* wait for child process to exit and get its status */
	    /* if status is not success */
	        cout << "Parent process " << /* process ID */ << " is exiting since child could not write message in memory" << endl;
	        return -1;
	
        cout << "Parent process " << /* process ID */ << " will read message from process " << /* child process ID */ << " finished with status "<< /* status of finished child process */ << endl;
        shared_object = /* create the shared object to read from */
        if (shared_object < 0) {
            cout << "Error in shm_open()" << endl;
            return -1;
        }
            
        shared_message = (struct message_struct *)/* map the shared object to memory */
        if (shared_message == NULL) {
            cout << "Error in memory map" << endl;
            return -1;
        }

        cout << "Parent process " << /* process ID */ << " read the message '" << shared_message->content << "' from sender " << shared_message->sender << " in memory " << endl;
   
        int removed = /* remove the shared object */
        if (removed != 0) {
            cout << "Error removing the shared object" << endl;
            return -1;
        }
    }
}

