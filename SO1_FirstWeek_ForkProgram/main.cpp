#include <string>
#include <iostream>
#include <unistd.h>
#include <errno.h>
#include <sys//* Lib where system call is found */>
#include <sys//* Lib where system call is found */>

using namespace std;
int main ()
{
   // include declarations you need
   int count = 0;
   /* Repeat 5 times */
      cout << "Parent process " << /* PID of parent-process */ << ": Creating child" << endl;
      /* Duplicate this process */
      /* If there was an error on duplication then */
            cout << "Error on fork()" << endl;
            return -1;  
      /* If child-process is running then */
            cout << "Child process " << /* PID of child-process */ << ": Running" << endl;
            count++;
            /* Sleep for 1 second */
            cout << "Child process " << /* PID of child-process */ << ": Exiting with status " << count << endl;

   /* if this is the parent-process then */
      cout << "Parent process " << /* PID of parent-process */ << ": Waiting children to exit" << endl;
      int errno, status, sum = 0;
      /* Parent-process waits for all children to exit, adding each status to the sum variable */
      cout << "Parent process " << /* PID of parent-process*/ << ": Exiting with sum " <<sum << endl;
   
   /*  */
   return count;
}