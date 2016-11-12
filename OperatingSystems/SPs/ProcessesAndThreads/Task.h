/*
 * Task.h
 *
 *  Created on: Aug 15, 2014
 */

#ifndef TASK_H_
#define TASK_H_

#include <Queue.h>
#include <ucontext.h>



/**
* @see BOOOS namespace declaration at the main file 'BOOOS.h'.
*/
namespace BOOOS
{
    class Task : public Queue::Element
    {
    public:
      
    	enum State
    	{
    		READY,
    		WAITING,
    		RUNNING,
    		FINISHING
    	};
        
      
    	Task( void ( *entry_point )( void* ), int nargs, void* arg );
    	virtual ~Task();
    	
      
    	int tid();
    	State state();
    	void pass_to(Task* t, State s = READY);
        void yield();
    	void exit(int code);
        void nice(int p);
    	
   
    	static Task* self();
    	static void init();
       
        
        
    private:
     
    	Task();
      
    	static volatile Task* __running;
    	static volatile Task* __main;
    	
    	
        static int __tidCounter;
        static Queue __ready;
        
        static int const STACK_SIZE = 32768;
        char * _stack;
        bool hasStack=false;
    
    	State _state;
      
    	int _tid; // task ID
        ucontext_t _context;	
    
    	
    }; // end class Task : public Queue::Element
    
} // end namespace BOOOS 

#endif // TASK_H_










