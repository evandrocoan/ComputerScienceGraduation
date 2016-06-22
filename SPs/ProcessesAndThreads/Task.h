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
        
        /**
         * 
         */
    	enum State
    	{
    		READY,
    		WAITING,
    		RUNNING,
    		FINISHING
    	};
        
        /**
         * 
         */
    	Task( void ( *entry_point )( void* ), int nargs, void* arg )
    	{
    	    DEBUGGERLN( a2, "I AM ENTERING ON BOOOS::Task::Task(3) | nargs: %d, ", nargs );
    	}
    	
    	/**
    	 * 
    	 */
    	virtual ~Task()
    	{
    	    DEBUGGERLN( a2, "I AM ENTERING ON BOOOS::Task::~Task(0)" );
    	}
        
        /**
         * 
         */
    	int tid()
        {
    	    DEBUGGERLN( a2, "I AM ENTERING ON BOOOS::Task::~Task(0) | returning _tid: %d", this->_tid );
            
            return this->_tid;
        }
        	
    	/**
    	 * 
    	 */
    	State state()
    	{ 
    	    DEBUGGERLN( a2, "I AM ENTERING ON BOOOS::Task::state(0) | returning _state: %d", this->_state );
    	    
    	    return this->_state;
    	}
        
        /**
         * 
         */
    	void pass_to(Task* t, State s = READY)
    	{
    	    DEBUGGERLN( a2, "I AM ENTERING ON BOOOS::Task::pass_to(2) | Task tid: %s, State: %d", t->_tid, s );
    	}
        
        /**
         * 
         */
    	void exit(int code)
    	{
    	    DEBUGGERLN( a2, "I AM ENTERING ON BOOOS::Task::exit(1) | code: %d", code );
    	}
        
        /**
         * static Task* self()
         * 
         * Método de classe (static) que retorna a Task executando no momento.
         */
    	static Task* self()
    	{
    	    DEBUGGERLN( a2, "I AM ENTERING ON BOOOS::Task::self(0) | returning __running->tid: %d", __running->_tid );
    	    
	        return (Task*) Task::__running;
	    }
	    
    	/**
    	 * static void init()
    	 * 
    	 * Método de classe que precisa ser chamando na inicialização do sistema e deve
    	 * inicializar os atributos de classe (static). Atenção, o atributo __main, que é
    	 * static, deve ser inicializado aqui!
    	 */
    	static void init()
        {
            DEBUGGERLN( a2, "I AM ENTERING ON BOOOS::Task::init(0)" );
        }
        
        
    private:
        
        /**
         * 
         */
    	Task()
    	{
    	    DEBUGGERLN( a2, "I AM ENTERING ON BOOOS::Task::Task(0) THE PRIVATE CONSTRUCTOR!" );
    	}
        
        /**
         * 
         */
    	static volatile Task* __running;
        
        /**
         * 
         */
    	State _state;
    	
    	/**
    	 * 
    	 */
    	int _tid; // task ID
    	
    	// ...
    	
    	
    }; // end class Task : public Queue::Element
    
} // end namespace BOOOS 

#endif // TASK_H_










