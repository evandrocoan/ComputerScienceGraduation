/*
 * Task.cc
 *
 *  Created on: Feb 27, 2014
 */

#include "Task.h"
#include <iostream>
using namespace std;

/**
* @see BOOOS namespace declaration at the main file 'BOOOS.h'.
*/
namespace BOOOS
{
	/**
	* @see BOOOS::Task::__running member class declaration.
	*/

	volatile Task* Task:: __running = 0;
	volatile Task* Task:: __main = 0;

	int Task::__tidCounter = 0;
	Queue Task::__ready = Queue();

    Task::Task ( void ( *entry_point ) ( void* ), int nargs, void* arg ) 
	
	{
		DEBUGGERLN ( a8, "I AM ENTERING ON BOOOS::Task::Task(3) | id: %d, ", this );
		
		_stack = new char[STACK_SIZE];
		_state = READY;
		getcontext(&this->_context);
		
		if(_stack)hasStack = true;
	    this->_context.uc_stack.ss_sp = _stack;
		this->_context.uc_stack.ss_size = STACK_SIZE;
		this->_context.uc_stack.ss_flags = 0;
		this->_context.uc_link = 0;
	   

		makecontext(&this->_context, (void (*)()) entry_point, nargs, arg);
		_tid = __tidCounter;
		__tidCounter++;

		rank ( 0 );
	//	__ready.insert ( this );


	}
	BOOOS::Task::Task()
	{
		DEBUGGERLN ( a8, "I AM ENTERING ON BOOOS::Task::Task(0) THE PRIVATE CONSTRUCTOR! | id: %d", this );
		getcontext ( &_context );
	
		_tid = __tidCounter;
		
	
		rank ( 0 );
	}

	/**
	* static void init()
	*
	* Método de classe que precisa ser chamando na inicialização do sistema e deve
	* inicializar os atributos de classe (static). Atenção, o atributo __main, que é
	* static, deve ser inicializado aqui!
	*/
	BOOOS::Task::~Task()
	{
		DEBUGGERLN ( a8, "I AM ENTERING ON BOOOS::Task::~Task() | tid: %d", this->_tid );
		if ( _state != FINISHING && this != __main ) {
	//		exit ( 0 );
		}
		
		if(hasStack){
		    delete [] _stack;
		}


	}
	void Task::init()
	{
		DEBUGGERLN ( a8, "I AM ENTERING ON BOOOS::Task::init(0)" );
		__tidCounter = 0;
		__main = new Task();
		__main->_state = RUNNING;
		__tidCounter++;
		__running = __main;
		return;
	}


	/**
	 * static Task* self()
	 *
	 * Método de classe (static) que retorna a Task executando no momento.
	 */

	Task* Task::self()
	{
		DEBUGGERLN ( a8, "I AM ENTERING ON BOOOS::Task::self(0) | returning __running->tid: %d", __running->_tid );

		return ( Task* ) __running;
	}
	void Task::exit ( int code )
	{
		DEBUGGERLN ( a8, "I AM ENTERING ON BOOOS::Task::exit(1) | tid: %d", this->_tid );
	    //__ready.remove(this );
		this->pass_to ( ( Task* ) __main, FINISHING );

	}
	void Task::pass_to ( Task* t, State s )
	{
		DEBUGGERLN ( a8, "I AM ENTERING ON BOOOS::Task::pass_to(2) | Task tid: %d, State: %d", t , s );
		_state = s;
		__running = t;
	    
		if ( s == READY ) {
		    try{
		//	__ready.insert ( this );
		    }
		    catch(int e){
		      DEBUGGERLN ( a8, "BOOOS::Task::pass_to(2) | Exception:%d", e );
	
		    }
		}
	//	__ready.remove(t);
		t->_state = RUNNING;
		swapcontext ( &this->_context, &t->_context );

	}
	Task::State Task::state()
	{
		DEBUGGERLN ( a8, "I AM ENTERING ON BOOOS::Task::state(0) | returning _state: %d", this->_state );

		return this->_state;
	}
	int Task::tid()
	{
		DEBUGGERLN ( a8, "I AM ENTERING ON BOOOS::Task::tid()| returning _tid: %d", this->_tid );

		return _tid;
	}




} // end namespace BOOOS











