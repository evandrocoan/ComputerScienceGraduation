/*
 * Task.cc
 *
 *  Created on: Feb 27, 2014
 */

#include "Task.h"


/**
* @see BOOOS namespace declaration at the main file 'BOOOS.h'.
*/
namespace BOOOS
{
    /**
    * @see BOOOS::Task::tid() member class declaration.
    */
    int BOOOS::Task::tid()
    {
        return _tid;
    }
    
    /**
    * @see BOOOS::Task::self() member class declaration.
    */
	static Task* self()
	{
	    return (Task*) __running;
	}
    
} // end namespace BOOOS











