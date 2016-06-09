/*
* BOOOS.h
*
*  Created on: Aug 14, 2014
*/

#include <BOOOS.h>
#include <Task.h>
#include <iostream>


/**
* @see BOOOS namespace declaration at the main file 'BOOOS.h'.
*/
namespace BOOOS
{
    /**
    * @see BOOOS::BOOOS( bool verbose ) member class declaration.
    */
    BOOOS::BOOOS( bool verbose ) : _verbose(verbose)
    {
    	if(_verbose) std::cout << "Welcome to BOOOS - Basic Object Oriented Operating System!" << std::endl;
        
    	// Call init routines of other components
        
    	Task::init();
    }
    
    /**
    * @see BOOOS::~BOOOS() member class declaration.
    */
    BOOOS::~BOOOS()
    {
    	// Call finish routines of other components (if any)
        
    	if(_verbose) std::cout << "BOOOS ended... Bye!" << std::endl;
    }
    
    /**
    * @see BOOOS::panic() member class declaration.
    */
    void BOOOS::panic()
    {
    	std::cerr << "BOOOSta! Panic!" << std::endl;
    	
    	while(true)
    	{
    	}
    }

} // namespace BOOOS


