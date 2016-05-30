/*
 * BOOOS.h
 *
 *  Created on: Aug 14, 2014
 */

#ifndef BOOOS_H_
#define BOOOS_H_


/**
 * To creates the operation system components over an namespace called BOOOS, as in the C++ build-in
 * 'std::' namespace.
 */
namespace BOOOS
{
    // Configuration Space
    
    /**
     * 
     */
    class BOOOS 
    {
    public:
        
        /**
         * Creates an operation system instance.
         * 
         * @param verbose    whether the debugging is enabled or not. If ommited, the default value
         *                   is true (enabled).
         */
    	BOOOS(bool verbose = true);
    	
    	/**
    	 * Destructs the operation system instance.
    	 */
    	~BOOOS();
        
        /**
         * The current operatin system version.
         */
    	static const int BOOOS_VERSION = 0;
        
        /**
         * Stops the computer from responding to the user. Only an manual and complete system restart
         * would exits this state.
         */
    	void panic();
        
        
    private:
        
        /**
         * Whether or not the operating system is on debug mode.
         */
    	bool _verbose;
    };

} // end namespace BOOOS

#endif // BOOOS_H_


