/*
 * BOOOS.h
 *
 *  Created on: Aug 14, 2014
 */


#ifndef QUEUE_H_
#define QUEUE_H_


/**
 * 
 */
#include <iostream>
#include <string>
#include <cstdarg>
#include <set>
#include <list>
#include <sstream>

inline int incrementFuckingCounter()
{
    static int g_fuker_output_counter = 0;
    return g_fuker_output_counter++;
}



/**
 * This is to view internal program data while execution. Default value: 0
 * 
 *  0   = Disables this feature.
 *  1  >= MemoryManager debugging.
 */
#define DEBUG_LEVEL 1

#define G_DEBUG_MINIMUM_LINE_TO_PRINT 0
#define G_DEBUG_MAXIMUM_LINE_TO_PRINT 500


#define DEBUG_LEVEL_DISABLED_DEBUG 0
#define DEBUG_LEVEL_BASIC_DEBUG    1



/**
 * Debugging definitions.
 */
#if DEBUG_LEVEL > DEBUG_LEVEL_DISABLED_DEBUG

/**
 * A value like a127 (111111) for 'g_debugLevel' enables all 'a' mask debugging levels. To enable all
 * debugging levels at once, use "a127 b127 c127" etc, supposing the level 64 is the highest to each
 * mask 'a', 'b', 'c', etc.
 * 
 * Queue debugging:
 * a0   - Disabled all debug.
 * a1   - Basic debug messages.
 * a2   - Functions entrances.
 * a4   - Element debugging.
 * 
 * Task debugging:
 * b0   - Disabled all debug.
 * b1   - 
 * b2   - 
 * b3   - 
 * b4   - 
 */
const char* const g_debugLevel = "a 8 b 127";


#endif


/**
 * Debugging definitions.
 */
#if DEBUG_LEVEL > DEBUG_LEVEL_DISABLED_DEBUG
#define DEBUG

/**
 * Print like function for logging putting a new line at the end of string. See the variables
 * 'g_debugLevel', 'g_debugMask', for the avalibles levels.
 * 
 * @param level     the debugging desired level to be printed.
 * @param ...       variable number os formating arguments parameters.
 */
#define DEBUGGERLN( level, ... ) \
do \
{ \
    if( __computeDeggingLevel( #level ) ) \
    { \
        PRINT_FUCKING_TEXT( format( __VA_ARGS__ ) << std::endl ); \
    } \
} \
while( 0 )


/**
 * The same as DEBUGGERLN(...) just below, but do not put automatically a new line.
 */
#define DEBUGGER( level, ... ) \
do \
{ \
    if( __computeDeggingLevel( #level ) ) \
    { \
            PRINT_FUCKING_TEXT( format( __VA_ARGS__ ) ); \
    } \
} \
while( 0 )


/**
 * The same as DEBUGGER(...), but it is for standard program output.
 */
#define FPRINT( level, ... ) \
do \
{ \
    if( __computeDeggingLevel( #level ) ) \
    { \
        PRINT_FUCKING_TEXT( format( __VA_ARGS__ ) ); \
    } \
} \
while( 0 )


/**
 * The same as DEBUGGERLN(...), but it is for standard program output.
 */
#define FPRINTLN( level, ... ) \
do \
{ \
    if( __computeDeggingLevel( #level ) ) \
    { \
        PRINT_FUCKING_TEXT( format( __VA_ARGS__ ) << std::endl ); \
    } \
} \
while( 0 )


#define PRINT_FUCKING_TEXT( X ) \
do \
{ \
    int minimum = G_DEBUG_MINIMUM_LINE_TO_PRINT; \
    int maximum = G_DEBUG_MAXIMUM_LINE_TO_PRINT; \
    int fucker = incrementFuckingCounter(); \
    std::stringstream inputed; \
    inputed << X; \
    if( fucker == minimum ) \
        std::cout << "\n\n\n\nTHIS IS THE MINIMUM! The shit text just started below your head.\n" << std::endl; \
    if( fucker >= minimum && fucker <= maximum ) \
        std::cout << format( "%3d: %s", fucker, inputed.str().c_str() ); \
    if( fucker == maximum ) \
        std::cout << "\n\n\n\nTHIS IS THE MAXIMUM! The shit text just ended above your head.\n" << std::endl; \
} \
while( 0 )


#include <execinfo.h>
#include <stdlib.h>
#include <unistd.h>
#include <stdio.h>
#include <cstring>

/**
 * Print to the standard out stream the stack trace until this call.
 */
inline void __printBacktrace()
{
#define BACKTRACE_SIZE 100
    
    int traceIndex;
    int traceLevels;
    
    void *buffer[ BACKTRACE_SIZE ];
    char **strings;
    
    traceLevels = backtrace( buffer, BACKTRACE_SIZE );
    std::cout << "backtrace() returned " << traceLevels << " addresses" << std::endl;
    
    strings = backtrace_symbols( buffer, traceLevels );
    
    if( strings == NULL )
    {
        std::cout << "ERROR! We failure at failing!" << std::endl;
        std::cout << "There are none backtrace_symbols!" << std::endl;
        exit( EXIT_FAILURE );
    }
    else
    {
        for( traceIndex = 0; traceIndex < traceLevels; traceIndex++ )
        {
            std::cout << strings[traceIndex] << std::endl;
        }
    }
    
    free( strings );
}

/**
 * Determines whether the given debug level is enabled.
 * 
 * @param debugLevel       the given char* string level to the debugger.
 * @return true when the current debug output is enabled, false otherwise.
 */
inline bool __computeDeggingLevel( const char* debugLevel )
{
#define COMPUTE_DEBUGGING_LEVEL_DEBUG      0
#define COMPUTE_DEBUGGING_DEBUG_INPUT_SIZE 32
    
    int inputLevel;
    int builtInLevel;
    
    int inputLevelSize;
    int builtInLevelSize;
    
    int inputLevelTokenSize;
    int builtInLevelTokenSize;
    
    char* inputLevelToken;
    char* builtInLevelToken;
    
    char builtInLevelChar[ COMPUTE_DEBUGGING_DEBUG_INPUT_SIZE ];
    char inputLevelChar  [ COMPUTE_DEBUGGING_DEBUG_INPUT_SIZE ];
    char inputLevelChars [ COMPUTE_DEBUGGING_DEBUG_INPUT_SIZE ][ COMPUTE_DEBUGGING_DEBUG_INPUT_SIZE ];
    
    int        inputLevels  = 0;
    const char separator[2] = " ";
    
    inputLevelSize   = strlen( debugLevel );
    builtInLevelSize = strlen( g_debugLevel );
    
    if( 2 > inputLevelSize > COMPUTE_DEBUGGING_DEBUG_INPUT_SIZE
        || 2 > builtInLevelSize > COMPUTE_DEBUGGING_DEBUG_INPUT_SIZE )
    {
        std::cout << "ERROR while processing the DEBUG LEVEL: " << debugLevel << std::endl;
        std::cout << "! The masks sizes are " << inputLevelSize << " and " << builtInLevelSize;
        std::cout << ", but they must to be between 1 and 32." << std::endl;
        
        __printBacktrace();
        exit( EXIT_FAILURE );
    }
    
    strcpy( inputLevelChar, debugLevel );
    strcpy( builtInLevelChar, g_debugLevel );
    
    // So, how do we debug the debugger?
#if COMPUTE_DEBUGGING_LEVEL_DEBUG > 0
    int currentExternLoop = 0;
    int currentInternLoop = 0;

    std::cout << "\ng_debugLevel: " << g_debugLevel << ", builtInLevelSize: " << builtInLevelSize ;
    std::cout << ", debugLevel: " << debugLevel << ", inputLevelSize: " << inputLevelSize  << std::endl;
#endif
    
    inputLevelToken = strtok( inputLevelChar, separator );
    
    do
    {
        strcpy( inputLevelChars[ inputLevels++ ], inputLevelToken );
    } while( ( inputLevelToken = strtok( NULL, separator ) ) != NULL );
    
    while( inputLevels-- > 0 )
    {
    #if COMPUTE_DEBUGGING_LEVEL_DEBUG > 0
        currentInternLoop = 0;
        std::cout << "CURRENT_ExternLoop: " << currentExternLoop++ << std::endl;
    #endif
        
        builtInLevelToken   = strtok( builtInLevelChar, separator );
        inputLevelTokenSize = strlen( inputLevelChars[ inputLevels ] );
        
        do
        {
            builtInLevelTokenSize = strlen( builtInLevelToken );
            
        #if COMPUTE_DEBUGGING_LEVEL_DEBUG > 0
            std::cout << "space" << std::endl;
            std::cout << "CURRENT_InternLoop: " << currentInternLoop++ << std::endl;
            
            std::cout << "builtInLevelToken: " << builtInLevelToken << std::endl;
            std::cout << "builtInLevelTokenSize: " << builtInLevelTokenSize << std::endl;
            std::cout << "inputLevelChars[" << inputLevels << "]: " << inputLevelChars[ inputLevels ] << std::endl;
            std::cout << "inputLevelTokenSize: " << inputLevelTokenSize << std::endl;
        #endif
            
            if( inputLevelTokenSize > 0
                && builtInLevelTokenSize > 0 )
            {
                if( isdigit( inputLevelChars[ inputLevels ][ 1 ] )
                    && isdigit( builtInLevelToken[ 1 ] ) )
                {
                    if( builtInLevelToken[ 0 ] == inputLevelChars[ inputLevels ][ 0 ] )
                    {
                        sscanf( &inputLevelChars[ inputLevels ][ 1 ], "%d", &inputLevel );
                        sscanf( &builtInLevelToken[ 1 ], "%d", &builtInLevel );
                        
                    #if COMPUTE_DEBUGGING_LEVEL_DEBUG > 0
                        std::cout << "builtInLevel: " << builtInLevel << std::endl;
                        std::cout << "inputLevel: " << inputLevel << std::endl;
                        std::cout << "Is activeated? " << ( ( inputLevel & builtInLevel ) > 0 ) << std::endl;
                    #endif
                        
                        if( ( inputLevel & builtInLevel ) > 0 )
                        {
                            return true;
                        }
                    }
                }
            }
            
        } while( ( builtInLevelToken = strtok( NULL, separator ) ) != NULL );
        
    #if COMPUTE_DEBUGGING_LEVEL_DEBUG > 0
        std::cout << "space" << std::endl;
    #endif
    }
    
    return false;
}


#else
    #define DEBUGGER( stream, ... )
    #define DEBUGGERLN( stream, ... )


/**
 * The same as DEBUGGER(...), but it is for standard program output when the debugging is disabled.
 */
#define FPRINT( level, ... ) \
do \
{ \
    std::cout << format( __VA_ARGS__ ); \
} \
while( 0 )


/**
 * The same as DEBUGGERLN(...), but it is for standard program output when the debugging is disabled.
 */
#define FPRINTLN( level, ... ) \
do \
{ \
    std::cout << format( __VA_ARGS__ ) << std::endl; \
} \
while( 0 )


#endif // #if DEBUG_LEVEL > 0



/**
 *  Calculates a static array size.
 */
#if !defined STATIC_ARRAY_SIZE
    #define STATIC_ARRAY_SIZE( array ) ( sizeof( ( array ) ) / sizeof( ( array )[0] ) )

#endif



/**
 * Missing string printf. This is safe and convenient but not exactly efficient.
 * 
 * @param fmt     a char array
 * @param ...     a variable length number of formating characters.
 * 
 * @see http://stackoverflow.com/a/10150393/4934640
 * @see http://stackoverflow.com/questions/2342162/stdstring-formatting-like-sprintf/10150393#10150393
 */
inline std::string format(const char* fmt, ...)
{
    int   size   = 512;
    char* buffer = 0;
    
    buffer = new char[size];
    
    va_list vl;
    
    va_start(vl, fmt);
    
    int nsize = vsnprintf(buffer, size, fmt, vl);
    
    //fail delete buffer and try again
    if(size<=nsize)
    { 
        delete[] buffer;
        
        buffer = 0;
        buffer = new char[nsize+1]; //+1 for /0
        nsize  = vsnprintf(buffer, size, fmt, vl);
    }
    
    std::string ret(buffer);
    
    va_end(vl);
    
    delete[] buffer;
    
    return ret;
}






/**
* @see BOOOS namespace declaration at the main file 'BOOOS.h'.
*/
namespace BOOOS
{

    
    /**
     * Implements a queue abtraction using a double linked list.
     */
    class Queue
    {
    public:
        
        /**
         * Implements a queue node which holds its information as its rank, next and previous
         * elements on the queue. This elementa data must to be explicitly updated every time
         * the queue is changed.
         */
        class Element
        {
        public:
            
            /**
             * Creates a new Element object.
             */
            Element()
            {
                DEBUGGERLN( a2, "I AM ENTERING ON BOOOS::Queue::Element::Element(0) | THE CONSTRUCTOR!" );
                
                this->_prev = 0;
                this->_next = 0;
                this->_rank = 0;
            }
            
            /**
             * Free the memory used by this object.
             */
            virtual ~Element()
            {
                DEBUGGERLN( a2, "I AM ENTERING ON BOOOS::Queue::Element::~Element(0) | THE DESTRUCTOR! this->_rank: %d", this->_rank );
                
                // this->_prev = 0;
                // this->_next = 0;
                // this->_rank = 0;
            }
            
            /**
             * Gets the previus element this double linked list is pointing to.
             * 
             * @return null when there is not any next element, otherwise, an Element object
             *         pointer.
             */
            Element* prev()
            {
                DEBUGGERLN( a 2 a4, "I AM ENTERING ON BOOOS::Queue::Element::prev(0) | this->_rank: %d", this->_rank );
                return this->_prev;
            }
            
            /**
             * Gets the next element this double linked list is pointing to.
             * 
             * @return null when there is not any next element, otherwise, an Element object
             *      pointer.
             */
            Element* next()
            { 
                DEBUGGERLN( a 2 a4, "I AM ENTERING ON BOOOS::Queue::Element::next(0) | this->_rank: %d", this->_rank );
                return this->_next;
            }
            
            /**
             * Gets this element rank on the double linked list.
             * 
             * @return this element rank.
             */
            int rank()
            {
                DEBUGGERLN( a 2 a4, "I AM ENTERING ON BOOOS::Queue::Element::rank(0) | this->_rank: %d", this->_rank );
                return this->_rank; 
            }
            
            /**
             * Sets the previous element to this on the double linked list.
             * 
             * @param p    the previous element to this on the linked list.
             */
            void prev( Element* p )
            { 
                DEBUGGERLN( a 2 a4, "I AM ENTERING ON BOOOS::Queue::Element::prev(1) | this->_rank: %d", this->_rank );
                this->_prev = p;
            }
            
            /**
             * Sets the next element to this on the double linked list.
             * 
             * @param p    the next element to this on the double linked list.
             */
            void next( Element* p )
            {
                DEBUGGERLN( a 2 a4, "I AM ENTERING ON BOOOS::Queue::Element::next(1) | this->_rank: %d", this->_rank );
                this->_next = p;
            }
            
            /**
             * Set this element rank on the linked list.
             * 
             * @param r    this element rank on the double linked list.
             */
            void rank(int r)
            { 
                DEBUGGERLN( a 2 a4, "I AM ENTERING ON BOOOS::Queue::Element::rank(1) | this->_rank: %d", this->_rank );
                this->_rank = r;
            }
            
            
        private:
            
            /**
             * Holds the previous element pointer to this current element on the double linked
             * list.
             */
            Element* _prev;
            
            /**
             * Holds the next element pointer to this current element on the double linked list.
             */
            Element* _next;
            
            /**
             * Holds current element rank on the double linked list.
             */
            int _rank;
        };
        
        
        /**
         * Queue constructor: must initialize queue's attributes, to create an empty Queue object
         * ready to receive elements.
         * 
         * @see BOOOS::Queue::_head member class atribute.
         */
        Queue() : _head()
        {
            DEBUGGERLN( a2, "I AM ENTERING ON BOOOS::Queue::Queue(0) | THE CONSTRUCTOR!" );
            this->_length = 0;
            this->_head.rank( -1 );
        }
        
        /**
         * Queue destructor: must finalize queue's attributes
         * 
         * Free the memory used by this object.
         */
        virtual ~Queue()
        {
            DEBUGGERLN( a2, "\n\n\nI AM ENTERING ON BOOOS::Queue::~Queue(0) | THE DESTRUCTOR!" );
            
//int n;
//std::cin >> n;

        #ifdef DEBUGg
            DEBUGGERLN( a1, " ( ~Queue ) | before FOR!" );
            
            int  length         = this->_length + 10;
            auto currentElement = this->_head.next();
            // auto lastElement = _head.prev();
            
            for( int index = 0; index < length; ++index )
            {
                if( currentElement == NULL )
                {
                    DEBUGGERLN( a1, " ( ~Queue ) | BREAKING for NULL | index: %d, this->_length: %d", index, this->_length );
                    break;
                }
                
                DEBUGGERLN( a1, " ( ~Queue ) | inside for | index: %d, _rank: %d", index, currentElement->rank() );
                currentElement = currentElement->next();
            }
        #endif
            
            DEBUGGERLN( a1, " ( ~Queue ) | POINT 1" );
            Element* elem;
            
            do
            {
                DEBUGGERLN( a1, " ( ~Queue ) | POINT 2" );
                
                if( this->_length > 0 )
                {
                    DEBUGGERLN( a1, " ( ~Queue ) | CALLING Remove!" );
                    elem = this->remove();
                    
                    DEBUGGERLN( a1, " ( ~Queue ) | POINT 3" );
    	            delete elem;
                }
                else
                {
                    DEBUGGERLN( a1, " ( ~Queue ) | NOTHING TO REMOVE!" );
                }
                
	            DEBUGGERLN( a1, " ( ~Queue ) | POINT 4" );
            } while( ( this->_head.next() ) != 0 );
            
            DEBUGGERLN( a1, " ( ~Queue ) | POINT 5" );
            elem = this->_head.next();
            
            //DEBUGGERLN( a1, " ( ~Queue ) | POINT 6" );
            //this->_head.next( NULL );
            //
            //DEBUGGERLN( a1, " ( ~Queue ) | POINT 7" );
            //this->_head.prev( NULL );
            
        #ifdef DEBUGg
            DEBUGGERLN( a1, " ( ~Queue ) | before FOR!" );
            
            length         = this->_length + 10;
            currentElement = this->_head.next();
            // auto lastElement = _head.prev();
            
            for( int index = 0; index < length; ++index )
            {
                if( currentElement == NULL )
                {
                    DEBUGGERLN( a1, " ( ~Queue ) | BREAKING for NULL | index: %d, this->_length: %d", index, this->_length );
                    break;
                }
                
                DEBUGGERLN( a1, " ( ~Queue ) | inside for | index: %d, _rank: %d", index, currentElement->rank() );
                currentElement = currentElement->next();
            }
        #endif
            
            this->_length = 0;
            DEBUGGERLN( a1, " ( ~Queue ) | EXITING! this->_length: %d", this->_length );
        }
        
        
        /**
         * Gets the current double linked list head element.
         * 
         * @return an Element pointer to the current double linked list head.
         */
        Element* head()
        { 
            DEBUGGERLN( a 2 a4, "I AM ENTERING ON BOOOS::Queue::head(0)" );
            return &( this->_head ); 
        }
        
        /**
         * 
         */
        int length()
        {
            return this->_length;
        }
        
        /** 
         * Insert method: must insert the Element at the end of the queue.
         * 
         * @param 
         * 
         * @throw 1   an integer to represent an invalid Element (if elem == 0)
         * @throw 2   an integer to represent a duplicated Element (if elem already is on the queue)
         */
        void insert( Element* elem )
        {
            DEBUGGERLN( a2, "\nI AM ENTERING ON BOOOS::Queue::insert(1)" );
            
            if( elem == NULL )
            {
                DEBUGGERLN( a1, " ( insert ) | throwing 1!" );
                throw 1;
            }
            
            if( this->search( elem ) != NULL )
            {
                DEBUGGERLN( a1, " ( insert ) | throwing 2!" );
                throw 2;
            }
            
            if( this->_length == 0 )
            {
                DEBUGGERLN( a1, " ( insert ) | inside '_LENGHT == 0'" );
                
                this->_head.prev( elem );
                this->_head.next( elem );
                
                elem->next( elem );
                elem->prev( elem );
            }
            else
            {
                this->_head.prev()->next( elem );
                elem->prev( this->_head.prev() );
                
                DEBUGGERLN( a1, " ( insert ) | this->_length: %d", this->_length );
              //  elem->rank( this->_length );
                
                DEBUGGERLN( a1, " ( insert ) | elem->rank(): %d", elem->rank() );
                this->_head.prev( elem );
            }
            
            // the first element must to point to the last element as it previous.
            this->_head.next()->prev( this->_head.prev() );
            
            ++( this->_length );
            
        #ifdef DEBUGg
            DEBUGGERLN( a1, " ( insert ) | before FOR!" );
            
            int  length         = this->_length + 10;
            auto currentElement = this->_head.next();
            // auto lastElement = _head.prev();
            
            for( int index = 0; index < length; ++index )
            {
                if( currentElement == NULL )
                {
                    DEBUGGERLN( a1, " ( insert ) | BREAKING for NULL | index: %d, this->_length: %d", index, this->_length );
                    break;
                }
                
                DEBUGGERLN( a1, " ( insert ) | inside for | index: %d, _rank: %d", index, currentElement->rank() );
                currentElement = currentElement->next();
            }
        #endif
            
            DEBUGGERLN( a1, " ( insert ) | EXITING! this->_length: %d", this->_length );
        }
        
        /**
         * 
         */
        void insert_ordered( Element* elem )
        {
            DEBUGGERLN( a2, "I AM ENTERING ON BOOOS::Queue::insert_ordered(1)" );
        }
        
        /**
         * Remove: Must search and remove the first element from the queue.
         * 
         * Error messages:
         *   1: queue is empty.
         */
        Element* remove()
        {
            DEBUGGERLN( a2, "\nI AM ENTERING ON BOOOS::Queue::remove(0) | this->_length: %d", this->_length );
            Element* backup = this->_head.next();
            
            if( this->_length == 0 )
            {
                throw 1;
            }
            
            if( this->_length > 1 )
            {
                DEBUGGERLN( a1, " ( remove ) | this->_length > 1!" );
                
                // the new first element points to NULL as prev
                this->_head.next()->next()->prev( this->_head.prev() );
                this->_head.next( this->_head.next()->next() );
                
                // the last element now points to the new first element.
                this->_head.prev()->next( this->_head.next() );
            }
            else
            {
                DEBUGGERLN( a1, " ( remove ) | this->_length < 2!" );
                
                this->_head.next( NULL );
                this->_head.prev( NULL );
            }
            
            --( this->_length );
            
        #ifdef DEBUGg
            DEBUGGERLN( a1, " ( remove ) | before FOR!" );
            
            int  length         = this->_length + 10;
            auto currentElement = this->_head.next();
            // auto lastElement = _head.prev();
            
            if( currentElement == NULL )
            {
                DEBUGGERLN( a1, " ( remove ) | BREAKING for NULL | currentElement == NULL" );
            }
            
            for( int index = 0; index < length; ++index )
            {
                if( currentElement == NULL )
                {
                    DEBUGGERLN( a1, " ( remove ) | BREAKING for NULL | index: %d, this->_length: %d", index, this->_length );
                    break;
                }
                
                DEBUGGERLN( a1, " ( remove ) | inside for | index: %d, _rank: %d", index, currentElement->rank() );
                currentElement = currentElement->next();
            }
        #endif
            
            DEBUGGERLN( a1, " ( remove ) | EXITING! this->_length: %d", this->_length );
            return backup;
        }
        
        /**
         * 
         */
        void remove( Element* e )
        {
            DEBUGGERLN( a2, "I AM ENTERING ON BOOOS::Queue::remove(1)" );
        }
        
        
    private:
        
        /**
         * An integer holding the current double linked list size/lenght. This would be its 
         * element count number.
         */
        int _length;
        
        /**
         * Holds the current double linked list head Element object.
         * 
         * 1) Calling '_head.next()' will point to its head.
         * 2) calling '_head.prev()' will point to tail.
         */
        Element _head;
        
        /**
         * Search for an element on this double linked list.
         * 
         * @param elem       an element pointer to be found.
         * @return null the element is not found, otherwise an Element pointer to the found
         *         element.
         */
        Element* search( Element* elem )
        {
            DEBUGGERLN( a2, "I AM ENTERING ON BOOOS::Queue::search(1)" );
            
            auto lastElement = _head.prev();
            
            if( this->_length > 0 )
            {
                DEBUGGERLN( a2, "RUNNING: auto currentElement = this->_head.next();" );
                
                int  maximum        = this->_length - 1;
                auto currentElement = this->_head.next();
                
                do
                {
                    DEBUGGERLN( a2, "RUNNING: if( currentElement == elem )" );
                    
                    if( currentElement == elem )
                    {
                        DEBUGGERLN( a1, " ( search ) | Returnning currentElement!" );
                        return currentElement;
                    }
                    
                    DEBUGGERLN( a2, "RUNNING: currentElement = currentElement->next();" );
                    
                    if( ( currentElement = currentElement->next() ) == NULL )
                    {
                        break;
                    }
                } while( currentElement != lastElement );
            }
            
            DEBUGGERLN( a1, " ( search ) | Returnning NULL!" );
            return NULL;
        }
        
    }; // end class Queue
    
} // end namespace BOOOS

#endif // QUEUE_H_













