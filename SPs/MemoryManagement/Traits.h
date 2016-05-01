/*
 * File:   Traits.h
 * Author: cancian
 *
 * Created on September 27, 2015, 4:16 PM
 */



#ifndef TRAITS_H
#define	TRAITS_H

#include "Debug.h"
#include "CPU.h"
#include "Thread.h"
#include "Model.h"
#include "Scheduler.h"
#include "Process.h"

#include <iostream>
#include <string>
#include <cstdarg>



/**
 * This is to view internal program data while execution. Default value: 0
 * 
 *  0   - Disables this feature.
 *  1   - Normal debug.
 */
#define DEBUG_LEVEL 1



#if DEBUG_LEVEL > 0

/**
 * 0   - Disabled all debug.
 * 1   - Basic debug messages.
 * 2   - Functions entrances.
 * 4   - I AM IN HERER MESSAGE.
 * 8   - Partitioning creation.
 * 16  - Show the teacher required output.
 * 32  - _BestFit::allocateMemory( 1 ) debugging.
 * 
 * 
 * 63  - Enables all debugging levels (111111).
 */
const int g_debugLevel = 2 + 32;

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



#if DEBUG_LEVEL > 0
    #define DEBUG


/**
* Print like function for logging putting a new line at the end of string. Following its explanations:
* 
* // Print to the specified output stream the formatting args. If the variable argument is left
* // out when the DEBUGGER macro is used, then the comma before the ## will be deleted.
* fprintf( stream, ##__VA_ARGS__ ); 
* 
* // Print a new line.
* fprintf( stream, "\n" );
* 
* // Flushes the output stream to avoid double output over '>'. Example: './main > results.txt'
* // would get doubled/... print.
* fflush( stream );
* 
* // To allow to use ';' semicolon over the macro statement use and still to be able to use it
* // within an unbraced if statement.
* while( 0 )
* */
#define DEBUGGERLN( level, ... ) \
do \
{ \
    if( level & g_debugLevel ) \
    { \
        std::cout << format( __VA_ARGS__ ) << std::endl; \
    } \
} \
while( 0 )


/**
* The same as DEBUGGERLN(...) just below, but do not put automatically a new line.
* */
#define DEBUGGER( level, ... ) \
do \
{ \
    if( level & g_debugLevel ) \
    { \
            std::cout << format( __VA_ARGS__ ); \
    } \
} \
while( 0 )


/**
The same as DEBUGGER(...), but it is for standard program output.
*/
#define FPRINT( level, ... ) \
do \
{ \
    if( level & g_debugLevel ) \
    { \
        std::cout << format( __VA_ARGS__ ); \
    } \
} \
while( 0 )


/**
* The same as DEBUGGERLN(...), but it is for standard program output.
* */
#define FPRINTLN( level, ... ) \
do \
{ \
    if( level & g_debugLevel ) \
    { \
        std::cout << format( __VA_ARGS__ ) << std::endl; \
    } \
} \
while( 0 )


#else
    #define DEBUGGER( stream, ... )
    #define DEBUGGERLN( stream, ... )


/**
The same as DEBUGGER(...), but it is for standard program output when the debugging is disabled.
*/
#define FPRINT( level, ... ) \
do \
{ \
    std::cout << format( __VA_ARGS__ ); \
} \
while( 0 )


/**
The same as DEBUGGERLN(...), but it is for standard program output when the debugging is disabled.
* */
#define FPRINTLN( level, ... ) \
do \
{ \
    std::cout << format( __VA_ARGS__ ) << std::endl; \
} \
while( 0 )


#endif // #if DEBUG_LEVEL > 0



template<typename T>
struct Traits 
{
    static const bool enabled = true;
    static const bool debugged = true;
};

template<> struct Traits<Process> 
{
    static constexpr double timeBetweenCreations = 50.0; // time units
    static constexpr unsigned int minAddressSpace = 10e3; // bytes
    static constexpr unsigned int maxAddressSpace = 200e3; // bytes
};

template<> struct Traits<Debug> 
{ // CHANGE THE DEBUG LEVEL HERE SETTING THE LEVELS YOU WANT TO SHOW
    // debug levels
    static const bool error = 0;
    static const bool warning = 0;
    static const bool trace = 0; //false;
    static const bool info = 0; //true;
    static const bool fine = 0; //true;
    //
    static const bool showEntityAttributes = 0;
    static const bool showListOfEvents = 0;
    static const bool pauseOnEveryEvent = 0; //true;
};

template<> struct Traits<CPU> 
{
    static constexpr double context_switch_overhead = 1.0; // time units
    static constexpr double timer_interrupt_period = 100.0; // time units
};

template<> struct Traits<Thread> 
{
    static constexpr double minCpuBurst = 200.0;   // time units
    static constexpr double maxCpuBurst = 500.0;   // time units
    static constexpr int maxBursts = 5;            // CPUBurst
    static constexpr int minThreadsPerProcess = 1; // threads
    static constexpr int maxThreadsPerProcess = 1; // threads
};

template<> struct Traits<Model> 
{
    static constexpr double simulationLength = 5000.0; // time units
    static constexpr double firstCreation = 0.0;       // time units
};

template<> struct Traits<MemoryManager> 
{
    static constexpr unsigned int physicalMemorySize = 1e6; // bytes
};

template<> struct Traits<Scheduler> 
{
    static constexpr double timeSlice = 300.0; // time units
};



/**
 *  Calculates a static array size.
 * */
#if !defined STATIC_ARRAY_SIZE
    #define STATIC_ARRAY_SIZE( array ) ( sizeof( ( array ) ) / sizeof( ( array )[0] ) )

#endif



#endif	/* TRAITS_H */
