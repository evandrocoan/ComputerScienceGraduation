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
 *  1   - MemoryManager debugging.
 *  2   - Algorithm Strategy debugging.
 */
#define DEBUG_LEVEL 0


/**
 * MemoryManager debugging.
 */
#if DEBUG_LEVEL > 0

/**
 * A value like a127 (111111) for 'g_debugLevel' enables all debugging levels. Use 'g_debugMask'
 * as "a b c" to enable multiple masks simultaneously.
 * 
 * MemoryManager debugging:
 * a0   - Disabled all debug.
 * a1   - Basic debug messages.
 * a2   - Functions entrances.
 * a4   - I AM IN HERER MESSAGE.
 * a8   - Partitioning creation.
 * a16  - Show the teacher required output.
 * a32  - MemoryManager::showMemory(0) debugging.
 * 
 * Algorithm Strategy debugging:
 * b0   - Disabled all debug.
 * b1   - _FirstFit::allocateMemory(1) debugging.
 * b2   - _NextFit::allocateMemory(1) debugging.
 */
const char* const g_debugMask  = "b";
const int         g_debugLevel = 2;


#endif



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



#if DEBUG_LEVEL > 0
    #define DEBUG
    
    #include <execinfo.h>
    #include <stdlib.h>
    #include <unistd.h>
    #include <stdio.h>
    #include <cstring>
    
    inline void __printBacktrace();


/**
 * Determines whether the given debug level is enabled.
 * 
 * @param debugLevel       the given char* string level to the debugger.
 * @return true when the current debug output is enabled, false otherwise.
 */
inline bool __computeDeggingLevel( const char* debugLevel )
{
#define COMPUTE_DEBUGGING_LEVEL_DEBUG 0
    
    char* token;
    char  inputLevelChar[32];
    
    int charIndex;
    int readIndex;
    int charCopyIndex;
    int charTokenSize;
    
    int inputLevelSize;
    int builtInLevelSize;
    int inputLevel;
    
    const char separator[2] = " ";
    
    inputLevelSize   = strlen( debugLevel );
    builtInLevelSize = strlen( g_debugMask );
    
    if( inputLevelSize < 2 )
    {
        std::cout << "ERROR while processing the DEBUG LEVEL: " << debugLevel << "! The mask is missing." << std::endl;
        __printBacktrace();
    }
    
    strcpy( inputLevelChar, debugLevel );
    token = strtok( inputLevelChar, separator );
    
    // So, how do we debug the debugger?
    //
#if COMPUTE_DEBUGGING_LEVEL_DEBUG > 0
    std::cout << "\ng_debugMask: " << g_debugMask << ", debugLevel: " << debugLevel << ", inputLevelSize: ";
    std::cout << inputLevelSize << ", builtInLevelSize: " << builtInLevelSize << std::endl;
    
#endif
    
    for( charIndex = 0; charIndex < inputLevelSize; ++charIndex )
    {
        for( readIndex = 0; readIndex < builtInLevelSize; ++readIndex )
        {
        #if COMPUTE_DEBUGGING_LEVEL_DEBUG > 0
            std::cout << "charIndex: " << charIndex << std::endl;
            std::cout << "readIndex: " << readIndex << std::endl;
            std::cout << "g_debugMask[ readIndex ]: " << g_debugMask[ readIndex ] << std::endl;
            std::cout << "debugLevel[ charIndex ]: " << debugLevel[ charIndex ] << std::endl;
            std::cout << "debugLevel[ charIndex - 1 ]: " << debugLevel[ charIndex - 1 ] << std::endl;
            std::cout << "g_debugLevel: " << g_debugLevel << std::endl;
            std::cout << "token: " << token << std::endl;
            
        #endif
        
            if( ( charTokenSize = strlen( token ) - 1 ) > 0
                && isdigit( token[ 1 ] ) )
            {
                if( g_debugMask[ readIndex ] == token[ 0 ] )
                {
                    for( charCopyIndex = 0; charCopyIndex < charTokenSize; ++charCopyIndex )
                    {
                        inputLevelChar[ charCopyIndex ] = token[ charCopyIndex + 1 ];
                    }
                    
                    inputLevelChar[ charTokenSize ] = '\0';
                    sscanf( inputLevelChar, "%d", &inputLevel );
                    
                #if COMPUTE_DEBUGGING_LEVEL_DEBUG > 0
                    std::cout << "inputLevelChar: " << inputLevelChar << ", charTokenSize: " << charTokenSize << std::endl;
                    std::cout << "inputLevel: " << inputLevel << ", g_debugLevel: " << g_debugLevel << std::endl;
                    
                #endif
                    
                    if( ( inputLevel & g_debugLevel ) > 0 )
                    {
                        return true;
                    }
                }
            }
        }
        
        if( ( token = strtok( NULL, separator ) ) == NULL )
        {
            break;
        }
    }
    
    return false;
}

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
        std::cout << "ERROR there are none backtrace_symbols!" << std::endl;
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
        std::cout << format( __VA_ARGS__ ) << std::endl; \
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
            std::cout << format( __VA_ARGS__ ); \
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
        std::cout << format( __VA_ARGS__ ); \
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
        std::cout << format( __VA_ARGS__ ) << std::endl; \
    } \
} \
while( 0 )


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


#endif	/* TRAITS_H */
