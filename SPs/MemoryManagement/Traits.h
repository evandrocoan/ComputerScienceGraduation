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
 *  0   = Disables this feature.
 *  1  >= MemoryManager debugging.
 *  2   = Enable the Traits<Debug> pauseOnEveryEvent.
 *  4   = Enable all Traits<Debug> levels.
 */
#define DEBUG_LEVEL 0


#define DEBUG_LEVEL_DISABLED_DEBUG       0
#define DEBUG_LEVEL_BASIC_DEBUG          1
#define DEBUG_LEVEL_PAUSE_ON_EVERY_EVENT 2
#define DEBUG_LEVEL_ALL_TRAITS_LEVELS    4


/**
 * MemoryManager debugging.
 */
#if DEBUG_LEVEL > DEBUG_LEVEL_DISABLED_DEBUG

/**
 * A value like a127 (111111) for 'g_debugLevel' enables all 'a' mask debugging levels. To enable all
 * debugging levels at once, use "a127 b127 c127" etc, supposing the level 64 is the highest to each
 * mask 'a', 'b', 'c', etc.
 * 
 * MemoryManager debugging:
 * a0   - Disabled all debug.
 * a1   - Basic debug messages.
 * a2   - Functions entrances.
 * a4   - I AM IN HERER MESSAGE.
 * a8   - Partitions handling as in deletePartition(1) and addPartition(1).
 * a16  - Show the teacher required output.
 * a32  - MemoryManager::showMemory(0) and Algorithm::getPartition(1) debugging.
 * 
 * Algorithm Strategy debugging:
 * b0   - Disabled all debug.
 * b1   - _FirstFit::allocateMemory(1) debugging.
 * b2   - _NextFit::allocateMemory(1) debugging.
 * b3   - _WorstFit::allocateMemory(1) debugging.
 * b4   - _BestFit::allocateMemory(1) debugging.
 */
const char* const g_debugLevel = "a2 a8 a16 b4 a32";


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

#if DEBUG_LEVEL == DEBUG_LEVEL_PAUSE_ON_EVERY_EVENT
    static const bool error = 0;
    static const bool warning = 0;
    static const bool trace = 0; //false;
    static const bool info = 0; //true;
    static const bool fine = 0; //true;
    //
    static const bool showEntityAttributes = 0;
    static const bool showListOfEvents = 0;
    static const bool pauseOnEveryEvent = 1; //true;

#elif DEBUG_LEVEL == DEBUG_LEVEL_ALL_TRAITS_LEVELS
    static const bool error = 1;
    static const bool warning = 1;
    static const bool trace = 1; //false;
    static const bool info = 1; //true;
    static const bool fine = 1; //true;
    //
    static const bool showEntityAttributes = 1;
    static const bool showListOfEvents = 1;
    static const bool pauseOnEveryEvent = 1; //true;
#else
    static const bool error = 0;
    static const bool warning = 0;
    static const bool trace = 0; //false;
    static const bool info = 0; //true;
    static const bool fine = 0; //true;
    //
    static const bool showEntityAttributes = 0;
    static const bool showListOfEvents = 0;
    static const bool pauseOnEveryEvent = 0; //true;
#endif
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
