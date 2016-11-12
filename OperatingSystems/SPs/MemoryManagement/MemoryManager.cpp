/**
 * File:   MemoryManager.cpp
 * 
 * Authors: Wagner Fernando Gascho ( 12100779 ),
 *          Evandro  Coan (  )
 * 
 * Strategy: INSERT THE MEMORY MANAGEMENT SCHEMA HERE ( THE ALGORITHM YOU ARE IMPLEMENTING -- RANDOMLY GENERATED BY MOODLE AS A VARIATION )
 *
 * Created on 30/04/2016
 */



#include "Debug.h"
#include "MemoryManager.h"
#include "Simulator.h"
#include "Traits.h"


using namespace std;


/**
 * @see Partition::Partition( unsigned int , unsigned int, bool ) member class declaration.
 */
Partition::Partition( unsigned int beginAddress, unsigned int endAddress, bool isFree )
{
#if defined DEBUG
    static int openedCount = 0;
#endif
#if defined DEBUG
    if( openedCount > 28 )
#endif
    DEBUGGERLN( a2, "I AM ENTERING IN Partition::Partition(3) | openedCount: %d", ++openedCount );
    
    _beginAddress = beginAddress;
    _endAddress = endAddress;
    _isFree = isFree;
}

/**
 * @see Partition::getBeginAddress() member class declaration.
 */
unsigned int Partition::getBeginAddress() const 
{
    return _beginAddress;
}

/**
 * @see Partition::getEndAddress() member class declaration.
 */
unsigned int Partition::getEndAddress() const 
{
    return _endAddress;
}

/**
 * @see Partition::getLength() member class declaration.
 */
unsigned int Partition::getLength() const 
{
    return _endAddress - _beginAddress + 1;
}

/**
 * @see Partition::operator<( const Partition& ) member class declaration.
 */
bool Partition::operator<( const Partition& p ) const
{
    return this->_beginAddress < p._beginAddress;
}

/**
 * @see Partition::operator==( const Partition& ) member class declaration.
 */
bool Partition::operator==( const Partition& p ) const // note the implicit "this" pointer is const-qualified
{
    return this->_beginAddress == p._beginAddress;
}



/**
 * @see const unsigned int MemoryManager::maxAddress member class declaration.
 */
const unsigned int MemoryManager::maxAddress = Traits<MemoryManager>::physicalMemorySize - 1;

/**
 * @see MemoryManager::MemoryManager( MemoryAllocationAlgorithm ) member class declaration.
 */
MemoryManager::MemoryManager( MemoryAllocationAlgorithm algorithm )
{
    DEBUGGERLN( a2, "I AM ENTERING IN MemoryManager::MemoryManager(1), MemoryAllocationAlgorithm: %d!", algorithm );
    
    switch( algorithm )
    {
        case FirstFit:
        {
            currentStrategy = new _FirstFit();
            break;
        }
        case NextFit:
        {
            currentStrategy = new _NextFit();
            break;
        }
        case BestFit:
        {
            currentStrategy = new _BestFit();
            break;
        }
        case WorstFit:
        {
            currentStrategy = new _WorstFit();
            break;
        }
        default:
        {
            FPRINT( a16, "\nERROR! Invalid MemoryAllocationAlgorithm num: %d", algorithm );
            exit( EXIT_FAILURE );
        }
    }
    
    DEBUGGERLN( a4, "\n\n\n\n\n\n\nI AM IN HERE %s \n\n\n\n\n\n\n", "In HERER Constructing..." );
}

/**
 * @see MemoryManager::MemoryManager( const MemoryManager& ) member class declaration.
 */
MemoryManager::MemoryManager( const MemoryManager& source )
{
    DEBUGGERLN( a2, "I AM ENTERING IN MemoryManager::MemoryManager(1), THE COPY CONSTRUCTOR!" );
    *( this->currentStrategy ) = *( source.currentStrategy );
}

/**
 * @see MemoryManager::~MemoryManager() member class declaration.
 */
MemoryManager::~MemoryManager()
{
    DEBUGGERLN( a2, "I AM ENTERING IN MemoryManager::~MemoryManager(0)" );
    delete currentStrategy;
    
    DEBUGGERLN( a4, "\n\n\n\n\n\n\nI AM IN HERE %s \n\n\n\n\n\n\n", "In HERER \n\nDESTRUCTING\n\n\n" );
}

/**
 * @see MemoryManager::allocateMemory( unsigned int ) member class declaration.
 */
Partition* MemoryManager::allocateMemory( unsigned int size )
{
    Debug::cout( Debug::Level::trace, "MemoryManager::allocateMemory( " + std::to_string( size ) + " )" );
    
    return currentStrategy->allocateMemory( size );
}

/**
 * @see MemoryManager::deallocateMemory( Partition* ) member class declaration.
 */
void MemoryManager::deallocateMemory( Partition* partition )
{
    Debug::cout( Debug::Level::trace, "MemoryManager::deallocateMemory( " + std::to_string( reinterpret_cast<unsigned long> ( partition ) ) + " )" );
    this->currentStrategy->deletePartition( partition );
}

/**
 * @see MemoryManager::getNumPartitions() member class declaration.
 */
unsigned int MemoryManager::getNumPartitions()
{
    return this->currentStrategy->partitionListSize();
}

/**
 * @see MemoryManager::getPartition( unsigned int ) member class declaration.
 */
Partition* MemoryManager::getPartition( unsigned int index )
{
    return this->currentStrategy->getPartition( index );
}

/**
 * @see MemoryManager::showMemory() member class declaration.
 */
void MemoryManager::showMemory()
{
#if defined DEBUG
    static int openedCount = 0;
#endif
    
    //FPRINT( a16, "\n" );
    DEBUGGERLN( a2 a32, "I AM ENTERING IN MemoryManager::showMemory(0) | openedCount: %d", ++openedCount );
    
    unsigned int partitionListSize = this->currentStrategy->partitionListSize();
    
    if( partitionListSize == 0 )
    {
        FPRINTLN( a16, "0-%u: FREE %u", MemoryManager::maxAddress, MemoryManager::maxAddress + 1 );
        return;
    }
    
    int64_t      holeSize;
    unsigned int nextStartAddress;
    
    Partition*   currentPartition  = this->currentStrategy->getPartition( 0 );
    DEBUGGERLN( a32, "( showMemory ) before currentStartAddress = currentPartition->getBeginAddress();" );
    
    unsigned int currentStartAddress = currentPartition->getBeginAddress();
    unsigned int currentEndAddress   = currentPartition->getEndAddress();
    
    if( currentStartAddress > 0 )
    {
        FPRINTLN( a16, "0-%u: FREE %u", currentStartAddress - 1, currentStartAddress );
    }
    
    for( unsigned int partitionIndex = 1; partitionIndex < partitionListSize; partitionIndex++ )
    {
        FPRINTLN( a16, "%u-%u: ALLOCATED %u", currentStartAddress, currentEndAddress, currentPartition->getLength() );
        
        currentPartition = this->currentStrategy->getPartition( partitionIndex );
        nextStartAddress = currentPartition->getBeginAddress();
        holeSize         = ( (int64_t) nextStartAddress ) - currentEndAddress - 1;
        
        if( holeSize > 0 )
        {
            FPRINTLN( a16, "%u-%u: FREE %lld", currentEndAddress + 1, nextStartAddress - 1, holeSize );
        }
        
        currentStartAddress = currentPartition->getBeginAddress();
        currentEndAddress   = currentPartition->getEndAddress();
    }
    
    FPRINTLN( a16, "%u-%u: ALLOCATED %u", currentStartAddress, currentEndAddress, currentPartition->getLength() );
    holeSize = MemoryManager::maxAddress - currentPartition->getEndAddress();
    
    DEBUGGERLN( a32, "( showMemory ) holeSize: %lld, \ncurrentPartition->getEndAddress():%u, \nMemoryManager::maxAddress: %u,",
                                    holeSize,          currentPartition->getEndAddress(),      MemoryManager::maxAddress );
    
    if( holeSize > 0 )
    {
        FPRINTLN( a16, "%u-%lld: FREE %lld", currentPartition->getEndAddress() + 1, MemoryManager::maxAddress, holeSize );
    }
}


