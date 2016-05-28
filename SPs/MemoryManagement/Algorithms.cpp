
#include <limits.h>
#include "Traits.h"
#include "MemoryManager.h"



/**
 * @see Algorithm::Algorithm( MemoryManager* ) member class declaration.
 */
Algorithm::Algorithm() : g_partitionList(), g_lastIteratorAccess()
{
    DEBUGGERLN( a2, "I AM ENTERING IN Algorithm::Algorithm(0) THE CONSTRUCTOR!" );
}

/**
 * @see Algorithm::~Algorithm() member class declaration.
 */
Algorithm::~Algorithm() 
{
    DEBUGGERLN( a2, "I AM ENTERING IN Algorithm::~Algorithm(0) THE DESTRUCTOR!" );
}

/**
 * @see Algorithm::addPartition( Partition* newPartition ) member class declaration.
 */
void Algorithm::addPartition( Partition* newPartition, bool insertBeforeIterator )
{
#if defined DEBUG
    static int openedCount = 0;
#endif
    
    DEBUGGERLN( a2 a8, "I AM ENTERING IN Algorithm::addPartition(2) | Begin: %u, openedCount: %d", newPartition->getBeginAddress(), ++openedCount );
    
    bool isTheLastPosition = ( ( ( ( (int64_t) this->partitionListSize() ) - this->g_lastIndexAccess ) < 2 )
                               || this->g_lastIndexAccess == DISABLED_LAST_PARTITION_INDEX );
    DEBUGGERLN( a8, "( addPartition ) | isTheLastPosition: %d, \ninsertBeforeIterator: %d, \n( int64_t ) this->g_lastIndexAccess: %lld, "
                                        "\n( ( (int64_t) this->partitionListSize() ) - this->g_lastIndexAccess: %lld,",
                                        isTheLastPosition,       insertBeforeIterator,       ( int64_t ) this->g_lastIndexAccess,
                                        ( ( (int64_t) this->partitionListSize() ) - this->g_lastIndexAccess ) );
    
    if( isTheLastPosition )
    {
        if( insertBeforeIterator )
        {
            this->g_partitionList.insert( g_lastIteratorAccess, *newPartition );
        }
        else
        {
            this->g_partitionList.push_back( *newPartition );
        }
    }
    else
    {
        if( insertBeforeIterator )
        {
            this->g_partitionList.insert( g_lastIteratorAccess, *newPartition );
        }
        else
        {
            this->g_partitionList.insert( ++g_lastIteratorAccess, *newPartition );
        }
    }
    
    this->g_lastIndexAccess = DISABLED_LAST_PARTITION_INDEX;
}

/**
 * @see Algorithm::getPartition( unsigned int index ) member class declaration.
 */
Partition* Algorithm::getPartition( unsigned int index )
{
#if defined DEBUG
    static int openedCount = 0;
#endif
    
    DEBUGGERLN( a 2 a32, "I AM ENTERING IN Algorithm::getPartition(1) | index: %d, openedCount: %d", index, ++openedCount );
    DEBUGGERLN( a32, "( getPartition ) | this->partitionListSize(): %d, \nthis->g_lastIndexAccess: %d,",
                                         this->partitionListSize(),       this->g_lastIndexAccess );
    DEBUGGERLN( a32, "( getPartition|what? ) "
            "-1 <= %u? %d, "
            "%u < %u? %d, "
            "\n-1 < index && index < this->partitionListSize(): %d",
            index, -1 < index,
            index, this->partitionListSize(), index < this->partitionListSize(),
            -1 < index && index < this->partitionListSize() );
    
    if( 0 <= index
        && index < this->partitionListSize() )
    {
        if( index == this->g_lastIndexAccess + 1 )
        {
            DEBUGGERLN( a32, "( getPartition ) exiting by index == + 1" );
            DEBUGGERLN( a32, "( getPartition ) what is '( *this->g_lastIteratorAccess ).getEndAddress()'? %d,",
                                                        ( *this->g_lastIteratorAccess ).getEndAddress() );
            this->g_lastIteratorAccess = ++( this->g_lastIteratorAccess );
        }
        else if( index == this->g_lastIndexAccess )
        {
            DEBUGGERLN( a32, "( getPartition ) exiting by index ==" );
            goto exit;
        }
        else if( index == this->g_lastIndexAccess - 1 )
        {
            DEBUGGERLN( a32, "( getPartition ) exiting by index == -1" );
            DEBUGGERLN( a32, "( getPartition ) could I increment 'this->g_lastIteratorAccess'? %d,", ( *this->g_lastIteratorAccess ).getEndAddress() );
            this->g_lastIteratorAccess = --( this->g_lastIteratorAccess );
        }
        else
        {
            DEBUGGERLN( a32, "( getPartition ) exiting by index == none" );
            this->g_lastIteratorAccess = std::next( this->g_partitionList.begin(), index );
        }
        
        this->g_lastIndexAccess = index;
    }
    else
    {
        DEBUGGERLN( a32, "( getPartition ) returned NULL;" );
        return NULL;
    }
    
    exit:
    DEBUGGERLN( a32, "( getPartition ) this->g_lastIteratorAccess: %d", this->g_lastIteratorAccess );
    DEBUGGERLN( a32, "( getPartition ) this->g_lastIteratorAccess == this->g_partitionList.end(): %d",
                                      this->g_lastIteratorAccess == this->g_partitionList.end() );
    
    DEBUGGERLN( a32, "( getPartition ) exiting by return &( *( this->g_lastIteratorAccess ) );" );
    return &( *( this->g_lastIteratorAccess ) );
}

/**
 * @see Algorithm::deletePartition( Partition* currentPartition ) member class declaration.
 */
void Algorithm::deletePartition( Partition* partition )
{
#if defined DEBUG
    static int openedCount = 0;
#endif
    
    DEBUGGERLN( a2 a8, "I AM ENTERING IN Algorithm::deletePartition(1) | Begin: %u, openedCount: %d", partition->getBeginAddress(), ++openedCount );
    
    this->g_lastIndexAccess = DISABLED_LAST_PARTITION_INDEX;
    this->g_partitionList.erase( std::find( this->g_partitionList.begin(), this->g_partitionList.end(), *partition ) );
}

/**
 * @see Algorithm::partitionListSize() member class declaration.
 */
unsigned int Algorithm::partitionListSize()
{
#if defined DEBUG
    static int openedCount = 0;
#endif
    
    DEBUGGERLN( a 2, "I AM ENTERING IN Algorithm::partitionListSize(0) | size: %u, openedCount: %d", this->g_partitionList.size(), ++openedCount );
    return this->g_partitionList.size();
}



/**
 * @see _BestFit::allocateMemory( unsigned int ) member class declaration.
 */
Partition* _BestFit::allocateMemory( unsigned int size ) 
{
#if defined DEBUG
    static int openedCount = 0;
#endif
    
    DEBUGGERLN( a2 b4, "\nI AM ENTERING IN _BestFit::allocateMemory(1) | size: %u, openedCount: %d", size, ++openedCount );
    
    Partition*   novo               = NULL;
    unsigned int partitionsListSize = this->partitionListSize();
#if defined DEBUG
    if( openedCount > 28 )
#endif
    DEBUGGERLN( b4, "( allocateMemory ) partitionsListSize: %d,", partitionsListSize );
    
    // maxAddress: 1, size: 2 =: 0 < 1 OK
    if( partitionsListSize == 0 )
    {
        // maxAddress: 1, size: 2 =: 0 < 1 OK
        if( size - 2 < MemoryManager::maxAddress )
        {
            novo = new Partition( 0, size - 1, false );
            this->addPartition( novo, false );
#if defined DEBUG
    if( openedCount > 28 )
#endif
            DEBUGGERLN( b4, "( allocateMemory|size 0 ) novo->getBeginAddress(): %d, \nnovo->getEndAddress(): %d,",
                                                       novo->getBeginAddress(),       novo->getEndAddress() );
        }
        
        return novo;
    }
    
    // Everything you see on this algorithm is used configure the variable 'g_lastIndexAccess' or some other thing
    // which you do not know that exists around here. So, do not go out there changing any attributions or things
    // you think are unnecessary or non-optmized. But why this text? Becuse I do not want to comment every single
    // line stating why exacly it exists or why this call is done that way.
    unsigned int currentHoleSize;
    unsigned int partitionEndAddress;
    unsigned int partitionStartAddress;
    
    unsigned int smallestHoleSize      = UINT_MAX;
    unsigned int smallestHoleSizeIndex = 0;
    
    bool         isReadyToCreate  = false;
    unsigned int partitionIndex   = 0;
    Partition*   currentPartition = NULL;
    Partition*   nextPartition    = NULL;
    
    // Settting this to false, will cause a push back into the partitions list, which is what we just want when
    // it is not fount any big enough hole over the end of the next search.
    bool insertBeforeIterator = false;
    currentPartition          = this->getPartition( 0 );
    currentHoleSize           = currentPartition->getBeginAddress();
#if defined DEBUG
    if( openedCount > 28 )
#endif
    DEBUGGERLN( b4, "( allocateMemory|what? ) %u <= %u? %d, %lld < %u? %d,"
            "\nsize <= currentHoleSize < smallestHoleSize: %d",
            size, currentHoleSize, size <= currentHoleSize,
            currentHoleSize, smallestHoleSize, currentHoleSize < smallestHoleSize,
            size <= currentHoleSize < smallestHoleSize );
    
    // size: 1, currentHoleSize: 2, smallestHoleSize: 10 =: 1 < 2 < 10 OK
    if( size <= currentHoleSize
        && currentHoleSize < smallestHoleSize )
    {
        isReadyToCreate  = true;
        smallestHoleSize = currentHoleSize;
#if defined DEBUG
    if( openedCount > 28 )
#endif
        DEBUGGERLN( b4, "( allocateMemory|for ) UPDATING THE BIGGEST HOLE SIZE TO THE FIRST POSITION!\nsmallestHoleSize: %u", smallestHoleSize );
        
        // Setting this true put it before the current iterator instead of to put it after the interator.
        insertBeforeIterator  = true;
        smallestHoleSizeIndex = 0;
    }
    
    for( partitionIndex = 1; partitionIndex < partitionsListSize; ++partitionIndex )
    {
#if defined DEBUG
    if( openedCount > 28 )
#endif
        DEBUGGERLN( b4, "( allocateMemory|for ) partitionIndex: %d,", partitionIndex );
#if defined DEBUG
    if( openedCount > 28 )
#endif
        DEBUGGERLN( b4, "( allocateMemory|for ) currentPartition->getBeginAddress(): %d, \ncurrentPartition->getEndAddress(): %d,",
                                                currentPartition->getBeginAddress(),       currentPartition->getEndAddress() );
        
        nextPartition   = this->getPartition( partitionIndex );
        currentHoleSize = nextPartition->getBeginAddress() - currentPartition->getEndAddress() - 1;
#if defined DEBUG
    if( openedCount > 28 )
#endif
        DEBUGGERLN( b4, "( allocateMemory|for ) nextPartition->getBeginAddress(): %d, \nnextPartition->getEndAddress(): %d, \ncurrentHoleSize: %d,",
                                                nextPartition->getBeginAddress(),       nextPartition->getEndAddress(),       currentHoleSize );
        
        if( size <= currentHoleSize
            && currentHoleSize < smallestHoleSize )
        {
            isReadyToCreate  = true;
            smallestHoleSize = currentHoleSize;
#if defined DEBUG
    if( openedCount > 28 )
#endif
            DEBUGGERLN( b4, "( allocateMemory|for ) UPDATING THE BIGGEST HOLE SIZE TO THE INDEX %d POSITION!\nsmallestHoleSize: %u",
                                                                                              partitionIndex, smallestHoleSize );
            
            // Specifies that we must to place this new allocated memory between the currentPartition and before the nextPartition.
            insertBeforeIterator  = false;
            smallestHoleSizeIndex = ( partitionIndex < 1 ? 0 : partitionIndex - 1 );
        }
        
        currentPartition = nextPartition;
    }
#if defined DEBUG
    if( openedCount > 28 )
#endif
    DEBUGGERLN( b4, "( allocateMemory|after for 1 )" );
    
    // maxAddress: 1, getEndAddress: 1 =: currentHoleSize: 0 OK
    currentHoleSize = MemoryManager::maxAddress - currentPartition->getEndAddress();
    
    if( size <= currentHoleSize
        && currentHoleSize < smallestHoleSize )
    {
        isReadyToCreate  = true;
        smallestHoleSize = currentHoleSize;
#if defined DEBUG
    if( openedCount > 28 )
#endif
        DEBUGGERLN( b4, "( allocateMemory|for ) UPDATING THE BIGGEST HOLE SIZE TO THE END OF THE MEMORY!\nsmallestHoleSize: %u", smallestHoleSize );
        
        // Specifies that we must to place this new allocated memory between the currentPartition and before the nextPartition.
        insertBeforeIterator  = false;
        smallestHoleSizeIndex = ( partitionIndex < 1 ? 0 : partitionIndex - 1 );
    }
#if defined DEBUG
    if( openedCount > 28 )
#endif
    DEBUGGERLN( b4, "( allocateMemory|after for 2 )" );
    
    // When 'smallestHoleSizeIndex' is 0 and 'insertBeforeIterator' true, it is time to insert this partition as the first on the list.
    if( !smallestHoleSizeIndex
        && insertBeforeIterator )
    {
#if defined DEBUG
    if( openedCount > 28 )
#endif
        DEBUGGERLN( b4, "( allocateMemory ) smallestHoleSizeIndex: 0" );
        
        // This is need to re-update the 'g_lastIndexAccess' used to add the partition at the begging
        currentPartition      = this->getPartition( 0 );
        partitionEndAddress   = size - 1;
        partitionStartAddress = 0;
    }
    else
    {
#if defined DEBUG
    if( openedCount > 28 )
#endif
        DEBUGGERLN( b4, "( allocateMemory ) smallestHoleSizeIndex: %u", smallestHoleSizeIndex );
        
        currentPartition      = this->getPartition( smallestHoleSizeIndex );
        partitionEndAddress   = currentPartition->getEndAddress() + size;
        partitionStartAddress = currentPartition->getEndAddress() + 1;
    }
    
#if defined DEBUG
    if( openedCount > 28 )
#endif
    DEBUGGERLN( b4, "( allocateMemory ) partitionStartAddress: %d, \npartitionEndAddress: %d",
                                        partitionStartAddress,       partitionEndAddress );
    
#if defined DEBUG
    if( openedCount > 28 )
#endif
    DEBUGGERLN( b4, "( allocateMemory|after for 3 )" );
    
    if( isReadyToCreate )
    {
        // partitionEndAddress: 1, maxAddress: 1, =: 1 < 2 OK
        if( partitionEndAddress < MemoryManager::maxAddress + 1 )
        {
#if defined DEBUG
    if( openedCount > 28 )
#endif
            DEBUGGERLN( b4, "( allocateMemory|after for 4 ) partitionStartAddress: %d, \npartitionEndAddress: %d",
                                                            partitionStartAddress,       partitionEndAddress );
            
            novo = new Partition( partitionStartAddress, partitionEndAddress, false );
            this->addPartition( novo, insertBeforeIterator );
#if defined DEBUG
    if( openedCount > 28 )
#endif
            DEBUGGERLN( b4, "( allocateMemory ) novo->getBeginAddress(): %d, \nnovo->getEndAddress(): %d, \nnovo->getLength(): %d",
                                                novo->getBeginAddress(),       novo->getEndAddress(),       novo->getLength() );
        }
    }
    
#if defined DEBUG
    partitionIndex = 0;
    
    for( auto partition : this->g_partitionList )
    {if( openedCount > 28 )
        DEBUGGERLN( b4, "( allocateMemory|DEBUG ) partitionIndex: %d, \npartition.getBeginAddress(): %d, \npartition.getEndAddress(): %d, \npartition.getLength(): %d",
                                                  partitionIndex++,     partition.getBeginAddress(),       partition.getEndAddress(),       partition.getLength() );
    }
    
#endif
    
    return novo;
}



/**
 * @see _FirstFit::allocateMemory( unsigned int ) member class declaration.
 */
Partition* _FirstFit::allocateMemory( unsigned int size ) 
{
    DEBUGGERLN( a2 b1, "\nI AM ENTERING IN _FirstFit::allocateMemory(1) | size: %u", size );
    
    Partition*   novo               = NULL;
    unsigned int partitionsListSize = this->partitionListSize();
    
    DEBUGGERLN( b1, "( allocateMemory ) size: %d, partitionsListSize: %d,", size, partitionsListSize );
    
    // maxAddress: 1, size: 2 =: 0 < 1 OK
    if( partitionsListSize == 0 )
    {
        // maxAddress: 1, size: 2 =: 0 < 1 OK
        if( size - 2 < MemoryManager::maxAddress )
        {
            novo = new Partition( 0, size - 1, false );
            this->addPartition( novo, false );
            
            DEBUGGERLN( b1, "( allocateMemory|size 0 ) novo->getBeginAddress(): %d, \nnovo->getEndAddress(): %d,",
                                                       novo->getBeginAddress(),       novo->getEndAddress() );
        }
        
        return novo;
    }
    
    unsigned int holeSize;
    unsigned int partitionEndAddress;
    unsigned int partitionStartAddress;
    
    unsigned int partitionIndex   = 0;
    Partition*   currentPartition = NULL;
    Partition*   nextPartition    = NULL;
    
    // Settting this to false, will cause a push back into the partitions list, which is what we just want when
    // it is not fount any big enough hole over the end of the next search.
    bool insertBeforeIterator = false;
    currentPartition          = this->getPartition( 0 );
    holeSize                  = currentPartition->getBeginAddress();
    
    // holeSize: 1, size: 1 =: 1 = 1 OK
    if( holeSize >= size )
    {
        DEBUGGERLN( b1, "( allocateMemory|for ) EXITING BY HOLE SIZE COMPATIBLE AT FIRST POSITION!" );
        
        // Setting this true put it before the current iterator instead of to put it after the interator.
        insertBeforeIterator = true;
    }
    else
    {
        for( partitionIndex = 1; partitionIndex < partitionsListSize; ++partitionIndex )
        {
            DEBUGGERLN( b1, "( allocateMemory|for ) partitionIndex: %d,", partitionIndex );
            DEBUGGERLN( b1, "( allocateMemory|for ) currentPartition->getBeginAddress(): %d, \ncurrentPartition->getEndAddress(): %d,",
                                                    currentPartition->getBeginAddress(),       currentPartition->getEndAddress() );
            
            nextPartition = this->getPartition( partitionIndex );
            holeSize      = nextPartition->getBeginAddress() - currentPartition->getEndAddress() - 1;
            
            DEBUGGERLN( b1, "( allocateMemory|for ) nextPartition->getBeginAddress(): %d, \nnextPartition->getEndAddress(): %d, \nholeSize: %d,",
                                                    nextPartition->getBeginAddress(),       nextPartition->getEndAddress(),       holeSize );
            
            if( holeSize >= size )
            {
                DEBUGGERLN( b1, "( allocateMemory|for ) EXITING BY HOLE SIZE COMPATIBLE!" );
                
                // Specifies that we must to place this new allocated memory between the currentPartition and before the nextPartition.
                insertBeforeIterator = true;
                break;
            }
            
            currentPartition = nextPartition;
        }
    }
    
    DEBUGGERLN( b1, "( allocateMemory|after for 1 )" );
    
    if( !partitionIndex )
    {
        partitionEndAddress   = size - 1;
        partitionStartAddress = 0;
    }
    else
    {
        partitionEndAddress   = currentPartition->getEndAddress() + size;
        partitionStartAddress = currentPartition->getEndAddress() + 1;
    }
    
    DEBUGGERLN( b1, "( allocateMemory|after for 2 )" );
    
    // partitionEndAddress: 1, maxAddress: 1, =: 1 < 2 OK
    if( partitionEndAddress < MemoryManager::maxAddress + 1 )
    {
        DEBUGGERLN( b1, "( allocateMemory|after for 3 ) partitionStartAddress: %d, \npartitionEndAddress: %d",
                                                        partitionStartAddress,       partitionEndAddress );
        
        novo = new Partition( partitionStartAddress, partitionEndAddress, false );
        this->addPartition( novo, insertBeforeIterator );
        
        DEBUGGERLN( b1, "( allocateMemory ) novo->getBeginAddress(): %d, \nnovo->getEndAddress(): %d, \nnovo->getLength(): %d",
                                            novo->getBeginAddress(),       novo->getEndAddress(),       novo->getLength() );
    }
    
#if defined DEBUG
    partitionIndex = 0;
    
    for( auto partition : this->g_partitionList )
    {
        DEBUGGERLN( b1, "( allocateMemory|DEBUG ) partitionIndex: %d, \npartition.getBeginAddress(): %d, \npartition.getEndAddress(): %d, \npartition.getLength(): %d",
                                                  partitionIndex++,     partition.getBeginAddress(),       partition.getEndAddress(),       partition.getLength() );
    }
    
#endif
    
    return novo;
}



/**
 * @see _NextFit::addPartition( Partition* partition ) member class declaration.
 */
void _NextFit::addPartition( Partition* partition, bool insertBeforeIterator )
{
    g_lastAllocationBeginAddress = partition->getBeginAddress();
    this->Algorithm::addPartition( partition, insertBeforeIterator );
}

/**
 * @see _NextFit::deletePartition( Partition* partition ) member class declaration.
 */
void _NextFit::deletePartition( Partition* partition )
{
    if( g_lastAllocationBeginAddress >= partition->getBeginAddress() )
    {
        if( g_lastAllocationIndex > 0 )
        {
            --g_lastAllocationIndex;
        }
    }
    
    this->Algorithm::deletePartition( partition );
}

/**
 * @see _NextFit::allocateMemory( unsigned int ) member class declaration.
 */
Partition* _NextFit::allocateMemory( unsigned int size ) 
{
    DEBUGGERLN( b2 + a2, "\nI AM ENTERING IN _NextFit::allocateMemory(1) | size: %u", size );
    
    Partition*   novo               = NULL;
    unsigned int partitionsListSize = this->partitionListSize();
    
    DEBUGGERLN( b2, "( allocateMemory ) size: %d, partitionsListSize: %u,", size, partitionsListSize );
    
    if( partitionsListSize == 0 )
    {
        g_lastAllocationIndex = 0;
        
        // maxAddress: 1, size: 2 =: 0 < 1 OK
        if( size - 2 < MemoryManager::maxAddress )
        {
            novo = new Partition( 0, size - 1, false );
            this->addPartition( novo, false );
            
            DEBUGGERLN( b2, "( allocateMemory|size 0 ) novo->getBeginAddress(): %d, \nnovo->getEndAddress(): %d,",
                                                       novo->getBeginAddress(),       novo->getEndAddress() );
        }
        
        return novo;
    }
    
    unsigned int holeSize;
    unsigned int partitionEndAddress;
    unsigned int partitionStartAddress;
    
    bool         insertBeforeIterator = false;
    Partition*   currentPartition     = NULL;
    Partition*   nextPartition        = NULL;
    unsigned int lastAllocationIndex  = g_lastAllocationIndex;
    
    currentPartition = this->getPartition( g_lastAllocationIndex );
    
    do
    {
        if( ++g_lastAllocationIndex >= partitionsListSize )
        {
            // checks after the last partition
            holeSize = MemoryManager::maxAddress - currentPartition->getEndAddress();
            
            // maxAddress: 1, getEndAddress: 1 =: holeSize: 0 OK
            if( holeSize >= size )
            {
                DEBUGGERLN( b2, "( allocateMemory|for ) EXITING BY HOLE SIZE COMPATIBLE AT THE MEMORY END POSITION!" );
                
                insertBeforeIterator = false;
                break;
            }
            
            // checks before the first partition
            g_lastAllocationIndex = 0;
            currentPartition      = this->getPartition( 0 );
            holeSize              = currentPartition->getBeginAddress();
            
            // holeSize: 1, size: 1 =: 1 = 1 OK
            if( holeSize >= size )
            {
                DEBUGGERLN( b2, "( allocateMemory|for ) EXITING BY HOLE SIZE COMPATIBLE AT FIRST POSITION!" );
                
                insertBeforeIterator = true;
                break;
            }
            
            continue;
        }
        
        DEBUGGERLN( b2, "( allocateMemory|for ) partitionIndex: %d,", g_lastAllocationIndex );
        DEBUGGERLN( b2, "( allocateMemory|for ) currentPartition->getBeginAddress(): %d, \ncurrentPartition->getEndAddress(): %d,",
                                                currentPartition->getBeginAddress(),       currentPartition->getEndAddress() );
        
        nextPartition = this->getPartition( g_lastAllocationIndex );
        holeSize      = nextPartition->getBeginAddress() - currentPartition->getEndAddress() - 1;
        
        DEBUGGERLN( b2, "( allocateMemory|for ) nextPartition->getBeginAddress(): %d, \nnextPartition->getEndAddress(): %d, \nholeSize: %d,",
                                                nextPartition->getBeginAddress(),       nextPartition->getEndAddress(),       holeSize );
        
        if( holeSize >= size )
        {
            DEBUGGERLN( b2, "( allocateMemory|for ) EXITING BY HOLE SIZE COMPATIBLE!" );
            
            // This must to be true, because the this->getPartition(1) function call above, incremented the last
            // accessed position, then are not pointing to currentPartition, we are pointing to the nextPartition,
            // then we must to put this partition between the currentPartition and nextPartition, to keep is ordered.
            insertBeforeIterator = true;
            break;
        }
        
        currentPartition = nextPartition;
    } while( g_lastAllocationIndex != lastAllocationIndex );
    
    DEBUGGERLN( b2, "( allocateMemory|after for 1 )" );
    
    if( holeSize >= size )
    {
        if( g_lastAllocationIndex
            || !insertBeforeIterator )
        {
            partitionEndAddress   = currentPartition->getEndAddress() + size;
            partitionStartAddress = currentPartition->getEndAddress() + 1;
        }
        else
        {
            partitionEndAddress   = size - 1;
            partitionStartAddress = 0;
        }
        
        DEBUGGERLN( b2, "( allocateMemory|after for 2 ) partitionStartAddress: %d, \npartitionEndAddress: %d,",
                                                        partitionStartAddress,       partitionEndAddress );
        
        novo = new Partition( partitionStartAddress, partitionEndAddress, false );
        this->addPartition( novo, insertBeforeIterator );
        
        DEBUGGERLN( b2, "( allocateMemory ) novo->getBeginAddress(): %d, \nnovo->getEndAddress(): %d, \nnovo->getLength(): %d",
                                            novo->getBeginAddress(),       novo->getEndAddress(),       novo->getLength() );
    }
    
#if defined DEBUG
    int partitionIndex = 0;
    
    for( auto partition : this->g_partitionList )
    {
        DEBUGGERLN( b2, "( allocateMemory|DEBUG ) partitionIndex: %d, \npartition.getBeginAddress(): %d, \npartition.getEndAddress(): %d, \npartition.getLength(): %d",
                                                  partitionIndex++,     partition.getBeginAddress(),       partition.getEndAddress(),       partition.getLength() );
    }
    
#endif
    
    return novo;
}



/**
 * @see _WorstFit::allocateMemory( unsigned int ) member class declaration.
 */
Partition* _WorstFit::allocateMemory( unsigned int size ) 
{
    DEBUGGERLN( a2 b3, "\nI AM ENTERING IN _WorstFit::allocateMemory(1) | size: %u", size );
    
    Partition*   novo               = NULL;
    unsigned int partitionsListSize = this->partitionListSize();
    
    DEBUGGERLN( b3, "( allocateMemory ) size: %d, partitionsListSize: %d,", size, partitionsListSize );
    
    // maxAddress: 1, size: 2 =: 0 < 1 OK
    if( partitionsListSize == 0 )
    {
        // maxAddress: 1, size: 2 =: 0 < 1 OK
        if( size - 2 < MemoryManager::maxAddress )
        {
            novo = new Partition( 0, size - 1, false );
            this->addPartition( novo, false );
            
            DEBUGGERLN( b3, "( allocateMemory|size 0 ) novo->getBeginAddress(): %d, \nnovo->getEndAddress(): %d,",
                                                       novo->getBeginAddress(),       novo->getEndAddress() );
        }
        
        return novo;
    }
    
    // Everything you see on this algorithm is used configure the variable 'g_lastIndexAccess' or some other thing
    // which you do not know that exists around here. So, do not go out there changing any attributions or things
    // you think are unnecessary or non-optmized. But why this text? Becuse I do not want to comment every single
    // line stating why exacly it exists or why this call is done that way.
    unsigned int currentHoleSize;
    unsigned int partitionEndAddress;
    unsigned int partitionStartAddress;
    
    unsigned int biggestHoleSize      = 0;
    unsigned int biggestHoleSizeIndex = 0;
    
    unsigned int partitionIndex   = 0;
    Partition*   currentPartition = NULL;
    Partition*   nextPartition    = NULL;
    
    // Settting this to false, will cause a push back into the partitions list, which is what we just want when
    // it is not fount any big enough hole over the end of the next search.
    bool insertBeforeIterator = false;
    currentPartition          = this->getPartition( 0 );
    currentHoleSize           = currentPartition->getBeginAddress();
    
    // currentHoleSize: 1, biggestHoleSize: 1 =: 1 = 1 OK
    if( currentHoleSize > biggestHoleSize )
    {
        biggestHoleSize = currentHoleSize;
        DEBUGGERLN( b3, "( allocateMemory|for ) UPDATING THE BIGGEST HOLE SIZE TO THE FIRST POSITION!\nbiggestHoleSize: %u", biggestHoleSize );
        
        // Setting this true put it before the current iterator instead of to put it after the interator.
        insertBeforeIterator = true;
        biggestHoleSizeIndex = 0;
    }
    
    for( partitionIndex = 1; partitionIndex < partitionsListSize; ++partitionIndex )
    {
        DEBUGGERLN( b3, "( allocateMemory|for ) partitionIndex: %d,", partitionIndex );
        DEBUGGERLN( b3, "( allocateMemory|for ) currentPartition->getBeginAddress(): %d, \ncurrentPartition->getEndAddress(): %d,",
                                                currentPartition->getBeginAddress(),       currentPartition->getEndAddress() );
        
        nextPartition   = this->getPartition( partitionIndex );
        currentHoleSize = nextPartition->getBeginAddress() - currentPartition->getEndAddress() - 1;
        
        DEBUGGERLN( b3, "( allocateMemory|for ) nextPartition->getBeginAddress(): %d, \nnextPartition->getEndAddress(): %d, \ncurrentHoleSize: %d,",
                                                nextPartition->getBeginAddress(),       nextPartition->getEndAddress(),       currentHoleSize );
        
        if( currentHoleSize > biggestHoleSize )
        {
            biggestHoleSize = currentHoleSize;
            DEBUGGERLN( b3, "( allocateMemory|for ) UPDATING THE BIGGEST HOLE SIZE TO THE INDEX %d POSITION!\nbiggestHoleSize: %u",
                                                                                              partitionIndex, biggestHoleSize );
            
            // Specifies that we must to place this new allocated memory between the currentPartition and before the nextPartition.
            insertBeforeIterator = false;
            biggestHoleSizeIndex = partitionIndex;
        }
        
        currentPartition = nextPartition;
    }
    
    DEBUGGERLN( b3, "( allocateMemory|after for 1 )" );
    
    // maxAddress: 1, getEndAddress: 1 =: currentHoleSize: 0 OK
    currentHoleSize = MemoryManager::maxAddress - currentPartition->getEndAddress();
    
    if( currentHoleSize > biggestHoleSize )
    {
        biggestHoleSize = currentHoleSize;
        DEBUGGERLN( b3, "( allocateMemory|for ) UPDATING THE BIGGEST HOLE SIZE TO THE END OF THE MEMORY!\nbiggestHoleSize: %u", biggestHoleSize );
        
        // Specifies that we must to place this new allocated memory between the currentPartition and before the nextPartition.
        insertBeforeIterator = false;
        biggestHoleSizeIndex = ( partitionsListSize < 2 ? 0 : partitionIndex - 1 );
    }
    
    DEBUGGERLN( b3, "( allocateMemory|after for 2 )" );
    
    // When 'biggestHoleSizeIndex' is 0 and 'insertBeforeIterator' true, it is time to insert this partition as the first on the list.
    if( !biggestHoleSizeIndex
        && insertBeforeIterator )
    {
        // This is need to re-update the 'g_lastIndexAccess' used to add the partition at the begging
        currentPartition      = this->getPartition( biggestHoleSizeIndex );
        partitionEndAddress   = size - 1;
        partitionStartAddress = 0;
    }
    else
    {
        currentPartition      = this->getPartition( ( partitionIndex < 1 ? 0 : partitionIndex - 1 ) );
        partitionEndAddress   = currentPartition->getEndAddress() + size;
        partitionStartAddress = currentPartition->getEndAddress() + 1;
    }
    
    DEBUGGERLN( b3, "( allocateMemory|after for 3 )" );
    
    // partitionEndAddress: 1, maxAddress: 1, =: 1 < 2 OK
    if( partitionEndAddress < MemoryManager::maxAddress + 1 )
    {
        DEBUGGERLN( b3, "( allocateMemory|after for 4 ) partitionStartAddress: %d, \npartitionEndAddress: %d",
                                                        partitionStartAddress,       partitionEndAddress );
        
        novo = new Partition( partitionStartAddress, partitionEndAddress, false );
        this->addPartition( novo, insertBeforeIterator );
        
        DEBUGGERLN( b3, "( allocateMemory ) novo->getBeginAddress(): %d, \nnovo->getEndAddress(): %d, \nnovo->getLength(): %d",
                                            novo->getBeginAddress(),       novo->getEndAddress(),       novo->getLength() );
    }
    
#if defined DEBUG
    partitionIndex = 0;
    
    for( auto partition : this->g_partitionList )
    {
        DEBUGGERLN( b3, "( allocateMemory|DEBUG ) partitionIndex: %d, \npartition.getBeginAddress(): %d, \npartition.getEndAddress(): %d, \npartition.getLength(): %d",
                                                  partitionIndex++,     partition.getBeginAddress(),       partition.getEndAddress(),       partition.getLength() );
    }
    
#endif
    
    return novo;
}

















