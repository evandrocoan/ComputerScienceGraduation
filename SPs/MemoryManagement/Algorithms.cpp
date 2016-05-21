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
    DEBUGGERLN( a2 a8, "I AM ENTERING IN Algorithm::addPartition(2) | Begin: %u", newPartition->getBeginAddress() );
    
    bool isTheLastPosition = ( ( ( ( (int64_t) this->partitionListSize() ) - this->g_lastIndexAccess ) < 2 )
                               || this->g_lastIndexAccess == DISABLED_LAST_PARTITION_INDEX );
    DEBUGGERLN( a8, "( addPartition ) | isTheLastPosition: %d, \ninsertBeforeIterator: %d, \n( int64_t ) this->g_lastIndexAccess: %lld, "
                                        "\ntheLastPositionValue: %lld,",
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
    DEBUGGERLN( a2 a32, "I AM ENTERING IN Algorithm::getPartition(1) | index: %d", index );
    DEBUGGERLN( a32, "( getPartition ) | this->partitionListSize(): %d, \nthis->g_lastIndexAccess: %d,",
                                         this->partitionListSize(),       this->g_lastIndexAccess );
    
    if( -1 < index < this->partitionListSize() )
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
    DEBUGGERLN( a2 a8, "I AM ENTERING IN Algorithm::deletePartition(1) | Begin: %u", partition->getBeginAddress() );
    
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
    
    DEBUGGERLN( a2, "I AM ENTERING IN Algorithm::partitionListSize(0) | size: %u, openedCount: %d", this->g_partitionList.size(), ++openedCount );
    return this->g_partitionList.size();
}



/**
 * @see _BestFit::addPartition( Partition* partition ) member class declaration.
 */
void _BestFit::addPartition( Partition* partition, bool insertBeforeIterator )
{
    g_lastAllocationBeginAddress = partition->getBeginAddress();
    this->Algorithm::addPartition( partition, insertBeforeIterator );
}

/**
 * @see _BestFit::deletePartition( Partition* partition ) member class declaration.
 */
void _BestFit::deletePartition( Partition* partition )
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
 * @see _BestFit::allocateMemory( unsigned int ) member class declaration.
 */
Partition* _BestFit::allocateMemory( unsigned int size ) 
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
    
    bool         insertBeforeIterator = false;;
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
            
            DEBUGGERLN( b2, "( allocateMemory|size 0 ) novo->getBeginAddress(): %d, \nnovo->getEndAddress(): %d,",
                                                       novo->getBeginAddress(),       novo->getEndAddress() );
        }
        
        return novo;
    }
    
    unsigned int holeSize;
    unsigned int partitionIndex;
    unsigned int partitionEndAddress;
    
    Partition* currentPartition = NULL;
    Partition* nextPartition    = NULL;
    
    DEBUGGERLN( b1, "( allocateMemory ) partitionsListSize: %d,", partitionsListSize );
    
    currentPartition = this->getPartition( 0 );
    holeSize         = currentPartition->getBeginAddress();
    
    // holeSize: 1, size: 1 =: 1 = 1 OK
    if( holeSize >= size )
    {
        DEBUGGERLN( b2, "( allocateMemory|for ) EXITING BY HOLE SIZE COMPATIBLE AT FIRST POSITION!" );
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
                break;
            }
            
            currentPartition = nextPartition;
        }
    }
    
    DEBUGGERLN( b1, "( allocateMemory|after for 1 )" );
    partitionEndAddress = currentPartition->getEndAddress() + size;
    DEBUGGERLN( b1, "( allocateMemory|after for 2 )" );
    
    // partitionEndAddress: 1, maxAddress: 1, =: 1 < 2 OK
    if( partitionEndAddress < MemoryManager::maxAddress + 1 )
    {
        DEBUGGERLN( b1, "( allocateMemory|after for 3 ) currentPartition->getEndAddress(): %d, \npartitionEndAddress: %d", currentPartition->getEndAddress(), partitionEndAddress );
        
        novo = new Partition( currentPartition->getEndAddress() + 1, partitionEndAddress, false );
        this->addPartition( novo, true );
        
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
 * @see _NextFit::allocateMemory( unsigned int ) member class declaration.
 */
Partition* _NextFit::allocateMemory( unsigned int size ) 
{
/*    Partition* novo;
    
    DEBUGGERLN( 2, "I AM ENTERING IN _NextFit::allocateMemory(1)" );
    
    auto this->memoryManager->g_partitionList = this->getPartitions();
    
    // Verificamos se não ha particoes, pra inserirmos no começo
    if( this->memoryManager->partitionListSize() == 0 ) 
    {
        novo = new Partition( 0,size,false );
        this->memoryManager->g_partitionList.insert( novo );
        return novo;
    }
    
    
    // Movemos o interator até o ultimo lugar que encontramos lugar vazio
    auto index = this->memoryManager->g_partitionList.begin();
    
    for( int i=0; i<lastIndex &&i < this->memoryManager->partitionListSize();i++ );
    
    // Aqui Verificamos se achamos um espaço vazio entre o inicio da memoria até 
    // a posição inical da primeira particao
    int partSize = 0 + ( *index )->getBeginAddress();
    int pos      = -1;
    
    if( partSize >= size ) 
    {
        pos  = 0;
        novo = new Partition( pos,size,false );
        this->memoryManager->g_partitionList.insert( novo );
        
        lastIndex= 0;
        
        return novo;
    }
    
    int holeSize,end,beg =0;
    
    // Aqui procuramos um espaço vazio entre as particoes, varrendo nossa lista
    for( int i=lastIndex; i< this->memoryManager->partitionListSize() -1;i++ ) 
    {
        end = ( *index )->getEndAddress();
        
        index++;
        
        beg = ( *index )->getBeginAddress();
        
        holeSize = ( beg- end ) +1;
        
        if( holeSize >= size ) 
        {
            pos =beg;
            novo = new Partition( pos,size,false );
             this->memoryManager->g_partitionList.insert( novo );
               lastIndex= i;
            return novo;
        }
    }
    
    // Caso ainda não encontremos espaco, procuramos entre a ultima particao e o final da memoria
    end = ( *index )->getBeginAddress();
    beg = memoryManager->maxAddress;
    
    holeSize = ( beg- end ) +1;
    
    if( holeSize >= size ) 
    {
        pos  = beg;
        novo = new Partition( pos,size,false );
        
        this->memoryManager->g_partitionList.insert( novo );
         
        return novo;
        
    }
    
    // No caso do next fit, não procuramos ainda nas particoes antes do lastIndex, então vamos procurar agora
    index = this->memoryManager->g_partitionList.begin();
     
    for( int i=0; i< lastIndex;i++ ) 
    {
        end = ( *index )->getEndAddress();
        
        
        index++;
        
        beg = ( *index )->getBeginAddress();
        
         holeSize = ( beg- end ) +1;
        
        if( holeSize >= size ) 
        {
            pos  = beg;
            novo = new Partition( pos,size,false );
            
            this->memoryManager->g_partitionList.insert( novo );
            
            lastIndex= i;
            
            return novo;
        }
    }
    
    if ( pos==-1 ) return 0;
    
    novo = new Partition( pos,size,false );
    
    this->memoryManager->g_partitionList.insert( novo );
    
    return novo;
*/
}



/**
 * @see _WorstFit::allocateMemory( unsigned int ) member class declaration.
 */
Partition* _WorstFit::allocateMemory( unsigned int size ) 
{
/*    Partition* novo;
    
    DEBUGGERLN( 2, "I AM ENTERING IN _WorstFit::allocateMemory(1)" );
    
    auto this->memoryManager->g_partitionList = this->getPartitions();
    
    if( this->memoryManager->partitionListSize() ==0 ) 
    {
        novo = new Partition( 0, size-1 ,false );
        
        this->memoryManager->g_partitionList.insert( novo );
        
        return novo;
    }
    
    auto index = this->memoryManager->g_partitionList.begin();
    
    int partSize = 0 + ( *index )->getBeginAddress();
    int pos = -1;
    
    if( partSize >= size ) pos =0;
    
    int holeSize,end,beg =0;
    
    for( int i=0; i< this->memoryManager->partitionListSize() -1;g_partitionList++ ) 
    {
        end = ( *index )->getEndAddress();
        
        index++;
        
        beg = ( *index )->getBeginAddress();
        
        holeSize = ( beg- end ) +1;
        
        if( holeSize >= size ) 
        {
            if( holeSize > partSize ) 
            {
                pos = beg;
                partSize = holeSize;
            }
        }
    }
    
    end = ( *index )->getBeginAddress();
    beg = memoryManager->maxAddress;
    
    holeSize = ( beg- end ) +1;
    
    if( holeSize >= size ) 
    {
        if( holeSize > partSize ) 
        {
            pos = beg;
            partSize = holeSize;
        }
    }
    
    if ( pos==-1 ) return 0;
    
    novo = new Partition( pos,size-1,false );
    
    this->memoryManager->g_partitionList.insert( novo );
    
    return novo;
*/
}

















