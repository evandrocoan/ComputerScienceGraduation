#include "Traits.h"
#include "MemoryManager.h"



/**
 * @see Algorithm::Algorithm( MemoryManager* ) member class declaration.
 */
Algorithm::Algorithm() 
{
    DEBUGGERLN( 2, "I AM ENTERING IN Algorithm::Algorithm(0) THE CONSTRUTOR!" );
}

/**
 * @see Algorithm::~Algorithm() member class declaration.
 */
Algorithm::~Algorithm() 
{
}

/**
 * @see Algorithm::addPartition( Partition* newPartition ) member class declaration.
 */
void Algorithm::addPartition( Partition* newPartition )
{
    this->partitionList.push_back( *newPartition );
}

/**
 * @see Algorithm::getPartition( unsigned int index ) member class declaration.
 */
Partition* Algorithm::getPartition( unsigned int index )
{
    if( -1 < index < this->partitionListSize() )
    {
        switch( index )
        {
            case this->lastIndexAccess - 1:
            {
                this->lastIteratorAccess = --( this->lastIteratorAccess );
                break;
            }
            case this->lastIndexAccess:
            {
                break;
            }
            case this->lastIndexAccess + 1:
            {
                this->lastIteratorAccess = ++( this->lastIteratorAccess );
                break;
            }
            default:
            {
                this->lastIteratorAccess = std::next( this->partitionList.begin(), index );
            }
        }
        
        this->lastIndexAccess = index;
    }
    else
    {
        return NULL;
    }
    
    return &( *( this->lastIteratorAccess ) );
}

/**
 * @see Algorithm::deletePartition( Partition* currentPartition ) member class declaration.
 */
void Algorithm::deletePartition( Partition* partition )
{
    this->partitionList.erase( std::find( this->partitionList.begin(), this->partitionList.end(), *partition ) );
}

/**
 * @see Algorithm::partitionListSize() member class declaration.
 */
unsigned int Algorithm::partitionListSize()
{
    return this->partitionList.size();
}

/**
 * @see Algorithm::getPartitionsListIterator() member class declaration.
 */
std::list< Partition >::const_iterator Algorithm::getPartitionsListIterator()
{
    return this->partitionList.cbegin();
}



/**
 * @see _BestFit::allocateMemory( unsigned int ) member class declaration.
 */
Partition* _BestFit::allocateMemory( unsigned int size ) 
{
    DEBUGGERLN( 2 + 32, "\nI AM ENTERING IN _NextFit::allocateMemory(1)" );
    
    Partition* novo = NULL;
    DEBUGGERLN( 32, "( allocateMemory ) size: %d, this->partitionList.size(): %d,", size, this->partitionList.size() );
    
    if( this->partitionList.size() == 0 )
    {
        novo = new Partition( 0, size - 1, false );
        this->addPartition( novo );
        
        DEBUGGERLN( 32, "( allocateMemory|size 0 ) novo->getBeginAddress(): %d, \nnovo->getEndAddress(): %d,",
                                                novo->getBeginAddress(),       novo->getEndAddress() );
        
        return novo;
    }
    
    unsigned int holeSize;
    unsigned int partitionIndex;
    unsigned int partitionEndAddress;
    
    Partition* currentPartition = NULL;
    Partition* nextPartition    = NULL;
    
    auto          partitionsListIteratorEnd = this->partitionList.end();
    auto          partitionsListIterator    = this->partitionList.begin();
    unsigned int  partitionsListSize        = this->partitionList.size();
    
    DEBUGGERLN( 32, "( allocateMemory ) partitionsListSize: %d,", partitionsListSize );
    
    for( partitionIndex = 1; partitionIndex < partitionsListSize; ++partitionIndex )
    {
        DEBUGGERLN( 32, "( allocateMemory|for ) partitionIndex: %d,", partitionIndex );
        
        currentPartition = &( *partitionsListIterator );
        ++partitionsListIterator;
        
        DEBUGGERLN( 32, "( allocateMemory|for ) currentPartition->getBeginAddress(): %d, \ncurrentPartition->getEndAddress(): %d,",
                                                currentPartition->getBeginAddress(),       currentPartition->getEndAddress() );
        
        nextPartition = &( *partitionsListIterator );
        holeSize      = nextPartition->getBeginAddress() - currentPartition->getEndAddress() - 1;
        
        DEBUGGERLN( 32, "( allocateMemory|for ) nextPartition->getBeginAddress(): %d, \nnextPartition->getEndAddress(): %d, \nholeSize: %d,",
                                                nextPartition->getBeginAddress(),       nextPartition->getEndAddress(),       holeSize );
        
        if( holeSize >= size )
        {
            DEBUGGERLN( 32, "( allocateMemory|for ) EXITING BY HOLE SIZE COMPATIBLE!" );
            break;
        }
        
        currentPartition = nextPartition;
        
        if( partitionsListIterator == partitionsListIteratorEnd )
        {
            DEBUGGERLN( 32, "( allocateMemory|for ) EXITING BY REACHING THE ITERATOR'S END!" );
            break;
        }
    }
    
    DEBUGGERLN( 32, "( allocateMemory|after for 1 )" );
    
    if( currentPartition == NULL )
    {
        currentPartition = &( *partitionsListIterator );
    }
    
    DEBUGGERLN( 32, "( allocateMemory|after for 2 )" );
    
    partitionEndAddress = currentPartition->getEndAddress() + size;
    DEBUGGERLN( 32, "( allocateMemory|after for 3 )" );
    
    if( partitionEndAddress < MemoryManager::maxAddress + 1 )
    {
        DEBUGGERLN( 32, "( allocateMemory|after for 4 ) currentPartition->getEndAddress(): %d, \npartitionEndAddress: %d", currentPartition->getEndAddress(), partitionEndAddress );
        
        novo = new Partition( currentPartition->getEndAddress() + 1, partitionEndAddress, false );
        this->partitionList.push_back( *novo );
        
        DEBUGGERLN( 32, "( allocateMemory ) novo->getBeginAddress(): %d, \nnovo->getEndAddress(): %d, \nnovo->getLength(): %d",
                                            novo->getBeginAddress(),       novo->getEndAddress(),       novo->getLength() );
    }
    
#if defined DEBUG
    partitionIndex = 0;
    
    for( auto partition : this->partitionList )
    {
        DEBUGGERLN( 32, "( allocateMemory|DEBUG ) partitionIndex: %d, \npartition.getBeginAddress(): %d, \npartition.getEndAddress(): %d, \npartition.getLength(): %d",
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
    DEBUGGERLN( 2 + 32, "\nI AM ENTERING IN _FirstFit::allocateMemory(1)" );
    
    Partition* novo = NULL;
    DEBUGGERLN( 32, "( allocateMemory ) size: %d, this->partitionList.size(): %d,", size, this->partitionList.size() );
    
    if( this->partitionList.size() == 0 )
    {
        novo = new Partition( 0, size - 1, false );
        this->addPartition( novo );
        
        DEBUGGERLN( 32, "( allocateMemory|size 0 ) novo->getBeginAddress(): %d, \nnovo->getEndAddress(): %d,",
                                                novo->getBeginAddress(),       novo->getEndAddress() );
        
        return novo;
    }
    
    unsigned int holeSize;
    unsigned int partitionIndex;
    unsigned int partitionEndAddress;
    
    Partition* currentPartition = NULL;
    Partition* nextPartition    = NULL;
    
    auto          partitionsListIteratorEnd = this->partitionList.end();
    auto          partitionsListIterator    = this->partitionList.begin();
    unsigned int  partitionsListSize        = this->partitionList.size();
    
    DEBUGGERLN( 32, "( allocateMemory ) partitionsListSize: %d,", partitionsListSize );
    
    for( partitionIndex = 1; partitionIndex < partitionsListSize; ++partitionIndex )
    {
        DEBUGGERLN( 32, "( allocateMemory|for ) partitionIndex: %d,", partitionIndex );
        
        currentPartition = &( *partitionsListIterator );
        ++partitionsListIterator;
        
        DEBUGGERLN( 32, "( allocateMemory|for ) currentPartition->getBeginAddress(): %d, \ncurrentPartition->getEndAddress(): %d,",
                                                currentPartition->getBeginAddress(),       currentPartition->getEndAddress() );
        
        nextPartition = &( *partitionsListIterator );
        holeSize      = nextPartition->getBeginAddress() - currentPartition->getEndAddress() - 1;
        
        DEBUGGERLN( 32, "( allocateMemory|for ) nextPartition->getBeginAddress(): %d, \nnextPartition->getEndAddress(): %d, \nholeSize: %d,",
                                                nextPartition->getBeginAddress(),       nextPartition->getEndAddress(),       holeSize );
        
        if( holeSize >= size )
        {
            DEBUGGERLN( 32, "( allocateMemory|for ) EXITING BY HOLE SIZE COMPATIBLE!" );
            break;
        }
        
        currentPartition = nextPartition;
        
        if( partitionsListIterator == partitionsListIteratorEnd )
        {
            DEBUGGERLN( 32, "( allocateMemory|for ) EXITING BY REACHING THE ITERATOR'S END!" );
            break;
        }
    }
    
    DEBUGGERLN( 32, "( allocateMemory|after for 1 )" );
    
    if( currentPartition == NULL )
    {
        currentPartition = &( *partitionsListIterator );
    }
    
    DEBUGGERLN( 32, "( allocateMemory|after for 2 )" );
    
    partitionEndAddress = currentPartition->getEndAddress() + size;
    DEBUGGERLN( 32, "( allocateMemory|after for 3 )" );
    
    if( partitionEndAddress < MemoryManager::maxAddress + 1 )
    {
        DEBUGGERLN( 32, "( allocateMemory|after for 4 ) currentPartition->getEndAddress(): %d, \npartitionEndAddress: %d", currentPartition->getEndAddress(), partitionEndAddress );
        
        novo = new Partition( currentPartition->getEndAddress() + 1, partitionEndAddress, false );
        this->partitionList.push_back( *novo );
        
        DEBUGGERLN( 32, "( allocateMemory ) novo->getBeginAddress(): %d, \nnovo->getEndAddress(): %d, \nnovo->getLength(): %d",
                                            novo->getBeginAddress(),       novo->getEndAddress(),       novo->getLength() );
    }
    
#if defined DEBUG
    partitionIndex = 0;
    
    for( auto partition : this->partitionList )
    {
        DEBUGGERLN( 32, "( allocateMemory|DEBUG ) partitionIndex: %d, \npartition.getBeginAddress(): %d, \npartition.getEndAddress(): %d, \npartition.getLength(): %d",
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
    
    auto this->memoryManager->partitionList = this->getPartitions();
    
    // Verificamos se não ha particoes, pra inserirmos no começo
    if( this->memoryManager->partitionList.size() == 0 ) 
    {
        novo = new Partition( 0,size,false );
        this->memoryManager->partitionList.insert( novo );
        return novo;
    }
    
    
    // Movemos o interator até o ultimo lugar que encontramos lugar vazio
    auto index = this->memoryManager->partitionList.begin();
    
    for( int i=0; i<lastIndex &&i < this->memoryManager->partitionList.size();i++ );
    
    // Aqui Verificamos se achamos um espaço vazio entre o inicio da memoria até 
    // a posição inical da primeira particao
    int partSize = 0 + ( *index )->getBeginAddress();
    int pos      = -1;
    
    if( partSize >= size ) 
    {
        pos  = 0;
        novo = new Partition( pos,size,false );
        this->memoryManager->partitionList.insert( novo );
        
        lastIndex= 0;
        
        return novo;
    }
    
    int holeSize,end,beg =0;
    
    // Aqui procuramos um espaço vazio entre as particoes, varrendo nossa lista
    for( int i=lastIndex; i< this->memoryManager->partitionList.size() -1;i++ ) 
    {
        end = ( *index )->getEndAddress();
        
        index++;
        
        beg = ( *index )->getBeginAddress();
        
        holeSize = ( beg- end ) +1;
        
        if( holeSize >= size ) 
        {
            pos =beg;
            novo = new Partition( pos,size,false );
             this->memoryManager->partitionList.insert( novo );
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
        
        this->memoryManager->partitionList.insert( novo );
         
        return novo;
        
    }
    
    // No caso do next fit, não procuramos ainda nas particoes antes do lastIndex, então vamos procurar agora
    index = this->memoryManager->partitionList.begin();
     
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
            
            this->memoryManager->partitionList.insert( novo );
            
            lastIndex= i;
            
            return novo;
        }
    }
    
    if ( pos==-1 ) return 0;
    
    novo = new Partition( pos,size,false );
    
    this->memoryManager->partitionList.insert( novo );
    
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
    
    auto this->memoryManager->partitionList = this->getPartitions();
    
    if( this->memoryManager->partitionList.size() ==0 ) 
    {
        novo = new Partition( 0, size-1 ,false );
        
        this->memoryManager->partitionList.insert( novo );
        
        return novo;
    }
    
    auto index = this->memoryManager->partitionList.begin();
    
    int partSize = 0 + ( *index )->getBeginAddress();
    int pos = -1;
    
    if( partSize >= size ) pos =0;
    
    int holeSize,end,beg =0;
    
    for( int i=0; i< this->memoryManager->partitionList.size() -1;partitionList++ ) 
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
    
    this->memoryManager->partitionList.insert( novo );
    
    return novo;
*/
}

















