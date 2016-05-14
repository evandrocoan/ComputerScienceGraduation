#include <iostream>

#include "Traits.h"
#include "MemoryManager.h"



/**
 * @see Algorithm::Algorithm( MemoryManager* ) member class declaration.
 */
Algorithm::Algorithm( MemoryManager* memoryManager ) 
{
    DEBUGGERLN( 2, "I AM ENTERING IN Algorithm::Algorithm(0) THE CONSTRUTOR!" );
    
    this->memoryManager = memoryManager;
}

/**
 * @see Algorithm::~Algorithm() member class declaration.
 */
Algorithm::~Algorithm() 
{
    delete this->memoryManager;
}



/**
 * @see _BestFit::allocateMemory( unsigned int ) member class declaration.
 */
Partition* _BestFit::allocateMemory( unsigned int size ) 
{
    DEBUGGERLN( 2 + 32, "\nI AM ENTERING IN _BestFit::allocateMemory(1)" );
    
    Partition* novo = NULL;
    DEBUGGERLN( 32, "( allocateMemory ) size: %d, this->memoryManager->partitions.size(): %d,", size, this->memoryManager->partitions.size() );
    
    if( this->memoryManager->partitions.size() == 0 )
    {
        novo = new Partition( 0, size - 1, false );
        this->memoryManager->partitions.push_back( *novo );
        
        DEBUGGERLN( 32, "( allocateMemory|size 0 ) novo->getBeginAddress(): %d, \nnovo->getEndAddress(): %d,",
                                                novo->getBeginAddress(),       novo->getEndAddress() );
        
        return novo;
    }
    
    unsigned int holeSize;
    unsigned int partitionIndex;
    unsigned int partitionEndAddress;
    
    Partition* currentPartition = NULL;
    Partition* nextPartition    = NULL;
    
    auto          partitionsListIteratorEnd = this->memoryManager->partitions.end();
    auto          partitionsListIterator    = this->memoryManager->partitions.begin();
    unsigned int  partitionsListSize        = this->memoryManager->partitions.size();
    
    DEBUGGERLN( 32, "( allocateMemory ) partitionsListSize: %d,", partitionsListSize );
    
    for( partitionIndex = 1; partitionIndex < partitionsListSize; ++partitionIndex )
    {
        DEBUGGERLN( 32, "( allocateMemory|for ) partitionIndex: %d,", partitionIndex );
        currentPartition = &( *partitionsListIterator );
        
        ++partitionsListIterator;
        DEBUGGERLN( 32, "( allocateMemory|for ) currentPartition->getBeginAddress(): %d, \ncurrentPartition->getEndAddress(): %d,",
                                                currentPartition->getBeginAddress(),       currentPartition->getEndAddress() );
        
        nextPartition = &( *partitionsListIterator );
        holeSize      = nextPartition->getBeginAddress() - currentPartition->getEndAddress();
        
        DEBUGGERLN( 32, "( allocateMemory|for ) holeSize: %d, \nnextPartition->getBeginAddress(): %d, \nnextPartition->getEndAddress(): %d,",
                                                holeSize,       nextPartition->getBeginAddress(),      nextPartition->getEndAddress() );
        
        if( holeSize >= size )
        {
            break;
        }
        
        if( partitionsListIterator == partitionsListIteratorEnd )
        {
            currentPartition = nextPartition;
            break;
        }
        
        currentPartition = nextPartition;
    }
    
    DEBUGGERLN( 32, "( allocateMemory|after for 1 )" );
    
    if( currentPartition == NULL )
    {
        currentPartition = &( *partitionsListIterator );
    }
    
    DEBUGGERLN( 32, "( allocateMemory|after for 2 )" );
    partitionEndAddress = currentPartition->getEndAddress() + size;
    
    if( partitionEndAddress < memoryManager->maxAddress )
    {
        DEBUGGERLN( 32, "( allocateMemory|after for 3 ) currentPartition->getEndAddress(): %d, \npartitionEndAddress: %d", currentPartition->getEndAddress(), partitionEndAddress );
        novo = new Partition( currentPartition->getEndAddress() + 1, partitionEndAddress, false );
    }
    
    this->memoryManager->partitions.push_back( *novo );
    
    DEBUGGERLN( 32, "( allocateMemory ) novo->getBeginAddress(): %d, \nnovo->getEndAddress(): %d, \nnovo->getLength(): %d",
                                        novo->getBeginAddress(),       novo->getEndAddress(),       novo->getLength() );
    
#if defined DEBUG
    partitionIndex = 0;
    
    for( auto partition : this->memoryManager->partitions )
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
/*    Partition* novo;
    
    DEBUGGERLN( 2, "I AM ENTERING IN _FirstFit::allocateMemory(1)" );
    
    auto this->memoryManager->partitions = this->getPartitions();
    
    if( this->memoryManager->partitions.size() ==0 ) 
    {
        novo = new Partition( 0,size,false );
        
        this->memoryManager->partitions.insert( novo );
        
        return novo;
    }
    
    auto index = this->memoryManager->partitions.begin();
    
    int partSize = 0 + ( *index )->getBeginAddress();
    int pos      = -1;
    
    if( partSize >= size ) 
    {
        pos  = 0;
        novo = new Partition( pos,size,false );
        
        this->memoryManager->partitions.insert( novo );
        
        return novo;
    }
    
    int holeSize, end, beg =0;
    
    for( int i=0; i< this->memoryManager->partitions.size() -1;i++ ) 
    {
        end = ( *index )->getEndAddress();
        
        index++;
        
        beg = ( *index )->getBeginAddress();
        
        holeSize = ( beg- end ) +1;
        
        if( holeSize >= size ) 
        {
            pos  = beg;
            novo = new Partition( pos,size,false );
            
            this->memoryManager->partitions.insert( novo );
            
            return novo;
        }
    }
    
    end = ( *index )->getBeginAddress();
    beg = memoryManager->maxAddress;
    
    holeSize = ( beg- end ) +1;
    
    if( holeSize >= size ) 
    {
        pos  = beg;
        novo = new Partition( pos,size,false );
        
        this->memoryManager->partitions.insert( novo );
        
        return novo;
        
    }
    
    if( pos==-1 ) return 0;
    
    novo = new Partition( pos,size,false );
    
    this->memoryManager->partitions.insert( novo );
    
    return novo;
*/
}



/**
 * @see _NextFit::allocateMemory( unsigned int ) member class declaration.
 */
Partition* _NextFit::allocateMemory( unsigned int size ) 
{
/*    Partition* novo;
    
    DEBUGGERLN( 2, "I AM ENTERING IN _NextFit::allocateMemory(1)" );
    
    auto this->memoryManager->partitions = this->getPartitions();
    
    // Verificamos se não ha particoes, pra inserirmos no começo
    if( this->memoryManager->partitions.size() == 0 ) 
    {
        novo = new Partition( 0,size,false );
        this->memoryManager->partitions.insert( novo );
        return novo;
    }
    
    
    // Movemos o interator até o ultimo lugar que encontramos lugar vazio
    auto index = this->memoryManager->partitions.begin();
    
    for( int i=0; i<lastIndex &&i < this->memoryManager->partitions.size();i++ );
    
    // Aqui Verificamos se achamos um espaço vazio entre o inicio da memoria até 
    // a posição inical da primeira particao
    int partSize = 0 + ( *index )->getBeginAddress();
    int pos      = -1;
    
    if( partSize >= size ) 
    {
        pos  = 0;
        novo = new Partition( pos,size,false );
        this->memoryManager->partitions.insert( novo );
        
        lastIndex= 0;
        
        return novo;
    }
    
    int holeSize,end,beg =0;
    
    // Aqui procuramos um espaço vazio entre as particoes, varrendo nossa lista
    for( int i=lastIndex; i< this->memoryManager->partitions.size() -1;i++ ) 
    {
        end = ( *index )->getEndAddress();
        
        index++;
        
        beg = ( *index )->getBeginAddress();
        
        holeSize = ( beg- end ) +1;
        
        if( holeSize >= size ) 
        {
            pos =beg;
            novo = new Partition( pos,size,false );
             this->memoryManager->partitions.insert( novo );
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
        
        this->memoryManager->partitions.insert( novo );
         
        return novo;
        
    }
    
    // No caso do next fit, não procuramos ainda nas particoes antes do lastIndex, então vamos procurar agora
    index = this->memoryManager->partitions.begin();
     
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
            
            this->memoryManager->partitions.insert( novo );
            
            lastIndex= i;
            
            return novo;
        }
    }
    
    if ( pos==-1 ) return 0;
    
    novo = new Partition( pos,size,false );
    
    this->memoryManager->partitions.insert( novo );
    
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
    
    auto this->memoryManager->partitions = this->getPartitions();
    
    if( this->memoryManager->partitions.size() ==0 ) 
    {
        novo = new Partition( 0, size-1 ,false );
        
        this->memoryManager->partitions.insert( novo );
        
        return novo;
    }
    
    auto index = this->memoryManager->partitions.begin();
    
    int partSize = 0 + ( *index )->getBeginAddress();
    int pos = -1;
    
    if( partSize >= size ) pos =0;
    
    int holeSize,end,beg =0;
    
    for( int i=0; i< this->memoryManager->partitions.size() -1;i++ ) 
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
    
    this->memoryManager->partitions.insert( novo );
    
    return novo;
*/
}

















