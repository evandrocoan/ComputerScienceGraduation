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
 * @see Algorithm::getPartitions() member class declaration.
 */
PartitionList* Algorithm::getPartitions()
{
    return &( this->memoryManager->partitions );
}



/**
 * @see _BestFit::allocateMemory( unsigned int ) member class declaration.
 */
Partition* _BestFit::allocateMemory( unsigned int size ) 
{
    DEBUGGERLN( 2 + 32, "I AM ENTERING IN _BestFit::allocateMemory(1)" );
    
    Partition* novo;
    auto partitionsObject = this->getPartitions();
    
    DEBUGGERLN( 32, "( allocateMemory ) size: %d, partitionsObject->size(): %d,", size, partitionsObject->size() );
    
    if( partitionsObject->size() == 0 )
    {
        novo = new Partition( 0, size - 1, false );
        partitionsObject->insert( novo );
        
        return novo;
    }
    
    int  pos      = -1;
    auto index    = partitionsObject->begin();
    int  partSize = 0 + ( *index )->getBeginAddress();
    
    DEBUGGERLN( 32, "( allocateMemory ) partSize: %d, index: %d, pos: %d,", partSize, index, pos );
    
    if( partSize >= size )
    {
        pos =0;
    }
    
    int holeSize;
    int end;
    
    int beg              = 0;
    int forPartitionSize = partitionsObject->size() - 1;
    
    for( int i = 0; i < forPartitionSize; ++i )
    {
        end = ( *index )->getEndAddress();
        index++;
        
        beg      = ( *index )->getBeginAddress();
        holeSize = ( beg - end ) + 1;
        
        DEBUGGERLN( 32, "( allocateMemory|for ) end: %d, index: %d, beg: %d, holeSize: %d,", end, index, beg, holeSize );
        
        if( holeSize >= size ) 
        {
            if( holeSize < partSize ) 
            {
                pos = beg;
                partSize = holeSize;
            }
        }
    }
    
    DEBUGGERLN( 32, "( allocateMemory ) end: %d, index: %d, beg: %d, holeSize: %d,", end, index, beg, holeSize );
    
    beg      = memoryManager->maxAddress;
    end      = ( *index )->getBeginAddress();
    holeSize = ( beg- end ) +1;
    
    DEBUGGERLN( 32, "( allocateMemory ) end: %d, index: %d, beg: %d, holeSize: %d,", end, index, beg, holeSize );
    
    if( holeSize >= size ) 
    {
        if( holeSize < partSize ) 
        {
            pos      = beg;
            partSize = holeSize;
        }
    }
    
    DEBUGGERLN( 32, "( allocateMemory ) end: %d, index: %d, beg: %d, holeSize: %d,", end, index, beg, holeSize );
    
    if( pos==-1 )
    {
        return 0;
    }
    
    novo = new Partition( pos, size - 1, false );
    partitionsObject->insert( novo );
    
    DEBUGGERLN( 32, "( allocateMemory ) size: %d, partitionsObject->size(): %d,", size, partitionsObject->size() );
    
    return novo;
}



/**
 * @see _FirstFit::allocateMemory( unsigned int ) member class declaration.
 */
Partition* _FirstFit::allocateMemory( unsigned int size ) 
{
    Partition* novo;
    
    DEBUGGERLN( 2, "I AM ENTERING IN _FirstFit::allocateMemory(1)" );
    
    auto partitionsObject = this->getPartitions();
    
    if( partitionsObject->size() ==0 ) 
    {
        novo = new Partition( 0,size,false );
        
        partitionsObject->insert( novo );
        
        return novo;
    }
    
    auto index = partitionsObject->begin();
    
    int partSize = 0 + ( *index )->getBeginAddress();
    int pos      = -1;
    
    if( partSize >= size ) 
    {
        pos  = 0;
        novo = new Partition( pos,size,false );
        
        partitionsObject->insert( novo );
        
        return novo;
    }
    
    int holeSize, end, beg =0;
    
    for( int i=0; i< partitionsObject->size() -1;i++ ) 
    {
        end = ( *index )->getEndAddress();
        
        index++;
        
        beg = ( *index )->getBeginAddress();
        
        holeSize = ( beg- end ) +1;
        
        if( holeSize >= size ) 
        {
            pos  = beg;
            novo = new Partition( pos,size,false );
            
            partitionsObject->insert( novo );
            
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
        
        partitionsObject->insert( novo );
        
        return novo;
        
    }
    
    if( pos==-1 ) return 0;
    
    novo = new Partition( pos,size,false );
    
    partitionsObject->insert( novo );
    
    return novo;
}



/**
 * @see _NextFit::allocateMemory( unsigned int ) member class declaration.
 */
Partition* _NextFit::allocateMemory( unsigned int size ) 
{
    Partition* novo;
    
    DEBUGGERLN( 2, "I AM ENTERING IN _NextFit::allocateMemory(1)" );
    
    auto partitionsObject = this->getPartitions();
    
    /*
     * Verificamos se não ha particoes, pra inserirmos no começo
     */
    if( partitionsObject->size() == 0 ) 
    {
        novo = new Partition( 0,size,false );
        partitionsObject->insert( novo );
        return novo;
    }
    
    /*
     * Movemos o interator até o ultimo lugar que encontramos lugar vazio
     */
    auto index = partitionsObject->begin();
    
    for( int i=0; i<lastIndex &&i < partitionsObject->size();i++ );
    
    /* 
     * Aqui Verificamos se achamos um espaço vazio entre o inicio da memoria até 
     * a posição inical da primeira particao
     */
    int partSize = 0 + ( *index )->getBeginAddress();
    int pos      = -1;
    
    if( partSize >= size ) 
    {
        pos  = 0;
        novo = new Partition( pos,size,false );
        partitionsObject->insert( novo );
        
        lastIndex= 0;
        
        return novo;
    }
    
    int holeSize,end,beg =0;
    
    /*
     * Aqui procuramos um espaço vazio entre as particoes, varrendo nossa lista
     */
    for( int i=lastIndex; i< partitionsObject->size() -1;i++ ) 
    {
        end = ( *index )->getEndAddress();
        
        index++;
        
        beg = ( *index )->getBeginAddress();
        
        holeSize = ( beg- end ) +1;
        
        if( holeSize >= size ) 
        {
            pos =beg;
            novo = new Partition( pos,size,false );
             partitionsObject->insert( novo );
               lastIndex= i;
            return novo;
        }
    }
    
    /*Caso ainda não encontremos espaco, procuramos entre a ultima particao e o final da memoria
    */
    end = ( *index )->getBeginAddress();
    beg = memoryManager->maxAddress;
    
    holeSize = ( beg- end ) +1;
    
    if( holeSize >= size ) 
    {
        pos  = beg;
        novo = new Partition( pos,size,false );
        
        partitionsObject->insert( novo );
         
        return novo;
        
    }
    
    /*
     * No caso do next fit, não procuramos ainda nas particoes antes do lastIndex, então vamos procurar agora
     */
     index = partitionsObject->begin();
     
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
            
            partitionsObject->insert( novo );
            
            lastIndex= i;
            
            return novo;
        }
    }
    
    if ( pos==-1 ) return 0;
    
    novo = new Partition( pos,size,false );
    
    partitionsObject->insert( novo );
    
    return novo;
}



/**
 * @see _WorstFit::allocateMemory( unsigned int ) member class declaration.
 */
Partition* _WorstFit::allocateMemory( unsigned int size ) 
{
    Partition* novo;
    
    DEBUGGERLN( 2, "I AM ENTERING IN _WorstFit::allocateMemory(1)" );
    
    auto partitionsObject = this->getPartitions();
    
    if( partitionsObject->size() ==0 ) 
    {
        novo = new Partition( 0, size-1 ,false );
        
        partitionsObject->insert( novo );
        
        return novo;
    }
    
    auto index = partitionsObject->begin();
    
    int partSize = 0 + ( *index )->getBeginAddress();
    int pos = -1;
    
    if( partSize >= size ) pos =0;
    
    int holeSize,end,beg =0;
    
    for( int i=0; i< partitionsObject->size() -1;i++ ) 
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
    
    partitionsObject->insert( novo );
    
    return novo;
}

















