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
    return &( memoryManager->partitions );
}



/**
 * @see _FirstFit::allocateMemory( unsigned int ) member class declaration.
 */
Partition* _FirstFit::allocateMemory( unsigned int size ) 
{
    Partition* novo;
    
    DEBUGGERLN( 2, "I AM ENTERING IN _FirstFit::allocateMemory(1)" );
    
    auto part = getPartitions();
    
    if( part->size() ==0 ) 
    {
        novo = new Partition( 0,size,false );
        
        part->insert( novo );
        
        return novo;
    }
    
    auto index = part->begin();
    
    int partSize = 0 + ( *index )->getBeginAddress();
    int pos      = -1;
    
    if( partSize >= size ) 
    {
        pos  = 0;
        novo = new Partition( pos,size,false );
        
        part->insert( novo );
        
        return novo;
    }
    
    int holeSize, end, beg =0;
    
    for( int i=0; i< part->size() -1;i++ ) 
    {
        end = ( *index )->getEndAddress();
        
        index++;
        
        beg = ( *index )->getBeginAddress();
        
        holeSize = ( beg- end ) +1;
        
        if( holeSize >= size ) 
        {
            pos  = beg;
            novo = new Partition( pos,size,false );
            
            part->insert( novo );
            
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
        
        part->insert( novo );
        
        return novo;
        
    }
    
    if( pos==-1 ) return 0;
    
    novo = new Partition( pos,size,false );
    
    part->insert( novo );
    
    return novo;
}



/**
 * @see _NextFit::allocateMemory( unsigned int ) member class declaration.
 */
Partition* _NextFit::allocateMemory( unsigned int size ) 
{
    Partition* novo;
    
    DEBUGGERLN( 2, "I AM ENTERING IN _NextFit::allocateMemory(1)" );
    
    auto part = getPartitions();
    
    /*
     * Verificamos se não ha particoes, pra inserirmos no começo
     */
    if( part->size() == 0 ) 
    {
        novo = new Partition( 0,size,false );
        part->insert( novo );
        return novo;
    }
    
    /*
     * Movemos o interator até o ultimo lugar que encontramos lugar vazio
     */
    auto index = part->begin();
    
    for( int i=0; i<lastIndex &&i < part->size();i++ );
    
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
        part->insert( novo );
        
        lastIndex= 0;
        
        return novo;
    }
    
    int holeSize,end,beg =0;
    
    /*
     * Aqui procuramos um espaço vazio entre as particoes, varrendo nossa lista
     */
    for( int i=lastIndex; i< part->size() -1;i++ ) 
    {
        end = ( *index )->getEndAddress();
        
        index++;
        
        beg = ( *index )->getBeginAddress();
        
        holeSize = ( beg- end ) +1;
        
        if( holeSize >= size ) 
        {
            pos =beg;
            novo = new Partition( pos,size,false );
             part->insert( novo );
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
        
        part->insert( novo );
         
        return novo;
        
    }
    
    /*
     * No caso do next fit, não procuramos ainda nas particoes antes do lastIndex, então vamos procurar agora
     */
     index = part->begin();
     
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
            
            part->insert( novo );
            
            lastIndex= i;
            
            return novo;
        }
    }
    
    if ( pos==-1 ) return 0;
    
    novo = new Partition( pos,size,false );
    
    part->insert( novo );
    
    return novo;
}



/**
 * @see _WorstFit::allocateMemory( unsigned int ) member class declaration.
 */
Partition* _WorstFit::allocateMemory( unsigned int size ) 
{
    Partition* novo;
    
    DEBUGGERLN( 2, "I AM ENTERING IN _WorstFit::allocateMemory(1)" );
    
    auto part = getPartitions();
    
    if( part->size() ==0 ) 
    {
        novo = new Partition( 0, size-1 ,false );
        
        part->insert( novo );
        
        return novo;
    }
    
    auto index = part->begin();
    
    int partSize = 0 + ( *index )->getBeginAddress();
    int pos = -1;
    
    if( partSize >= size ) pos =0;
    
    int holeSize,end,beg =0;
    
    for( int i=0; i< part->size() -1;i++ ) 
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
    
    part->insert( novo );
    
    return novo;
}



/**
 * @see _BestFit::allocateMemory( unsigned int ) member class declaration.
 */
Partition* _BestFit::allocateMemory( unsigned int size ) 
{
    Partition* novo;
    
    DEBUGGERLN( 2, "I AM ENTERING IN _BestFit::allocateMemory(1)" );
    
    auto part = getPartitions();
    
    DEBUGGERLN( 32, "( allocateMemory ) size: %d", size );
    DEBUGGERLN( 32, "( allocateMemory ) part->size(): %d", part->size() );
    
    if( part->size() == 0 ) 
    {
        novo = new Partition( 0, size - 1, false );
        
        part->insert( novo );
        
        return novo;
    }
    
    int  pos      = -1;
    auto index    = part->begin();
    int  partSize = 0 + ( *index )->getBeginAddress();
    
    if( partSize >= size )
    {
        pos =0;
    }
    
    int holeSize;
    int end;
    
    int beg = 0;
    
    for( int i = 0; i< part->size() -1; ++i ) 
    {
        end = ( *index )->getEndAddress();
        
        index++;
        
        beg = ( *index )->getBeginAddress();
        
        holeSize = ( beg- end ) +1;
        
        if( holeSize >= size ) 
        {
            if( holeSize < partSize ) 
            {
                pos = beg;
                partSize = holeSize;
            }
        }
    }
    
    beg      = memoryManager->maxAddress;
    end      = ( *index )->getBeginAddress();
    holeSize = ( beg- end ) +1;
    
    if( holeSize >= size ) 
    {
        if( holeSize < partSize ) 
        {
            pos      = beg;
            partSize = holeSize;
        }
    }
    
    if( pos==-1 )
    {
        return 0;
    }
    
    novo = new Partition( pos,size -1,false );
    
    part->insert( novo );
    
    return novo;
}
















