#include "Traits.h"
#include "MemoryManager.h"



/**
 * @see Algorithm::Algorithm( MemoryManager* ) member class declaration.
 */
Algorithm::Algorithm() : partitionList(), lastIteratorAccess()
{
    DEBUGGERLN( a2, "I AM ENTERING IN Algorithm::Algorithm(0) THE CONSTRUCTOR!" );
    lastIndexAccess = DISABLED_LAST_PARTITION_INDEX;
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
void Algorithm::addPartition( Partition* newPartition )
{
    DEBUGGERLN( a2, "I AM ENTERING IN Algorithm::addPartition(1)" );
    
    this->lastIndexAccess = DISABLED_LAST_PARTITION_INDEX;
    this->partitionList.push_back( *newPartition );
}

/**
 * @see Algorithm::getPartition( unsigned int index ) member class declaration.
 */
Partition* Algorithm::getPartition( unsigned int index )
{
    DEBUGGERLN( a2 a32, "I AM ENTERING IN Algorithm::getPartition(1) | index: %d.", index );
    DEBUGGERLN( a32, "I AM ENTERING IN Algorithm::getPartition(1) | this->partitionListSize(): %d, \nthis->lastIndexAccess: %d,",
                                                                    this->partitionListSize(),       this->lastIndexAccess );
    
    if( -1 < index < this->partitionListSize() )
    {
        if( index == this->lastIndexAccess + 1 )
        {
            DEBUGGERLN( a32, "( getPartition ) exiting by index == + 1" );
            DEBUGGERLN( a32, "( getPartition ) what is '( *this->lastIteratorAccess ).getEndAddress()'? %d,",
                                                        ( *this->lastIteratorAccess ).getEndAddress() );
            this->lastIteratorAccess = ++( this->lastIteratorAccess );
        }
        else if( index == this->lastIndexAccess )
        {
            DEBUGGERLN( a32, "( getPartition ) exiting by index ==" );
            goto exit;
        }
        else if( index == this->lastIndexAccess - 1 )
        {
            DEBUGGERLN( a32, "( getPartition ) exiting by index == -1" );
            DEBUGGERLN( a32, "( getPartition ) could I increment 'this->lastIteratorAccess'? %d,", ( *this->lastIteratorAccess ).getEndAddress() );
            this->lastIteratorAccess = --( this->lastIteratorAccess );
        }
        else
        {
            DEBUGGERLN( a32, "( getPartition ) exiting by index == none" );
            this->lastIteratorAccess = std::next( this->partitionList.begin(), index );
        }
        
        this->lastIndexAccess = index;
    }
    else
    {
        DEBUGGERLN( a32, "( getPartition ) returned NULL;" );
        return NULL;
    }
    
    exit:
    DEBUGGERLN( a32, "( getPartition ) this->lastIteratorAccess: %d", this->lastIteratorAccess );
    DEBUGGERLN( a32, "( getPartition ) this->lastIteratorAccess == this->partitionList.end(): %d",
                                      this->lastIteratorAccess == this->partitionList.end() );
    
    DEBUGGERLN( a32, "( getPartition ) exiting by return &( *( this->lastIteratorAccess ) );" );
    return &( *( this->lastIteratorAccess ) );
}

/**
 * @see Algorithm::deletePartition( Partition* currentPartition ) member class declaration.
 */
void Algorithm::deletePartition( Partition* partition )
{
    DEBUGGERLN( a2, "I AM ENTERING IN Algorithm::deletePartition(1)" );
    
    this->lastIndexAccess = DISABLED_LAST_PARTITION_INDEX;
    this->partitionList.erase( std::find( this->partitionList.begin(), this->partitionList.end(), *partition ) );
}

/**
 * @see Algorithm::partitionListSize() member class declaration.
 */
unsigned int Algorithm::partitionListSize()
{
    DEBUGGERLN( a2, "I AM ENTERING IN Algorithm::partitionListSize(0)" );
    return this->partitionList.size();
}



/**
 * @see _BestFit::allocateMemory( unsigned int ) member class declaration.
 */
Partition* _BestFit::allocateMemory( unsigned int size ) 
{
    DEBUGGERLN( b2 + a2, "\nI AM ENTERING IN _NextFit::allocateMemory(1)" );
    DEBUGGERLN( a2 b1, "\nI AM ENTERING IN _FirstFit::allocateMemory(1)" );
    
    Partition* novo = NULL;
    DEBUGGERLN( b1, "( allocateMemory ) size: %d, this->partitionList.size(): %d,", size, this->partitionList.size() );
    
    if( this->partitionList.size() == 0 )
    {
        novo = new Partition( 0, size - 1, false );
        this->addPartition( novo );
        
        DEBUGGERLN( b1, "( allocateMemory|size 0 ) novo->getBeginAddress(): %d, \nnovo->getEndAddress(): %d,",
                                                novo->getBeginAddress(),       novo->getEndAddress() );
        
        return novo;
    }
    
    unsigned int holeSize;
    unsigned int partitionIndex;
    unsigned int partitionEndAddress;
    
    Partition* currentPartition = NULL;
    Partition* nextPartition    = NULL;
    
    unsigned int  partitionsListSize  = this->partitionList.size();
    DEBUGGERLN( b1, "( allocateMemory ) partitionsListSize: %d,", partitionsListSize );
    
    currentPartition = this->getPartition( 0 );
    
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
    
    DEBUGGERLN( b1, "( allocateMemory|after for 1 )" );
    partitionEndAddress = currentPartition->getEndAddress() + size;
    DEBUGGERLN( b1, "( allocateMemory|after for 2 )" );
    
    if( partitionEndAddress < MemoryManager::maxAddress + 1 )
    {
        DEBUGGERLN( b1, "( allocateMemory|after for 3 ) currentPartition->getEndAddress(): %d, \npartitionEndAddress: %d", currentPartition->getEndAddress(), partitionEndAddress );
        
        novo = new Partition( currentPartition->getEndAddress() + 1, partitionEndAddress, false );
        this->partitionList.push_back( *novo );
        
        DEBUGGERLN( b1, "( allocateMemory ) novo->getBeginAddress(): %d, \nnovo->getEndAddress(): %d, \nnovo->getLength(): %d",
                                            novo->getBeginAddress(),       novo->getEndAddress(),       novo->getLength() );
    }
    
#if defined DEBUG
    partitionIndex = 0;
    
    for( auto partition : this->partitionList )
    {
        DEBUGGERLN( b1, "( allocateMemory|DEBUG ) partitionIndex: %d, \npartition.getBeginAddress(): %d, \npartition.getEndAddress(): %d, \npartition.getLength(): %d",
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

















