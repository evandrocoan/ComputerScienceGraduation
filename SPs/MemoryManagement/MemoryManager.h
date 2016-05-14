/**
 * File:   MemoryManager.cpp
 * 
 * Authors: Wagner Fernando Gascho (12100779),
 *          Evandro  Coan ()
 * 
 * Created on 30/04/2016
 */



#ifndef MEMORYMANAGER_H
#define	MEMORYMANAGER_H


#include <list>
#include <limits.h>



/**
 * 
 */
class Partition
{
public:
    
    /**
     * To creates an partition object, which can be initially allocated or free to be used.
     * The partition system uses "unsigned int" (starting at 0) values to allocate/create
     * partitions.
     * 
     * @param beginAddress          the partition beginning address.
     * @param endAddress            the partition ending address.
     * @param isFree                whether this partition is free (true) or filled (false).
     */
    Partition( unsigned int beginAddress, unsigned int endAddress, bool isFree );
    
    /**
     * 
     */
    unsigned int getBeginAddress() const; // The implicit "this" pointer is const-qualified
    
    /**
     * 
     */
    unsigned int getEndAddress() const; // The implicit "this" pointer is const-qualified
    
    /**
     * 
     */
    unsigned int getLength() const; // The implicit "this" pointer is const-qualified
    
    /**
     * Implements the < "less than" operator to this new Partition type.
     */
    bool operator<( const Partition& p ) const; // The implicit "this" pointer is const-qualified
    
    /**
     * Implements the == "equal" operator to this new Partition type.
     */
    bool operator==( const Partition& p ) const; // The implicit "this" pointer is const-qualified
    
    
private:
    
    /**
     * 
     */
    unsigned int _beginAddress;
    
    /**
     * 
     */
    unsigned int _endAddress;
    
    /**
     * 
     */
    bool _isFree;
    
    
};



/**
 * 
 */
class Algorithm;

/**
 * 
 */
typedef std::list< Partition > PartitionList;

/**
 * 
 */
enum MemoryAllocationAlgorithm {FirstFit, NextFit, BestFit, WorstFit};



/**
 * 
 */
class MemoryManager
{
public:
    
    /**
     * 
     */
    friend Algorithm;
    
    /**
     * 
     */
    PartitionList partitions;
    
    /**
     * 
     */
    const unsigned int maxAddress = UINT_MAX;
    
    /**
     * 
     * @param 
     */
    MemoryManager( MemoryAllocationAlgorithm algorithm );
    
    /**
     * 
     * @param 
     */
    MemoryManager( const MemoryManager& orig );
    
    /**
     * 
     */
    virtual ~MemoryManager();
    
    /**
     * 
     * @param 
     */
    Partition* allocateMemory( unsigned int size );
    
    /**
     * 
     * @param 
     */
    void deallocateMemory( Partition* partition );
    
    /**
     * 
     */
    void showMemory();
    
    /**
     * 
     */
    unsigned int getNumPartitions();
    
    /**
     * 
     * @param 
     */
    Partition* getPartition( unsigned int index );
    
    
private:
    
    /**
     * 
     */
    Algorithm* functions;
    
    /**
     * 
     */
    MemoryAllocationAlgorithm algorithm;
    
};



/**
 * A cumbersome high tight strategy pattern implementation. This class defines
 * a interface to be extended and implemented the algorithms you need.
 */
struct Algorithm
{
    /**
     * Creates the memory allocation algorithm cumbersome high tight strategy
     * object.
     * 
     * @param memoryManager    an MemoryManager class object
     */
    Algorithm( MemoryManager* memoryManager );
    
    /**
     * Safely destroy clean the allocated memory by this class.
     */
    ~Algorithm();
    
    /**
     * 
     * @param 
     */
    virtual Partition* allocateMemory( unsigned int size ) = 0;
    
    
protected:
    
    /**
     * 
     */
    MemoryManager* memoryManager;
    
};



/**
 * 
 */
struct _FirstFit: public Algorithm
{
    /**
     * 
     */
    using::Algorithm::Algorithm;
    
    /**
     * 
     * @param 
     */
    virtual Partition* allocateMemory( unsigned int size );

};



/**
 * 
 */
struct _NextFit: public Algorithm
{
    /**
     * 
     */
    using::Algorithm::Algorithm;
    
    /**
     * 
     * @param 
     */
    virtual Partition* allocateMemory( unsigned int size );
    
private:
    
    /**
     * 
     */
    int lastIndex=0;
 
};



/**
 * 
 */
struct _WorstFit: public Algorithm
{
    /**
     * 
     */
    using::Algorithm::Algorithm;
    
    /**
     * 
     * @param 
     */
    virtual Partition* allocateMemory( unsigned int size );
     
     
};



/**
 *
 */
struct _BestFit: public Algorithm
{
    /**
     * 
     */
    using::Algorithm::Algorithm;
    
    /**
     * 
     * @param 
     */
    virtual Partition* allocateMemory( unsigned int size );
    
    
};



#endif	/* MEMORYMANAGER_H */

