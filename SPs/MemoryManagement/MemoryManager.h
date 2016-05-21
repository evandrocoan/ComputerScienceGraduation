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
#include <iostream>
#include <algorithm>


/**
 * This is the value used when the the Algorithm Strategy Class need to initialize/reset the
 * last partition access index. Such approach is needed because when the partition list is
 * altered as adding or removing entries. If such reset is not performed, the last partition
 * index would point to and nonupdated value, then lead to segmentation fault when dereferencing
 * the iterator it is points to.
 */
#define DISABLED_LAST_PARTITION_INDEX -10



/**
 * Represents a dynamic continue allocation partition. Such allocation does not require great
 * resources and is able to use the concepts of comparison and less than.
 * 
 * @see Partition::operator<( const Partition& ) member class declaration.
 * @see Partition::operator==( const Partition& ) member class declaration.
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
     * Gets the partition begging address.
     * 
     * @return an unsigned int as the partition start address.
     */
    unsigned int getBeginAddress() const;
    
    /**
     * Gets the partition ending address.
     * 
     * @return an unsigned int as the partition end address.
     */
    unsigned int getEndAddress() const;
    
    /**
     * Gets the partitions total size/lenght.
     * 
     * @return an unsigned int as the partition length.
     */
    unsigned int getLength() const;
    
    /**
     * Implements the < "less than" operator to this new Partition type. A partition is considered
     * to be less than another when its start address is smaller.
     */
    bool operator<( const Partition& p ) const;
    
    /**
     * Implements the == "equal" operator to this new Partition type. A partition is considered to
     * be equal to another, when they have the same start address.
     */
    bool operator==( const Partition& p ) const;
    
    
private:
    
    /**
     * The partition begging address.
     */
    unsigned int _beginAddress;
    
    /**
     * The partiton ending address.
     */
    unsigned int _endAddress;
    
    /**
     * Whether this partition is free (true) or filled (false) when using it as a static allocation
     * strategy.
     */
    bool _isFree;
    
};



/**
 * An class prototyppe to allow the 'Algorithm' class to be implemented at the same file as other
 * class's which uses it, but to be refered only and only as a pointer declaration. As long as the
 * compiler requires it to be fully declared before to be instatiated.
 */
class Algorithm;

/**
 * A very quick way to refer to the partition internal representation within an alias.
 */
typedef std::list< Partition > PartitionList;

/**
 * Defines all allocations algorithm strategy this partitionning system supports.
 */
enum MemoryAllocationAlgorithm {FirstFit, NextFit, BestFit, WorstFit};



/**
 * Controls the memory allocations to this system. This used the delegation design pattern which
 * uses the 'Algorithm' strategy class to switch the allocation algorithm strategy during run time.
 * The MemoryManager class is used as a controller to switch the proper algorithm to be used as the
 * allocation strategy.
 * 
 * @see ::MemoryAllocationAlgorithm enum for the allocations strategies availables.
 */
class MemoryManager
{
public:
    
    /**
     * Specifies the maximum allocable address suported as the maximum unsigned integer value.
     */
    static const unsigned int maxAddress;
    
    /**
     * Creates a new and clean memory schema given an allocation strategy.
     * 
     * @param algorithm          the allocation strategy type.
     * @see ::MemoryAllocationAlgorithm enum for the allocations strategies type.
     */
    MemoryManager( MemoryAllocationAlgorithm algorithm );
    
    /**
     * To create a new MemoryManager object based on an existing MemoryManager Object. This new
     * object created, keep all allocations as the existent given MemoryManager Object.
     * 
     * @param source          an existing and properly initialized MemoryManager object.
     */
    MemoryManager( const MemoryManager& source );
    
    /**
     * Free the heap dynamic allocated memory on object destruction.
     */
    virtual ~MemoryManager();
    
    /**
     * Delegates the memory allocation to the child class Algorithm.
     * 
     * @see Algorithm::allocateMemory( unsigned int size ) for the deallocation details.
     */
    Partition* allocateMemory( unsigned int size );
    
    /**
     * Delegates the memory deallocation to the child class Algorithm.
     * 
     * @see Algorithm::deletePartition( Partition* currentPartition ) for the deallocation details.
     */
    void deallocateMemory( Partition* partition );
    
    /**
     * TO SHOW THE MEMORY MAP to the standart output, IN THE FOLLOWING FORMAT
     * <beginAddress>-<endAddress>: <FREE|ALLOCATED> <size>
     * Example:
     * 
     * 0-1499:FREE 1500
     * 1500-1999:ALLOCATED 500
     * 2000-2999:ALLOCATED 1000
     * 3000-9999:FREE 7000
     * 10000-19999:ALLOCATED 10000
     * 20000-1000000:FREE 800000
     */
    void showMemory();
    
    /**
     * Delegates the partitions number return to the child class Algorithm.
     * 
     * @see Algorithm::getPartition( unsigned int index ) for the deallocation details.
     */
    unsigned int getNumPartitions();
    
    /**
     * Delegates the get partition to the child class Algorithm.
     * 
     * @see Algorithm::getPartition( unsigned int index ) for the deallocation details.
     */
    Partition* getPartition( unsigned int index );
    
    
private:
    
    /**
     * Holds the construction time chosen allocation strategy.
     * 
     * @see ::MemoryAllocationAlgorithm enum for the allocations strategies.
     */
    Algorithm* currentStrategy;
    
};



/**
 * A strategy pattern implementation. This class defines a interface to be extended and implemented the
 * algorithms you need to implement the memory allocation technique.
 */
struct Algorithm
{
    /**
     * Creates the memory allocation algorithm within the strategy design pattern.
     */
    Algorithm();
    
    /**
     * Safely destroy clean the allocated memory by this class.
     */
    ~Algorithm();
    
    /**
     * Allocates/creates a new memory partition. This is a virtual method and must to implemented
     * by its inheriting class.
     * 
     * @param size         an unsigned int indicating the partition size.
     * @return a pointer to the zero value when the allocation fails, otherwise the allocated Partition
     *         type pointer.
     */
    virtual Partition* allocateMemory( unsigned int size ) = 0;
    
    /**
     * Removes the specified partition from the allocated list, freeing the allocated memory.
     * 
     * @param partition        a pointer to the Partition type.
     */
    void deletePartition( Partition* partition );
    
    /**
     * Gets the current partition list size.
     * 
     * @return an unsigned int representing the partition size.
     */
    unsigned int partitionListSize();
    
    /**
     * Obtains an partition from the partition list. This caches the last returned partition, then
     * it provide fast access to the partition before, last accessed and next partition to the index
     * provided during the last access to this function.
     * 
     * @param index         the ith element on the partition list.
     * @return a pointer to the zero value when the allocation fails, otherwise the allocated Partition
     *         type pointer.
     */
    Partition* getPartition( unsigned int index );
    
    
protected:
    
    /**
     * Holds all partitions allocated by this system. The storing approach is to list/save every allocated
     * partition as allocated partitionnig list.
     * 
     * @see ::std::list< Partition > PartitionList type definition.
     */
    PartitionList partitionList;
    
    /**
     * Used to save the last 'getPartition(1)' access, to know where the 'lastIteratorAccess' is pointing to.
     */
    unsigned int lastIndexAccess;
    
    /**
     * Its purpose is to speed up sequencial acess to the partitions using the 'getPartition(1)' funciton call.
     * Used to save the last 'getPartition(1)' access, to cache its value and void to walk throw the linked list
     * from the begging.
     */
    std::list< Partition >::iterator lastIteratorAccess;
    
    /**
     * Add a new partition to the partitition list.
     * 
     * @param newPartition        a pointer to the Partition type.
     * @see Algorithm::partitionList attribute for the partition list.
     */
    void addPartition( Partition* newPartition );
};



/**
 * Implements the abstract class (struct) Algorithm to allocate memory using the First Fit memory allocation
 * strategy.
 */
struct _FirstFit: public Algorithm
{
    /**
     * Inherrits the superclass constructor.
     */
    using::Algorithm::Algorithm;
    
    /**
     * To creates/allocates a new partition by the First Fit allocation strategy.
     * 
     * @param size        the new partition size to create.
     * @return NULL when the allocation fails, otherwise a pointer to the new partition just created.
     */
    virtual Partition* allocateMemory( unsigned int size ) override;

};



/**
 * Implements the abstract class (struct) Algorithm to allocate memory using the Next Fit memory allocation
 * strategy.
 */
struct _NextFit: public Algorithm
{
    /**
     * Inherrits the superclass constructor.
     */
    using::Algorithm::Algorithm;
    
    /**
     * To creates/allocates a new partition by the Next Fit allocation strategy.
     * 
     * @param size        the new partition size to create.
     * @return NULL when the allocation fails, otherwise a pointer to the new partition just created.
     */
    virtual Partition* allocateMemory( unsigned int size ) override;
    

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
    virtual Partition* allocateMemory( unsigned int size ) override;
     
     
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
    virtual Partition* allocateMemory( unsigned int size ) override;
    
    
private:
    
    /**
     * Used to save the last access, i.e., to know where the 'lastAllocationIterator' is pointing to.
     */
    int g_lastAllocationIndex = DISABLED_LAST_PARTITION_INDEX;
    
    
};



#endif	/* MEMORYMANAGER_H */

