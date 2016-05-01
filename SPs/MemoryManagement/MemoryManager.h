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

#include <set>



/**
 * 
 */
class Partition
{
public:
    
    /**
     * 
     * @param 
     * @param 
     * @param
     */
    Partition(unsigned int beginAddress, unsigned int endAddress, bool isFree);
    
    
public: // do not change
    
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
    
    
private: // do not change
    
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
    
    
private:
    // INSERT YOUR CODE HERE
    // ...
    
public:
    
    /**
     * 
     */
    bool operator<( const Partition& p ) const; // The implicit "this" pointer is const-qualified
};


/**
 * 
 */
typedef std::set<Partition*, bool (*)(Partition*,Partition*)> PartitionList;

/**
 * 
 */
enum MemoryAllocationAlgorithm {FirstFit, NextFit, BestFit, WorstFit};


/**
 * 
 */
class Algorithm;



/**
 * 
 */
class MemoryManager
{
public: // do not change
    
    /**
     * 
     * @param 
     */
    MemoryManager(MemoryAllocationAlgorithm algorithm);
    
    /**
     * 
     * @param 
     */
    MemoryManager(const MemoryManager& orig);
    
    /**
     * 
     */
    virtual ~MemoryManager();
    
    
public: // do not change
    
    /**
     * 
     * @param 
     */
    Partition* allocateMemory(unsigned int size);
    
    /**
     * 
     * @param 
     */
    void deallocateMemory(Partition* partition);
    
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
    Partition* getPartition(unsigned int index);
    
    ///extensão de classe na marra
    
    
protected: // private attributes and methods
    
    /**
     * 
     */
    PartitionList partitions;
    
    /**
     * 
     */
    Algorithm* functions;
    
    /**
     * 
     */
    MemoryAllocationAlgorithm algorithm;
    
    /**
     * 
     */
    friend Algorithm;
    
    
public:
    
    /**
     * 
     */
    const unsigned int maxAddress=0;
    
    
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
    Algorithm(MemoryManager* memoryManager);
    
    /**
     * Safely destroy clean the allocated memory by this class.
     */
    ~Algorithm();
    
    /**
     * 
     */
    PartitionList* getPartitions();
    
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
    virtual Partition* allocateMemory(unsigned int size);

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
    virtual Partition* allocateMemory(unsigned int size);
    
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
    virtual Partition* allocateMemory(unsigned int size);
     
     
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
    virtual Partition* allocateMemory(unsigned int size);
    
    
};



#endif	/* MEMORYMANAGER_H */

