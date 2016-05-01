/*
 * File:   MemoryManager.h
 * Authors: INSERT YOUR NAME HERE
 * Strategy: INSERT THE MEMORY MANAGEMENT SCHEMA HERE (THE ALGORITHM YOU ARE IMPLEMENTING -- RANDOMLY GENERATED BY MOODLE AS A VARIATION)
 *
 * Created on ...
 */

#ifndef MEMORYMANAGER_H
#define	MEMORYMANAGER_H

class Partition {
public:

    Partition(unsigned int beginAddress, unsigned int endAddress, bool isFree) {
        _beginAddress = beginAddress;
        _endAddress = endAddress;
        _isFree = isFree;
    }
public: // do not change

    unsigned int getBeginAddress() const {
        return _beginAddress;
    }

    unsigned int getEndAddress() const {
        return _endAddress;
    }

    unsigned int getLength() const {
        return _endAddress - _beginAddress + 1;
    }
private: // do not change
    unsigned int _beginAddress;
    unsigned int _endAddress;
    bool _isFree;
private:
    // INSERT YOUR CODE HERE
    // ...
};

enum MemoryAllocationAlgorithm {FirstFit, NextFit, BestFit, WorstFit};

class MemoryManager {
public: // do not change
    MemoryManager(MemoryAllocationAlgorithm algorithm);
    MemoryManager(const MemoryManager& orig);
    virtual ~MemoryManager();
public: // do not change
    Partition* allocateMemory(unsigned int size);
    void deallocateMemory(Partition* partition);
    void showMemory();
    unsigned int getNumPartitions();
    Partition* getPartition(unsigned int index);
private: // private attributes and methods
    // INSERT YOUR CODE HERE
    // ...
};

#endif	/* MEMORYMANAGER_H */

