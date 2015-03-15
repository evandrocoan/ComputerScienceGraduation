#ifndef __process_h
#define __process_h

#include "config.h"

class Process
{
public:
    /**
     * @param pid process id
     * @param ppid ?
     * @param *code_addr pointer to a address memory range to allocate the
     * program itself
     * @param *globaldata_addr point to a address memory range to allocate
     * data
     */
    Process( ProcessId pid, ProcessId ppid, AddressRegion *code_addr,
             AddressRegion *globaldata_addr ) :
            _pid( pid ), _ppid( ppid ), _code_addr( code_addr ),
            _globaldata_addr( globaldata_addr )
    {
    }
public:
    void setPID( ProcessId pid )
    {
        _pid = pid;
    }
    void setPPID( ProcessId ppid )
    {
        _ppid = ppid;
    }
    void setCodeAddressRegion( const Address begin, const Address end )
    {
        _code_addr = new AddressRegion( begin, end );
    }
    void setGlobalDataAddressRegion( const Address begin, const Address end )
    {
        _globaldata_addr = new AddressRegion( begin, end );
    }
    ProcessId getPID()
    {
        return _pid;
    }
    ProcessId getPPID()
    {
        return _ppid;
    }
    AddressRegion* getCodeAddressRegion()
    {
        return _code_addr;
    }
    AddressRegion* getGlobalDataAddressRegion()
    {
        return _globaldata_addr;
    }
private:
    ProcessId _pid;
    ProcessId _ppid;
    AddressRegion *_code_addr, *_globaldata_addr;
};

#endif
