// EPOS Cortex_M Memory Map

#ifndef __cortex_m_memory_map_h
#define __cortex_m_memory_map_h

#include <system/memory_map.h>

__BEGIN_SYS

template <>
struct Memory_Map<Cortex_M>
{
    // Physical Memory
    enum {
        MEM_BASE        = Traits<Cortex_M>::MEM_BASE,
        MEM_TOP         = Traits<Cortex_M>::MEM_TOP
    };

    // Logical Address Space
    enum {
        APP_LOW         = Traits<Cortex_M>::APP_LOW,
        APP_CODE        = Traits<Cortex_M>::APP_CODE,
        APP_DATA        = Traits<Cortex_M>::APP_DATA,
        APP_HIGH        = Traits<Cortex_M>::APP_HIGH,

        PHY_MEM         = Traits<Cortex_M>::PHY_MEM,
        IO              = Traits<Cortex_M>::IO_BASE,

        SYS             = Traits<Cortex_M>::SYS,
        SYS_INFO        = unsigned(-1),                 // Not used during boot. Dynamically built during initialization.
        SYS_CODE        = Traits<Cortex_M>::SYS_CODE,
        SYS_DATA        = Traits<Cortex_M>::SYS_DATA,
        SYS_HEAP        = SYS_DATA,                     // Not used (because multiheap can only be enabled with an MMU)
        SYS_STACK       = MEM_TOP + 1 - Traits<Cortex_M>::STACK_SIZE      // This stack is used before main(). The stack pointer is initialized at crt0.S
    };
};

/*
template <>
struct IO_Map<Cortex_M>
{
    enum {
        ITC_BASE                = 0x80020000,
        ITC_NIPEND              = ITC_BASE + 0x38,
    };
};
*/

__END_SYS

#endif
