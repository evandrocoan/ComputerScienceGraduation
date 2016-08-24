// EPOS LM3S811 (Cortex-M3) MCU Metainfo and Configuration

#ifndef __machine_traits_h
#define __machine_traits_h

#include <system/config.h>

__BEGIN_SYS

class Cortex_M_Common;
template <> struct Traits<Cortex_M_Common>: public Traits<void>
{
    static const bool debugged = Traits<void>::debugged;
};

template <> struct Traits<Cortex_M>: public Traits<Cortex_M_Common>
{
    static const unsigned int CPUS = Traits<Build>::CPUS;

    // Physical Memory
    static const unsigned int MEM_BASE  = 0x20000000;
    static const unsigned int MEM_TOP   = 0x20001fff; // 8 KB (MAX for 32-bit is 0x70000000 / 1792 MB)

    // Logical Memory Map
    static const unsigned int APP_LOW   = 0x20000000;
    static const unsigned int APP_CODE  = 0x00000000;
    static const unsigned int APP_DATA  = 0x20000000;
    static const unsigned int APP_HIGH  = 0x20001fff; // 8 KB

    static const unsigned int PHY_MEM   = 0x20000000;
    static const unsigned int IO_BASE   = 0x40000000;
    static const unsigned int IO_TOP    = 0x440067ff;

    static const unsigned int SYS       = 0x00200000;
    static const unsigned int SYS_CODE  = 0x00200000; // Library mode only => APP + SYS
    static const unsigned int SYS_DATA  = 0x20000000; // Library mode only => APP + SYS

    // Default Sizes and Quantities
    static const unsigned int STACK_SIZE = 512;
    static const unsigned int HEAP_SIZE = 512;
    static const unsigned int MAX_THREADS = 5;
};

template <> struct Traits<Cortex_M_IC>: public Traits<Cortex_M_Common>
{
    static const bool debugged = hysterically_debugged;
};

template <> struct Traits<Cortex_M_Timer>: public Traits<Cortex_M_Common>
{
    static const bool debugged = hysterically_debugged;

    // Meaningful values for the timer frequency range from 100 to
    // 10000 Hz. The choice must respect the scheduler time-slice, i. e.,
    // it must be higher than the scheduler invocation frequency.
    static const int FREQUENCY = 1000; // Hz
};

template <> struct Traits<Cortex_M_UART>: public Traits<Cortex_M_Common>
{
    static const unsigned int UNITS = 2;

    static const unsigned int CLOCK = Traits<ARMv7>::CLOCK;

    static const unsigned int DEF_BAUD_RATE = 115200;
    static const unsigned int DEF_DATA_BITS = 8;
    static const unsigned int DEF_PARITY = 0; // none
    static const unsigned int DEF_STOP_BITS = 1;
};

template <> struct Traits<Cortex_M_USB>: public Traits<Cortex_M_Common>
{
    static const bool enabled = false;
    static const unsigned int UNITS = 0;
    static const bool blocking = true;
};

template <> struct Traits<Cortex_M_Scratchpad>: public Traits<Cortex_M_Common>
{
    static const bool enabled = false;
};

template <> struct Traits<Cortex_M_IEEE802_15_4>: public Traits<Cortex_M_Common>
{
    static const bool enabled = (Traits<Build>::NODES > 1);

    typedef LIST<CC2538> NICS;
    static const unsigned int UNITS = NICS::Length;
};

template <> struct Traits<CC2538>: public Traits<Cortex_M_IEEE802_15_4>
{
    static const unsigned int UNITS = NICS::Count<CC2538>::Result;
    static const unsigned int SEND_BUFFERS = 64; // per unit
    static const unsigned int RECEIVE_BUFFERS = 256; // per unit
};

__END_SYS

#endif
