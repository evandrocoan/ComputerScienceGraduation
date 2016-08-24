// EPOS EPOSMoteIII (Cortex-M3) MCU Metainfo and Configuration

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
    static const unsigned int MEM_BASE  = 0x20000004;
    static const unsigned int MEM_TOP   = 0x20007ff7; // 32 KB (MAX for 32-bit is 0x70000000 / 1792 MB)

    // Logical Memory Map
    static const unsigned int APP_LOW   = 0x20000004;
    static const unsigned int APP_CODE  = 0x00204000;
    static const unsigned int APP_DATA  = 0x20000004;
    static const unsigned int APP_HIGH  = 0x20007ff7;

    static const unsigned int PHY_MEM   = 0x20000004;
    static const unsigned int IO_BASE   = 0x40000000;
    static const unsigned int IO_TOP    = 0x440067ff;

    static const unsigned int SYS       = 0x00204000;
    static const unsigned int SYS_CODE  = 0x00204000; // Library mode only => APP + SYS
    static const unsigned int SYS_DATA  = 0x20000004; // Library mode only => APP + SYS

    // Default Sizes and Quantities
    static const unsigned int STACK_SIZE = 3 * 1024;
    static const unsigned int HEAP_SIZE = 3 * 1024;
    static const unsigned int MAX_THREADS = 7;
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
    // Some observed objects are created before initializing the Display, which may use the USB.
    // Enabling debug may cause trouble in some Machines
    static const bool debugged = false;
    
    static const unsigned int UNITS = 1;
    static const bool blocking = false;
    static const bool enabled = Traits<Serial_Display>::enabled && (Traits<Serial_Display>::ENGINE == Traits<Serial_Display>::USB);
};

template <> struct Traits<Cortex_M_Scratchpad>: public Traits<Cortex_M_Common>
{
    static const bool enabled = false;
};

template <> struct Traits<Cortex_M_IEEE802_15_4>: public Traits<Cortex_M_Common>
{
    static const bool enabled = (Traits<Build>::NODES > 1);

    typedef TSTP_MAC MAC;

    typedef LIST<eMote3_TSTP_MAC> NICS;

    static const unsigned int UNITS = NICS::Length;
};

template <> struct Traits<eMote3_IEEE802_15_4>: public Traits<Cortex_M_IEEE802_15_4>
{
    static const unsigned int UNITS = NICS::Count<eMote3_IEEE802_15_4>::Result;
    static const unsigned int SEND_BUFFERS = 24;
    static const unsigned int RECEIVE_BUFFERS = 8;
    static const unsigned int DEFAULT_CHANNEL = 15; // From 11 to 26

    // There is no listen command on the radio interface yet,
    // so the only way to receive data is setting this flag
    static const bool auto_listen = true;

    static const bool CSMA_CA = true;
    static const unsigned int CSMA_CA_MIN_BACKOFF_EXPONENT = 3;
    static const unsigned int CSMA_CA_MAX_BACKOFF_EXPONENT = 5;
    static const unsigned int CSMA_CA_UNIT_BACKOFF_PERIOD = 320; // us
    static const unsigned int CSMA_CA_MAX_TRANSMISSION_TRIALS = 4;

    static const bool ACK = true;
    static const unsigned int RETRANSMISSIONS = 3;
    static const unsigned int ACK_TIMEOUT = 3 * 832; // us

    static const bool CRC = false;
};

template <> struct Traits<eMote3_TSTP_MAC>: public Traits<Cortex_M_IEEE802_15_4>
{
    static const unsigned int UNITS = NICS::Count<eMote3_TSTP_MAC>::Result;
    static const unsigned int SEND_BUFFERS = 8;
    static const unsigned int RECEIVE_BUFFERS = 8;
    static const unsigned int DEFAULT_CHANNEL = 15; // From 11 to 26
};

__END_SYS

#endif
