// EPOS Internal Type Management System

typedef __SIZE_TYPE__ size_t;

#ifndef __types_h
#define __types_h

// Memory allocators
__BEGIN_API
enum System_Allocator { SYSTEM };
enum Scratchpad_Allocator { SCRATCHPAD };
__END_API

extern "C"
{
    void * malloc(size_t);
    void free(void *);
}

inline void * operator new(size_t s, void * a) { return a; }
inline void * operator new[](size_t s, void * a) { return a; }

void * operator new(size_t, const EPOS::System_Allocator &);
void * operator new[](size_t, const EPOS::System_Allocator &);

void * operator new(size_t, const EPOS::Scratchpad_Allocator &);
void * operator new[](size_t, const EPOS::Scratchpad_Allocator &);

// Utilities
__BEGIN_UTIL
class Dummy;
class Bitmaps;
class CRC;
class ELF;
class Handler;
class Hashes;
class Heaps;
class Debug;
class Lists;
class Observers;
class Observeds;
class OStream;
class Queues;
class Random;
class Spin;
class SREC;
class Vectors;
__END_UTIL

__BEGIN_SYS

// System parts
class Build;
class Boot;
class Setup;
class Init;
class Utility;

// Architecture Hardware Mediators
class IA32;
class IA32_TSC;
class IA32_MMU;
class IA32_FPU;
class IA32_PMU;

class ARMv7;
class ARMv7_TSC;
class ARMv7_MMU;

class AVR8;
class AVR8_TSC;
class AVR8_MMU;

// Machine Hardware Mediators
class PC;
class PC_PCI;
class PC_IC;
class PC_Timer;
class PC_RTC;
class PC_UART;
class PC_EEPROM;
class PC_Display;
class PC_Keyboard;
class PC_Scratchpad;
class PC_NIC;
class PC_Ethernet;
class PC_FPGA;

class Cortex_M;
class Cortex_M_IC;
class Cortex_M_Timer;
class Cortex_M_RTC;
class Cortex_M_UART;
class Cortex_M_USB;
class Cortex_M_GPIO;
class Cortex_M_EEPROM;
class Cortex_M_Display;
class Cortex_M_Scratchpad;
class Cortex_M_NIC;
class Cortex_M_IEEE802_15_4;
class Cortex_M_I2C;

class ATmega;
class ATmega_IC;
class ATmega_Timer;
class ATmega_RTC;
class ATmega_EEPROM;
class ATmega_Flash;
class ATmega_Scratchpad;
class ATmega_UART;
class ATmega_USART;
class ATmega_SPI;
class ATmega_ADC;
class ATmega_Battery;
class ATmega_Radio;

class PCNet32;
class C905;
class E100;
class eMote3_IEEE802_15_4_PHY;
class eMote3_IEEE802_15_4;
class eMote3_TSTP_MAC;
class IEEE802_15_4;
class AT86RF;

class Serial_Display;
class Serial_Keyboard;

template <unsigned int KEY_LENGTH> 
class AES;

// Abstractions
class System;
class Application;

class Thread;
class Active;
class Periodic_Thread;
class RT_Thread;
class Task;

template<typename> class Scheduler;
namespace Scheduling_Criteria
{
    class Priority;
    class FCFS;
    class RR;
    class RM;
    class DM;
    class EDF;
    class GRR;
    class CPU_Affinity;
    class GEDF;
    class PEDF;
    class CEDF;
};

class Address_Space;
class Segment;

class Synchronizer;
class Mutex;
class Semaphore;
class Condition;

class Clock;
class Chronometer;
class Alarm;
class Delay;

class Network;

class ELP;

class TSTPOE;
class TSTPOTM;
class TSTP_MAC;
class TSTP_Timer;
class TSTP;
template<typename S>
class Smart_Data;

template<typename NIC, typename Network, unsigned int HTYPE>
class ARP;
class IP;
class ICMP;
class UDP;
class TCP;
class DHCP;

class IPC;

template<typename Channel, bool connectionless = Channel::connectionless>
class Link;
template<typename Channel, bool connectionless = Channel::connectionless>
class Port;

template<typename S>
class Smart_Data;

template <unsigned int SECRET_SIZE>
class Diffie_Hellman;
class Poly1305;

// Framework
class Framework;
template<typename Component> class Handle;
template<typename Component, bool remote> class Stub;
template<typename Component> class Proxy;
template<typename Component> class Adapter;
template<typename Component> class Scenario;
class Agent;

// Aspects
class Aspect;
template<typename Component> class Authenticated;
template<typename Component> class Shared;
template<typename Component> class Remote;

// System Components IDs
// The order in this enumeration defines many things in the system (e.g. init)
typedef unsigned int Type_Id;
enum
{
    CPU_ID = 100,
    TSC_ID,
    MMU_ID,
    FPU_ID,
    PMU_ID,

    MACHINE_ID = 200,
    PCI_ID,
    IC_ID,
    TIMER_ID,
    RTC_ID,
    EEPROM_ID,
    SCRATCHPAD_ID,
    UART_ID,
    DISPLAY_ID,
    KEYBOARD_ID,
    NIC_ID,

    THREAD_ID = 0,
    TASK_ID,
    ACTIVE_ID,

    ADDRESS_SPACE_ID,
    SEGMENT_ID,

    MUTEX_ID,
    SEMAPHORE_ID,
    CONDITION_ID,

    CLOCK_ID,
    ALARM_ID,
    CHRONOMETER_ID,

    IPC_COMMUNICATOR_ID,

    UTILITY_ID,

    LAST_TYPE_ID,

    UNKNOWN_TYPE_ID = 0xffff
};

// Type IDs for system components
template<typename T> struct Type { static const Type_Id ID = UNKNOWN_TYPE_ID; };

template<> struct Type<IA32> { static const Type_Id ID = CPU_ID; };
template<> struct Type<IA32_TSC> { static const Type_Id ID = TSC_ID; };
template<> struct Type<IA32_MMU> { static const Type_Id ID = MMU_ID; };
template<> struct Type<IA32_FPU> { static const Type_Id ID = FPU_ID; };
template<> struct Type<IA32_PMU> { static const Type_Id ID = PMU_ID; };

template<> struct Type<ARMv7> { static const Type_Id ID = CPU_ID; };
template<> struct Type<ARMv7_TSC> { static const Type_Id ID = TSC_ID; };
template<> struct Type<ARMv7_MMU> { static const Type_Id ID = MMU_ID; };


template<> struct Type<PC> { static const Type_Id ID = MACHINE_ID; };
template<> struct Type<PC_IC> { static const Type_Id ID = IC_ID; };
template<> struct Type<PC_Timer> { static const Type_Id ID = TIMER_ID; };
template<> struct Type<PC_UART> { static const Type_Id ID = UART_ID; };
template<> struct Type<PC_RTC> { static const Type_Id ID = RTC_ID; };
template<> struct Type<PC_PCI> { static const Type_Id ID = PCI_ID; };
template<> struct Type<PC_Display> { static const Type_Id ID = DISPLAY_ID; };
template<> struct Type<PC_Keyboard> { static const Type_Id ID = KEYBOARD_ID; };
template<> struct Type<PC_Scratchpad> { static const Type_Id ID = SCRATCHPAD_ID; };
template<> struct Type<PC_Ethernet> { static const Type_Id ID = NIC_ID; };

template<> struct Type<Cortex_M> { static const Type_Id ID = MACHINE_ID; };
template<> struct Type<Cortex_M_IC> { static const Type_Id ID = IC_ID; };
template<> struct Type<Cortex_M_Timer> { static const Type_Id ID = TIMER_ID; };
template<> struct Type<Cortex_M_UART> { static const Type_Id ID = UART_ID; };
template<> struct Type<Cortex_M_RTC> { static const Type_Id ID = RTC_ID; };
template<> struct Type<Cortex_M_Display> { static const Type_Id ID = DISPLAY_ID; };
template<> struct Type<Cortex_M_Scratchpad> { static const Type_Id ID = SCRATCHPAD_ID; };
template<> struct Type<Cortex_M_IEEE802_15_4> { static const Type_Id ID = NIC_ID; };


template<> struct Type<Thread> { static const Type_Id ID = THREAD_ID; };
template<> struct Type<Periodic_Thread> { static const Type_Id ID = THREAD_ID; };
template<> struct Type<RT_Thread> { static const Type_Id ID = THREAD_ID; };
template<> struct Type<Active> { static const Type_Id ID = ACTIVE_ID; };
template<> struct Type<Task> { static const Type_Id ID = TASK_ID; };

template<> struct Type<Address_Space> { static const Type_Id ID = ADDRESS_SPACE_ID; };
template<> struct Type<Segment> { static const Type_Id ID = SEGMENT_ID; };

template<> struct Type<Mutex> { static const Type_Id ID = MUTEX_ID; };
template<> struct Type<Semaphore> { static const Type_Id ID = SEMAPHORE_ID; };
template<> struct Type<Condition> { static const Type_Id ID = CONDITION_ID; };

template<> struct Type<Clock> { static const Type_Id ID = CLOCK_ID; };
template<> struct Type<Chronometer> { static const Type_Id ID = CHRONOMETER_ID; };
template<> struct Type<Alarm> { static const Type_Id ID = ALARM_ID; };
template<> struct Type<Delay> { static const Type_Id ID = ALARM_ID; };

template<> struct Type<Utility> { static const Type_Id ID = UTILITY_ID; };

// Type IDs for system components whose parameters are themselves components are defined where they are declared.

__END_SYS

#endif
